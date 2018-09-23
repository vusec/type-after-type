//===-- SafeStack.cpp - Safe Stack Insertion ------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass splits the stack into the safe stack (kept as-is for LLVM backend)
// and the unsafe stack (explicitly allocated and managed through the runtime
// support library).
//
// http://clang.llvm.org/docs/SafeStack.html
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Instrumentation.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <Utils.h>

#include <iostream>
#include <sstream>

#include "SizeofTypes.h"

using namespace llvm;

#define DEBUG_TYPE "typesafestack"

static std::string kUnsafeStackPtrCountVar = "__typesafestack_unsafe_stack_ptr_count";
static std::string kUnsafeStackPtrVar = "__typesafestack_unsafe_stack_ptrs";
static std::string kUnsafeStackPtrVarFinal = "__typesafestack_unsafe_stack_ptrs_final";
static std::string kUnsafeStackPtrVarTemp = "__typesafestack_unsafe_stack_ptrs_temp";

cl::opt<bool> OptOneStack ("typesafe-stack-onestack", cl::desc("Use a single unsafe stack, dropping type safety"), cl::init(false));
cl::opt<bool> OptKeepArrays ("typesafe-stack-keeparrays", cl::desc("Do not strip wrapper arrays in determining a type ID"), cl::init(false));

namespace llvm {

STATISTIC(NumFunctions, "Total number of functions");
STATISTIC(NumUnsafeStackFunctions, "Number of functions with unsafe stack");
STATISTIC(NumUnsafeStackRestorePointsFunctions,
          "Number of functions that use setjmp or exceptions");

STATISTIC(NumAllocas, "Total number of allocas");
STATISTIC(NumUnsafeStaticAllocas, "Number of unsafe static allocas");
STATISTIC(NumUnsafeDynamicAllocas, "Number of unsafe dynamic allocas");
STATISTIC(NumUnsafeStackRestorePoints, "Number of setjmps and landingpads");

} // namespace llvm

namespace {

/// Check whether a given alloca instruction (AI) should be put on the safe
/// stack or not. The function analyzes all uses of AI and checks whether it is
/// only accessed in a memory safe way (as decided statically).
bool IsSafeStackAlloca(const AllocaInst *AI) {
  // Go through all uses of this alloca and check whether all accesses to the
  // allocated object are statically known to be memory safe and, hence, the
  // object can be placed on the safe stack.

  SmallPtrSet<const Value *, 16> Visited;
  SmallVector<const Instruction *, 8> WorkList;
  WorkList.push_back(AI);

  // A DFS search through all uses of the alloca in bitcasts/PHI/GEPs/etc.
  while (!WorkList.empty()) {
    const Instruction *V = WorkList.pop_back_val();
    for (const Use &UI : V->uses()) {
      auto I = cast<const Instruction>(UI.getUser());
      assert(V == UI.get());

      switch (I->getOpcode()) {
      case Instruction::Load:
        // Loading from a pointer is safe.
        break;
      case Instruction::VAArg:
        // "va-arg" from a pointer is safe.
        break;
      case Instruction::Store:
        if (V == I->getOperand(0))
          // Stored the pointer - conservatively assume it may be unsafe.
          return false;
        // Storing to the pointee is safe.
        break;

      case Instruction::GetElementPtr:
        if (!cast<const GetElementPtrInst>(I)->hasAllConstantIndices())
          // GEP with non-constant indices can lead to memory errors.
          // This also applies to inbounds GEPs, as the inbounds attribute
          // represents an assumption that the address is in bounds, rather than
          // an assertion that it is.
          return false;

        // We assume that GEP on static alloca with constant indices is safe,
        // otherwise a compiler would detect it and warn during compilation.

        if (!isa<const ConstantInt>(AI->getArraySize()))
          // However, if the array size itself is not constant, the access
          // might still be unsafe at runtime.
          return false;

      /* fallthrough */

      case Instruction::BitCast:
      case Instruction::IntToPtr:
      case Instruction::PHI:
      case Instruction::PtrToInt:
      case Instruction::Select:
        // The object can be safe or not, depending on how the result of the
        // instruction is used.
        if (Visited.insert(I).second)
          WorkList.push_back(cast<const Instruction>(I));
        break;

      case Instruction::Call:
      case Instruction::Invoke: {
        // FIXME: add support for memset and memcpy intrinsics.
        ImmutableCallSite CS(I);

        // LLVM 'nocapture' attribute is only set for arguments whose address
        // is not stored, passed around, or used in any other non-trivial way.
        // We assume that passing a pointer to an object as a 'nocapture'
        // argument is safe.
        // FIXME: a more precise solution would require an interprocedural
        // analysis here, which would look at all uses of an argument inside
        // the function being called.
        ImmutableCallSite::arg_iterator B = CS.arg_begin(), E = CS.arg_end();
        for (ImmutableCallSite::arg_iterator A = B; A != E; ++A)
          if (A->get() == V && !CS.doesNotCapture(A - B))
            // The parameter is not marked 'nocapture' - unsafe.
            return false;
        continue;
      }

      default:
        // The object is unsafe if it is used in any other way.
        return false;
      }
    }
  }

  // All uses of the alloca are safe, we can place it on the safe stack.
  return true;
}

/// The TypeSafeStack pass splits the stack of each function into the
/// safe stack, which is only accessed through memory safe dereferences
/// (as determined statically), and per-type unsafe stacks, which contain all
/// local variables that are accessed in unsafe ways.
class TypeSafeStack : public FunctionPass {
  const TargetMachine *TM;
  const DataLayout *DL;

  PointerType *StackPtrTy;
  Type *IntPtrTy;
  Type *Int32Ty;
  Type *Int8Ty;

  GlobalVariable *stackPointerArray;
  std::map<std::string, size_t> typeIndexByTypeId;
  size_t typeIndexNext;

  /// Unsafe stack alignment. Each stack frame must ensure that the stack is
  /// aligned to this value. We need to re-align the unsafe stack if the
  /// alignment of any object on the stack exceeds this value.
  ///
  /// 16 seems like a reasonable upper bound on the alignment of objects that we
  /// might expect to appear on the stack on most common targets.
  enum { StackAlignment = 16 };

  /// \brief Build a constant representing a pointer to the unsafe stack
  /// pointer.
  GlobalVariable *createStackPtrArray(Module &M, std::string varName, size_t count);
  GlobalVariable *createStackPtrCount(Module &M, std::string varName, size_t count);
  size_t getTypeIndex(const std::string &typeId);
  Value *getOrCreateUnsafeStackPtr(IRBuilder<> &IRB, Function &F, std::string typeId);

  /// \brief Find all static allocas, dynamic allocas, return instructions and
  /// stack restore points (exception unwind blocks and setjmp calls) in the
  /// given function and append them to the respective vectors.
  void findInsts(Function &F, SmallVectorImpl<AllocaInst *> &StaticAllocas,
                 SmallVectorImpl<AllocaInst *> &DynamicAllocas,
                 SmallVectorImpl<ReturnInst *> &Returns,
                 SmallVectorImpl<Instruction *> &StackRestorePoints);

  /// \brief Allocate space for all static allocas in \p StaticAllocas,
  /// replace allocas with pointers into the unsafe stack and generate code to
  /// restore the stack pointer before all return instructions in \p Returns.
  ///
  /// \returns A pointer to the top of the unsafe stack after all unsafe static
  /// allocas are allocated.
  void moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                        MutableArrayRef<AllocaInst *> StaticAllocas,
                                        ArrayRef<ReturnInst *> Returns,
                                        std::set<std::string> &typeIds);
  Value *moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                        MutableArrayRef<AllocaInst *> StaticAllocas,
                                        ArrayRef<ReturnInst *> Returns,
                                        std::string typeId);

  /// \brief Generate code to restore the stack after all stack restore points
  /// in \p StackRestorePoints.
  ///
  /// \returns A local variable in which to maintain the dynamic top of the
  /// unsafe stack if needed.
  void
  createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                           ArrayRef<Instruction *> StackRestorePoints,
                           bool NeedDynamicTop,
                           std::set<std::string> &typeIds);
  AllocaInst *
  createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                           ArrayRef<Instruction *> StackRestorePoints,
                           Value *StaticTop, bool NeedDynamicTop,
                           std::string typeId);

  /// \brief Replace all allocas in \p DynamicAllocas with code to allocate
  /// space dynamically on the unsafe stack and store the dynamic unsafe stack
  /// top to \p DynamicTop if non-null.
  void moveDynamicAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                       MutableArrayRef<AllocaInst *> DynamicAllocas,
				       std::set<std::string> &typeIds);
  void moveDynamicAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                       AllocaInst *DynamicTop,
                                       MutableArrayRef<AllocaInst *> DynamicAllocas,
				       std::string type);

  void replaceUses(User *replaceWhat, User *replaceWith);

  struct statistics {
#	define STAT(name) long name;
#	include "TypeSafeStackStats.h"
#	undef STAT
  } stats;

  void printStatistics(const std::string &modname) {
	dbgs() << "TypeSafeStackStats:\tmodname";
#	define STAT(name) dbgs() << "\t" #name;
#	include "TypeSafeStackStats.h"
#	undef STAT
	dbgs() << "\n";
	dbgs() << "TypeSafeStackStats:\t" << modname;
#	define STAT(name) dbgs() << "\t" << stats.name;
#	include "TypeSafeStackStats.h"
#	undef STAT
	dbgs() << "\n";
  }

public:
  static char ID; // Pass identification, replacement for typeid.

  TypeSafeStack(const TargetMachine *TM)
      : FunctionPass(ID), TM(TM), DL(nullptr), typeIndexNext(0) {
    initializeSafeStackPass(*PassRegistry::getPassRegistry());
    memset(&stats, 0, sizeof(stats));
  }
  TypeSafeStack() : TypeSafeStack(nullptr) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addPreserved<SizeofTypes>();
    AU.addRequired<AAResultsWrapperPass>();
  }

  bool doInitialization(Module &M) override {
    DL = &M.getDataLayout();

    StackPtrTy = Type::getInt8PtrTy(M.getContext());
    IntPtrTy = DL->getIntPtrType(M.getContext());
    Int32Ty = Type::getInt32Ty(M.getContext());
    Int8Ty = Type::getInt8Ty(M.getContext());

    /* create a temporary stack pointer array as we do not know the size yet */
    stackPointerArray = createStackPtrArray(M, kUnsafeStackPtrVarTemp, 0);

    return false;
  }

  bool doFinalization(Module &M) override {
    /* replace temporary stack pointer array with a properly sized one */
    GlobalVariable *stackPointerArrayFinal = createStackPtrArray(M, kUnsafeStackPtrVarFinal, typeIndexNext);
    replaceUses(stackPointerArray, stackPointerArrayFinal);
    stackPointerArray->eraseFromParent();
    stackPointerArray = nullptr;

    /* replace static library stack pointer array with the newly allocated one */
    GlobalVariable *stackPointerArrayStatic = dyn_cast_or_null<GlobalVariable>(M.getNamedValue(kUnsafeStackPtrVar));
    if (stackPointerArrayStatic) {
      replaceUses(stackPointerArrayStatic, stackPointerArrayFinal);
      stackPointerArrayStatic->eraseFromParent();
    } else {
      errs() << "TypeSafeStack: error: no reference to variable " << kUnsafeStackPtrVar << " found, is the static library linked in?\n";
    }

    /* provide stack pointer count to static library */
    createStackPtrCount(M, kUnsafeStackPtrCountVar, typeIndexNext);

    printStatistics(M.getName());
    return false;
  }

  bool runOnFunction(Function &F) override;
}; // class TypeSafeStack

GlobalVariable *TypeSafeStack::createStackPtrArray(
  Module &M,
  std::string varName,
  size_t count) {

  /* we need an array of count stack pointers */
  ArrayType *type = ArrayType::get(StackPtrTy, count);

  /* initialize every stack pointer to NULL */
  Constant *initElement = ConstantPointerNull::get(StackPtrTy);
  std::vector<Constant *> initElements;
  for (size_t i = 0; i < count; i++) initElements.push_back(initElement);
  Constant *init = ConstantArray::get(type, initElements);

  /* create the global var */
  GlobalVariable *gv = new GlobalVariable(
      /*Module=*/M,
      /*Type=*/type,
      /*isConstant=*/false,
      /*Linkage=*/GlobalValue::ExternalLinkage,
      /*Initializer=*/init,
      /*Name=*/varName,
      /*InsertBefore=*/nullptr,
      /*ThreadLocalMode=*/GlobalValue::InitialExecTLSModel);
  return gv;
}

GlobalVariable *TypeSafeStack::createStackPtrCount(
  Module &M,
  std::string varName,
  size_t count) {

  IntegerType *type = IntegerType::get(M.getContext(), 64);
  Constant *init = ConstantInt::get(type, count);
  GlobalVariable *gv = dyn_cast_or_null<GlobalVariable>(M.getNamedValue(varName));
  if (!gv) {
    errs() << "TypeSafeStack: error: no reference to variable " << varName << " found, is the static library linked in?\n";
    return nullptr;
  }
  gv->setInitializer(init);
  gv->setConstant(true);

  return gv;
}

static bool needsDebugCall(Function &F) {
  return false;
}

static Value *getConstantString(IRBuilder<> &IRB, Module *M, const char *str) {
  IntegerType* elementType = IntegerType::get(M->getContext(), 8);
  std::vector<Constant *> data;
  char c;
  do {
    c = *(str++);
    data.push_back(ConstantInt::get(elementType, c));
  } while (c);
  ArrayType *type = ArrayType::get(elementType, data.size());
  Constant *array = ConstantArray::get(type, data);
  GlobalVariable *gv = new GlobalVariable(*M, type, true,
    GlobalVariable::PrivateLinkage, array);
  std::vector<Value *> arrayIndex;
  IntegerType *arrayIndexType = IntegerType::get(M->getContext(), 64);
  arrayIndex.push_back(ConstantInt::get(arrayIndexType, 0));
  arrayIndex.push_back(ConstantInt::get(arrayIndexType, 0));
  return IRB.CreateGEP(gv, arrayIndex);
}

static void debugCall(IRBuilder<> &IRB, Function &F, std::string typeId,
  Value *UnsafeStackPtr, int callType) {

  dbgs() << "TypeSafeStack F=" << F.getName() << " typeId=" << typeId << " callType=" << callType << "\n";
  if (!needsDebugCall(F)) return;

  Module *M = F.getParent();
  Type *voidType = Type::getVoidTy(M->getContext());
  PointerType *stringType = PointerType::get(IntegerType::get(M->getContext(), 8), 0);
  PointerType *voidPtrPtrType = PointerType::get(stringType, 0);
  IntegerType *callTypeType = IntegerType::get(M->getContext(), 32);

  Constant *callee = M->getOrInsertFunction(
    "__typesafestack_debug", voidType,
    stringType, stringType, voidPtrPtrType, callTypeType, NULL);

  std::string funcname = F.getName();
  std::vector<Value *> args;
  args.push_back(getConstantString(IRB, M, funcname.c_str()));
  args.push_back(getConstantString(IRB, M, typeId.c_str()));
  args.push_back(UnsafeStackPtr);
  args.push_back(ConstantInt::get(callTypeType, callType));
  IRB.CreateCall(callee, args);
}

size_t TypeSafeStack::getTypeIndex(const std::string &typeId) {
  /* TODO need to lock access to typeIndexByTypeId in case LLVM causes concurrent access? */

  auto typeIndexIt = typeIndexByTypeId.find(typeId);
  size_t typeIndex;
  if (typeIndexIt == typeIndexByTypeId.end()) {
    typeIndex = typeIndexNext++;
    dbgs() << "TypeSafeStack getTypeIndex typeId=" << typeId << " typeIndex=" << typeIndex << "\n";
    typeIndexByTypeId[typeId] = typeIndex;
  } else {
    typeIndex = typeIndexIt->second;
  }

  return typeIndex;  
}

Value *TypeSafeStack::getOrCreateUnsafeStackPtr(IRBuilder<> &IRB, Function &F, std::string typeId) {
  size_t typeIndex = getTypeIndex(typeId);
  IntegerType *gepIndexType = IntegerType::get(IRB.getContext(), 32);
  std::vector<Value *> gepIndices;
  gepIndices.push_back(ConstantInt::get(gepIndexType, 0));
  gepIndices.push_back(ConstantInt::get(gepIndexType, typeIndex));
  Value *gep = IRB.CreateGEP(stackPointerArray, gepIndices);
  return gep;
}

void TypeSafeStack::findInsts(Function &F,
                          SmallVectorImpl<AllocaInst *> &StaticAllocas,
                          SmallVectorImpl<AllocaInst *> &DynamicAllocas,
                          SmallVectorImpl<ReturnInst *> &Returns,
                          SmallVectorImpl<Instruction *> &StackRestorePoints) {
  for (Instruction &I : instructions(&F)) {
    if (auto AI = dyn_cast<AllocaInst>(&I)) {
      ++NumAllocas;

      if (IsSafeStackAlloca(AI))
        continue;

      if (AI->isStaticAlloca()) {
        ++NumUnsafeStaticAllocas;
        StaticAllocas.push_back(AI);
      } else {
        ++NumUnsafeDynamicAllocas;
        DynamicAllocas.push_back(AI);
      }
    } else if (auto RI = dyn_cast<ReturnInst>(&I)) {
      Returns.push_back(RI);
    } else if (auto CI = dyn_cast<CallInst>(&I)) {
      // setjmps require stack restore.
      if (CI->getCalledFunction() && CI->canReturnTwice())
        StackRestorePoints.push_back(CI);
    } else if (auto LP = dyn_cast<LandingPadInst>(&I)) {
      // Exception landing pads require stack restore.
      StackRestorePoints.push_back(LP);
    } else if (auto II = dyn_cast<IntrinsicInst>(&I)) {
      if (II->getIntrinsicID() == Intrinsic::gcroot)
        llvm::report_fatal_error(
            "gcroot intrinsic not compatible with safestack attribute");
    }
  }
  stats.count_static += StaticAllocas.size();
  stats.count_dynamic += DynamicAllocas.size();
  stats.count_return += Returns.size();
  stats.count_restore += StackRestorePoints.size();
}

static Type *typeStripArrays(Type *type) {
  ArrayType *at;

  while ((at = dyn_cast<ArrayType>(type))) {
    type = at->getElementType();
  }

  return type;
}

static std::string getTypeId(AllocaInst *ai) {
  if (!ai) return "";
  if (OptOneStack) return "onestack";
  Type *type = ai->getAllocatedType();
  if (!OptKeepArrays) type = typeStripArrays(type);
  return typeToStr(type);
}

static void getAllocaTypes(std::set<std::string> &typeIds, ArrayRef<AllocaInst *> allocas) {
  for (AllocaInst *ai : allocas) {
    std::string typeId = getTypeId(ai);
    typeIds.insert(typeId);
  }
}

static size_t countAllocaForTypeId(ArrayRef<AllocaInst *> allocas, std::string typeId) {
  size_t count = 0;
  for (AllocaInst *ai : allocas) {
    if (getTypeId(ai) == typeId) count++;
  }
  return count;
}

void
TypeSafeStack::createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                                    ArrayRef<Instruction *> StackRestorePoints,
                                    bool NeedDynamicTop,
                                    std::set<std::string> &typeIds) {
  for (auto &typeId : typeIds) {
    createStackRestorePoints(IRB, F, StackRestorePoints, nullptr, NeedDynamicTop, typeId);
  }
}
}

AllocaInst *
TypeSafeStack::createStackRestorePoints(IRBuilder<> &IRB, Function &F,
                                    ArrayRef<Instruction *> StackRestorePoints,
                                    Value *StaticTop, bool NeedDynamicTop,
                                    std::string typeId) {
  if (StackRestorePoints.empty())
    return nullptr;

  // We need the current value of the shadow stack pointer to restore
  // after longjmp or exception catching.

  // FIXME: On some platforms this could be handled by the longjmp/exception
  // runtime itself.

  AllocaInst *DynamicTop = nullptr;
  if (NeedDynamicTop)
    // If we also have dynamic alloca's, the stack pointer value changes
    // throughout the function. For now we store it in an alloca.
    DynamicTop = IRB.CreateAlloca(StackPtrTy, /*ArraySize=*/nullptr,
                                  "unsafe_stack_dynamic_ptr_" + typeId);

  Value *UnsafeStackPtr = getOrCreateUnsafeStackPtr(IRB, F, typeId);
  debugCall(IRB, F, typeId, UnsafeStackPtr, 12);
  if (!StaticTop)
    // We need the original unsafe stack pointer value, even if there are
    // no unsafe static allocas.
    StaticTop = IRB.CreateLoad(UnsafeStackPtr, false, "unsafe_stack_ptr_" + typeId);

  if (NeedDynamicTop)
    IRB.CreateStore(StaticTop, DynamicTop);

  IRBuilder<>::InsertPoint IP = IRB.saveIP();

  // Restore current stack pointer after longjmp/exception catch.
  for (Instruction *I : StackRestorePoints) {
    ++NumUnsafeStackRestorePoints;

    IRB.SetInsertPoint(cast<Instruction>(I->getNextNode()));
    Value *CurrentTop = DynamicTop ? IRB.CreateLoad(DynamicTop) : StaticTop;
    debugCall(IRB, F, typeId, UnsafeStackPtr, 13);
    IRB.CreateStore(CurrentTop, UnsafeStackPtr);
    debugCall(IRB, F, typeId, UnsafeStackPtr, 14);
  }

  IRB.restoreIP(IP);

  return DynamicTop;
}

void
TypeSafeStack::moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                          MutableArrayRef<AllocaInst *> StaticAllocas,
                                          ArrayRef<ReturnInst *> Returns,
					  std::set<std::string> &typeIds) {
  for (auto &typeId : typeIds) {
    moveStaticAllocasToUnsafeStack(IRB, F, StaticAllocas, Returns, typeId);
  }
}

Value *
TypeSafeStack::moveStaticAllocasToUnsafeStack(IRBuilder<> &IRB, Function &F,
                                          MutableArrayRef<AllocaInst *> StaticAllocas,
                                          ArrayRef<ReturnInst *> Returns,
					  std::string typeId) {

  DIBuilder DIB(*F.getParent());

  // We explicitly compute and set the unsafe stack layout for all unsafe
  // static alloca instructions. We save the unsafe "base pointer" in the
  // prologue into a local variable and restore it in the epilogue.

  // Load the current stack pointer (we'll also use it as a base pointer).
  // FIXME: use a dedicated register for it ?
  Value *UnsafeStackPtr = getOrCreateUnsafeStackPtr(IRB, F, typeId);
  debugCall(IRB, F, typeId, UnsafeStackPtr, 0);
  Instruction *BasePointer =
      IRB.CreateLoad(UnsafeStackPtr, false, "unsafe_stack_ptr_" + typeId);
  assert(BasePointer->getType() == StackPtrTy);

  for (ReturnInst *RI : Returns) {
    IRB.SetInsertPoint(RI);
    debugCall(IRB, F, typeId, UnsafeStackPtr, 1);
    IRB.CreateStore(BasePointer, UnsafeStackPtr);
    debugCall(IRB, F, typeId, UnsafeStackPtr, 2);
  }

  // Compute maximum alignment among static objects on the unsafe stack.
  unsigned MaxAlignment = 0;
  for (AllocaInst *AI : StaticAllocas) {
    if (getTypeId(AI) != typeId) continue;
    Type *Ty = AI->getAllocatedType();
    unsigned Align =
        std::max((unsigned)DL->getPrefTypeAlignment(Ty), AI->getAlignment());
    if (Align > MaxAlignment)
      MaxAlignment = Align;
  }

  if (MaxAlignment > StackAlignment) {
    // Re-align the base pointer according to the max requested alignment.
    assert(isPowerOf2_32(MaxAlignment));
    IRB.SetInsertPoint(cast<Instruction>(BasePointer->getNextNode()));
    BasePointer = cast<Instruction>(IRB.CreateIntToPtr(
        IRB.CreateAnd(IRB.CreatePtrToInt(BasePointer, IntPtrTy),
                      ConstantInt::get(IntPtrTy, ~uint64_t(MaxAlignment - 1))),
        StackPtrTy));
  }

  // Allocate space for every unsafe static AllocaInst on the unsafe stack.
  int64_t StaticOffset = 0; // Current stack top.
  for (size_t i = 0; i < StaticAllocas.size(); i++) {
    AllocaInst *AI = StaticAllocas[i];
    if (getTypeId(AI) != typeId) continue;
    IRB.SetInsertPoint(AI);

    auto CArraySize = cast<ConstantInt>(AI->getArraySize());
    Type *Ty = AI->getAllocatedType();

    uint64_t Size = DL->getTypeAllocSize(Ty) * CArraySize->getZExtValue();
    if (Size == 0)
      Size = 1; // Don't create zero-sized stack objects.

    // Ensure the object is properly aligned.
    unsigned Align =
        std::max((unsigned)DL->getPrefTypeAlignment(Ty), AI->getAlignment());

    // Add alignment.
    // NOTE: we ensure that BasePointer itself is aligned to >= Align.
    StaticOffset += Size;
    StaticOffset = RoundUpToAlignment(StaticOffset, Align);

    Value *Off = IRB.CreateGEP(BasePointer, // BasePointer is i8*
                               ConstantInt::get(Int32Ty, -StaticOffset));
    Value *NewAI = IRB.CreateBitCast(Off, AI->getType(), AI->getName());
    if (AI->hasName() && isa<Instruction>(NewAI))
      cast<Instruction>(NewAI)->takeName(AI);

    // Replace alloc with the new location.
    replaceDbgDeclareForAlloca(AI, BasePointer, DIB, /*Deref=*/true, -StaticOffset);
    AI->replaceAllUsesWith(NewAI);
    AI->eraseFromParent();
    StaticAllocas[i] = nullptr;
  }

  // Re-align BasePointer so that our callees would see it aligned as
  // expected.
  // FIXME: no need to update BasePointer in leaf functions.
  StaticOffset = RoundUpToAlignment(StaticOffset, StackAlignment);

  // Update shadow stack pointer in the function epilogue.
  IRB.SetInsertPoint(cast<Instruction>(BasePointer->getNextNode()));

  Value *StaticTop =
      IRB.CreateGEP(BasePointer, ConstantInt::get(Int32Ty, -StaticOffset),
                    "unsafe_stack_static_top_" + typeId);
  debugCall(IRB, F, typeId, UnsafeStackPtr, 3);
  IRB.CreateStore(StaticTop, UnsafeStackPtr);
  debugCall(IRB, F, typeId, UnsafeStackPtr, 4);

  return StaticTop;
}

void TypeSafeStack::moveDynamicAllocasToUnsafeStack(
    IRBuilder<> &IRB, Function &F,
    MutableArrayRef<AllocaInst *> DynamicAllocas,
    std::set<std::string> &typeIds) {
  for (auto &typeId : typeIds) {
    moveDynamicAllocasToUnsafeStack(IRB, F, nullptr, DynamicAllocas, typeId);
  }
}

void TypeSafeStack::moveDynamicAllocasToUnsafeStack(
    IRBuilder<> &IRB, Function &F, AllocaInst *DynamicTop,
    MutableArrayRef<AllocaInst *> DynamicAllocas,
    std::string typeId) {
  if (countAllocaForTypeId(DynamicAllocas, typeId) == 0)
    return;

  DIBuilder DIB(*F.getParent());

  Value *UnsafeStackPtr = getOrCreateUnsafeStackPtr(IRB, F, typeId);
  debugCall(IRB, F, typeId, UnsafeStackPtr, 5);
  for (size_t i = 0; i < DynamicAllocas.size(); i++) {
    AllocaInst *AI = DynamicAllocas[i];
    if (getTypeId(AI) != typeId) continue;

    IRBuilder<> IRB(AI);

    // Compute the new SP value (after AI).
    Value *ArraySize = AI->getArraySize();
    if (ArraySize->getType() != IntPtrTy)
      ArraySize = IRB.CreateIntCast(ArraySize, IntPtrTy, false);

    Type *Ty = AI->getAllocatedType();
    uint64_t TySize = DL->getTypeAllocSize(Ty);
    Value *Size = IRB.CreateMul(ArraySize, ConstantInt::get(IntPtrTy, TySize));

    Value *SP = IRB.CreatePtrToInt(IRB.CreateLoad(UnsafeStackPtr), IntPtrTy);
    SP = IRB.CreateSub(SP, Size);

    // Align the SP value to satisfy the AllocaInst, type and stack alignments.
    unsigned Align = std::max(
        std::max((unsigned)DL->getPrefTypeAlignment(Ty), AI->getAlignment()),
        (unsigned)StackAlignment);

    assert(isPowerOf2_32(Align));
    Value *SPAligned = IRB.CreateAnd(SP, ConstantInt::get(IntPtrTy, ~uint64_t(Align - 1)));
    Value *NewTop = IRB.CreateIntToPtr(SPAligned, StackPtrTy);

    // Save the stack pointer.
    debugCall(IRB, F, typeId, UnsafeStackPtr, 6);
    IRB.CreateStore(NewTop, UnsafeStackPtr);
    debugCall(IRB, F, typeId, UnsafeStackPtr, 7);
    if (DynamicTop)
      IRB.CreateStore(NewTop, DynamicTop);

    Value *NewAI = IRB.CreateIntToPtr(SPAligned, AI->getType());
    if (AI->hasName() && isa<Instruction>(NewAI))
      NewAI->takeName(AI);

    replaceDbgDeclareForAlloca(AI, NewAI, DIB, /*Deref=*/true);
    AI->replaceAllUsesWith(NewAI);
    AI->eraseFromParent();
    DynamicAllocas[i] = nullptr;
  }

  if (!DynamicAllocas.empty()) {
    // Now go through the instructions again, replacing stacksave/stackrestore.
    for (inst_iterator It = inst_begin(&F), Ie = inst_end(&F); It != Ie;) {
      Instruction *I = &*(It++);
      auto II = dyn_cast<IntrinsicInst>(I);
      if (!II)
        continue;

      if (II->getIntrinsicID() == Intrinsic::stacksave) {
        IRBuilder<> IRB(II);
        debugCall(IRB, F, typeId, UnsafeStackPtr, 8);
        Instruction *LI = IRB.CreateLoad(UnsafeStackPtr);
        LI->takeName(II);
        II->replaceAllUsesWith(LI);
        II->eraseFromParent();
      } else if (II->getIntrinsicID() == Intrinsic::stackrestore) {
        IRBuilder<> IRB(II);
        debugCall(IRB, F, typeId, UnsafeStackPtr, 9);
        Instruction *SI = IRB.CreateStore(II->getArgOperand(0), UnsafeStackPtr);
        debugCall(IRB, F, typeId, UnsafeStackPtr, 10);
        SI->takeName(II);
        assert(II->use_empty());
        II->eraseFromParent();
      }
    }
  }
}

bool ignoreFunction(Function &F) {
  if (F.getName() == "__typesafestack_init") return true;
  if (F.getName() == "unsafe_stack_alloc") return true;
  if (F.getName() == "unsafe_stack_setup") return true;
  return false;
}

static void replaceUsesInConstGEP(User *replaceWhat, User *replaceWith, ConstantExpr *ceOld) {
  if (ceOld->getOperand(0) != replaceWhat) {
    errs() << "TypeSafeStack: error: unexpected use of temporary stack pointer array in constant GEP index: ";
    ceOld->dump();
    return;
  }

  std::vector<Constant *> gepIndices;
  for (unsigned i = 1; i < ceOld->getNumOperands(); i++) {
    gepIndices.push_back(cast<Constant>(ceOld->getOperand(i)));
  }
  Constant *replaceWithConst = cast<Constant>(replaceWith);
  Constant *ceNew = ConstantExpr::getInBoundsGetElementPtr(nullptr, replaceWithConst, gepIndices);
  ceOld->replaceAllUsesWith(ceNew);
}

static void replaceUsesInConstBitcast(User *replaceWith, ConstantExpr *ceOld) {
  Constant *replaceWithConst = cast<Constant>(replaceWith);
  Constant *ceNew = ConstantExpr::getBitCast(replaceWithConst, ceOld->getType());
  ceOld->replaceAllUsesWith(ceNew);
}

static void replaceUsesInConst(User *replaceWhat, User *replaceWith, ConstantExpr *ceOld) {
  if (ceOld->getOpcode() == Instruction::GetElementPtr) {
    replaceUsesInConstGEP(replaceWhat, replaceWith, ceOld);
    return;
  }

  if (ceOld->getOpcode() == Instruction::BitCast) {
    replaceUsesInConstBitcast(replaceWith, ceOld);
    return;
  }

  errs() << "TypeSafeStack: error: unexpected constant using temporary stack pointer array: ";
  ceOld->dump();
}

static void replaceUsesInGEP(User *replaceWhat, User *replaceWith, GetElementPtrInst *gepOld) {
  if (gepOld->getPointerOperand() != replaceWhat) {
    errs() << "TypeSafeStack: error: unexpected use of temporary stack pointer array in GEP index: ";
    gepOld->dump();
    return;
  }

  std::vector<Value *> gepIndices;
  for (auto gepIndex = gepOld->idx_begin(); gepIndex != gepOld->idx_end(); ++gepIndex) {
    gepIndices.push_back(*gepIndex);
  }

  std::vector<Use *> uses;
  for (auto &use : gepOld->uses()) uses.push_back(&use);
  for (auto *use : uses) {
    User *userUser = use->getUser();
    Instruction *insertBefore = dyn_cast<Instruction>(userUser);
    if (!insertBefore) {
      errs() << "TypeSafeStack: error: temporary stack pointer array GEP used by non-instruction: ";
      userUser->dump();
      continue;
    }
    GetElementPtrInst *gepNew = GetElementPtrInst::CreateInBounds(replaceWith, gepIndices, "", insertBefore);
    use->set(gepNew);
  }
  gepOld->eraseFromParent();
}

void TypeSafeStack::replaceUses(User *replaceWhat, User *replaceWith) {
  /* replaceAllUsesWith does not work in this case because the type of an
   * array changes when its size changes
   */
  std::vector<User *> users;
  for (auto user : replaceWhat->users()) users.push_back(user);
  for (auto user : users) {
    ConstantExpr *ceOld = dyn_cast<ConstantExpr>(user);
    if (ceOld) {
      replaceUsesInConst(replaceWhat, replaceWith, ceOld);
      continue;
    }

    GetElementPtrInst *gepOld = dyn_cast<GetElementPtrInst>(user);
    if (gepOld) {
      replaceUsesInGEP(replaceWhat, replaceWith, gepOld);
      continue;
    }

    errs() << "TypeSafeStack: error: unexpected instruction using temporary stack pointer array: ";
    user->dump();
  }
}

bool TypeSafeStack::runOnFunction(Function &F) {
  DEBUG(dbgs() << "[TypeSafeStack] Function: " << F.getName() << "\n");
  stats.func++;

//  if (!F.hasFnAttribute(Attribute::SafeStack)) {
  if (ignoreFunction(F)) {
    DEBUG(dbgs() << "[TypeSafeStack]     typesafestack is not requested"
                    " for this function\n");
    return false;
  }

  if (F.isDeclaration()) {
    DEBUG(dbgs() << "[TypeSafeStack]     function definition"
                    " is not available\n");
    return false;
  }

  auto AA = &getAnalysis<AAResultsWrapperPass>().getAAResults();

  {
    // Make sure the regular stack protector won't run on this function
    // (safestack attribute takes precedence).
    AttrBuilder B;
    B.addAttribute(Attribute::StackProtect)
        .addAttribute(Attribute::StackProtectReq)
        .addAttribute(Attribute::StackProtectStrong);
    F.removeAttributes(
        AttributeSet::FunctionIndex,
        AttributeSet::get(F.getContext(), AttributeSet::FunctionIndex, B));
  }

  if (AA->onlyReadsMemory(&F)) {
    // XXX: we don't protect against information leak attacks for now.
    DEBUG(dbgs() << "[TypeSafeStack]     function only reads memory\n");
    stats.func_onlyreads++;
    return false;
  }

  ++NumFunctions;

  SmallVector<AllocaInst *, 16> StaticAllocas;
  SmallVector<AllocaInst *, 4> DynamicAllocas;
  SmallVector<ReturnInst *, 4> Returns;

  // Collect all points where stack gets unwound and needs to be restored
  // This is only necessary because the runtime (setjmp and unwind code) is
  // not aware of the unsafe stack and won't unwind/restore it prorerly.
  // To work around this problem without changing the runtime, we insert
  // instrumentation to restore the unsafe stack pointer when necessary.
  SmallVector<Instruction *, 4> StackRestorePoints;

  // Find all static and dynamic alloca instructions that must be moved to the
  // unsafe stack, all return instructions and stack restore points.
  findInsts(F, StaticAllocas, DynamicAllocas, Returns, StackRestorePoints);

  if (StaticAllocas.empty() && DynamicAllocas.empty() &&
      StackRestorePoints.empty()) {
    stats.func_nothing++;
    return false; // Nothing to do in this function.
  }

  if (!StaticAllocas.empty() || !DynamicAllocas.empty())
    ++NumUnsafeStackFunctions; // This function has the unsafe stack.

  if (!StackRestorePoints.empty())
    ++NumUnsafeStackRestorePointsFunctions;

  std::set<std::string> typeIds;
  getAllocaTypes(typeIds, StaticAllocas);
  getAllocaTypes(typeIds, DynamicAllocas);

  IRBuilder<> IRB(&F.front(), F.begin()->getFirstInsertionPt());
  // The top of the unsafe stack after all unsafe static allocas are allocated.
  moveStaticAllocasToUnsafeStack(IRB, F, StaticAllocas, Returns, typeIds);

  // Safe stack object that stores the current unsafe stack top. It is updated
  // as unsafe dynamic (non-constant-sized) allocas are allocated and freed.
  // This is only needed if we need to restore stack pointer after longjmp
  // or exceptions, and we have dynamic allocations.
  // FIXME: a better alternative might be to store the unsafe stack pointer
  // before setjmp / invoke instructions.

  createStackRestorePoints(
      IRB, F, StackRestorePoints, !DynamicAllocas.empty(), typeIds);

  // Handle dynamic allocas.
  moveDynamicAllocasToUnsafeStack(IRB, F,
                                  DynamicAllocas, typeIds);

  DEBUG(dbgs() << "[TypeSafeStack]     typesafestack applied\n");
  return true;
}

char TypeSafeStack::ID = 0;
static RegisterPass<TypeSafeStack> X("typesafe-stack",
                         "TypeSafe Stack instrumentation pass", false, false);

