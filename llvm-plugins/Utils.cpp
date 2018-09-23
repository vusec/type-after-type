/*
 * Utils.cpp
 *
 *  Created on: Oct 22, 2015
 *      Author: haller
 */

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetLowering.h>
#include <llvm/Target/TargetSubtargetInfo.h>
#include <llvm/Transforms/Utils/Local.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>

#include <Utils.h>
#include <algorithm>

using namespace llvm;

static void typeToStr(std::string &str, Type *type);

static void functionTypeToStr(std::string &str, FunctionType *ft) {
  char buffer[32];
  sprintf(buffer, "F%s%ux", ft->isVarArg() ? "V" : "", ft->getNumParams() + 1);
  str += buffer;

  typeToStr(str, ft->getReturnType());
  for (auto *type : ft->params()) {
    typeToStr(str, type);
  }
}

static void structTypeToStr(std::string &str, StructType *st) {
  char buffer[32];

  /* named struct types are identified by their name */
  if (st->hasName()) {
    std::string name = st->getName();
    sprintf(buffer, "S%zux", name.size());
    str += buffer;
    str += name;
    return;
  }

  /* anonymous struct types are identified by their element types */
  assert(!st->isOpaque());
  sprintf(buffer, "s%s%ux", st->isPacked() ? "p" : "", st->getNumElements());
  str += buffer;
  for (auto *type : st->elements()) {
    typeToStr(str, type);
  }
}

static void typeToStr(std::string &str, Type *type) {
  char buffer[32];

  /* some mangling to ensure types with the same names have the same layout and
   * can therefore share the same unsafe stack
   */

  ArrayType *at = dyn_cast<ArrayType>(type);
  if (at) {
    sprintf(buffer, "A%zux", at->getNumElements());
    str += buffer;
    typeToStr(str, at->getElementType());
    return;
  }

  FunctionType *ft = dyn_cast<FunctionType>(type);
  if (ft) {
    functionTypeToStr(str, ft);
    return;
  }

  IntegerType *it = dyn_cast<IntegerType>(type);
  if (it) {
    sprintf(buffer, "I%u", it->getBitWidth());
    str += buffer;
    return;
  }

  PointerType *pt = dyn_cast<PointerType>(type);
  if (pt) {
    str += "P";
    typeToStr(str, pt->getElementType());
    return;
  }

  StructType *st = dyn_cast<StructType>(type);
  if (st) {
    structTypeToStr(str, st);
    return;
  }

  VectorType *vt = dyn_cast<VectorType>(type);
  if (vt) {
    sprintf(buffer, "V%lux", (long) vt->getNumElements());
    str += buffer;
    typeToStr(str, vt->getElementType());
    return;
  }

  switch (type->getTypeID()) {
  case Type::VoidTyID: str += "v"; return;
  case Type::HalfTyID: str += "f16"; return;
  case Type::FloatTyID: str += "f32"; return;
  case Type::DoubleTyID: str += "f64"; return;
  case Type::X86_FP80TyID: str += "f80"; return;
  case Type::FP128TyID: str += "f128"; return;
  /* PPC_FP128TyID */
  /* LabelTyID */
  /* MetadataTyID */
  /* X86_MMXTyID */
  /* TokenTyID */
  default: break;
  }

  errs() << "typesafestack: unable to mangle type " << type->getTypeID() << "\n";
  abort();
}

llvm::Type *getElementType(llvm::Type *type) {
  PointerType *pt;
  SequentialType *st;

  if ((pt = dyn_cast<PointerType>(type))) return pt->getElementType();
  if ((st = dyn_cast<SequentialType>(type))) return st->getElementType();

  return nullptr;
}

std::string typeToStr(Type *type) {
  std::string str;
  typeToStr(str, type);
  return str;
}

unsigned long RoundUpToAlignment(unsigned long value, unsigned long align) {
  unsigned long remainder = value % align;
  if (remainder) {
    return value + align - remainder;
  } else {
    return value;
  }
}

