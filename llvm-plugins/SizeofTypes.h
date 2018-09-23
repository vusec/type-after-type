#ifndef SIZE_OF_TYPES_H
#define SIZE_OF_TYPES_H

#include "llvm/Pass.h"
#include "llvm/ADT/DenseMap.h"

using namespace llvm;

struct SizeofTypes : public ModulePass {
    static char ID;
    SizeofTypes() : ModulePass(ID) {}

    virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool runOnModule(Module &M);

    Type *getSizeofType(CallInst *CI);
    void setSizeofType(CallInst *CI, Type *Ty);

private:
    DenseMap<CallInst*, Type*> mallocTypes;
};

#endif  /* SIZE_OF_TYPES_H */
