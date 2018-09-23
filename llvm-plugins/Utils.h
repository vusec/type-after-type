#ifndef UTILS_H
#define UTILS_H

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/ScalarEvolutionExpressions.h>

#include <map>
#include <set>
#include <utility>

template<class T>
class TodoList {

private:
	llvm::SmallPtrSet<const T *, 16> seen;
	llvm::SmallVector<T *, 8> todo;

public:
	void add(T *obj) {
		if (!seen.count(obj)) {
			todo.push_back(obj);
			seen.insert(obj);
		}
	}
	
	T *get(void) {
		return todo.empty() ? nullptr : todo.pop_back_val();
	}
	
};

llvm::Type *getElementType(llvm::Type *type);

std::string typeToStr(llvm::Type *type);

unsigned long RoundUpToAlignment(unsigned long value, unsigned long align);

#endif /* !UTILS_H */
