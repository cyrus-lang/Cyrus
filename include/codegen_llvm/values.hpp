#ifndef CODEGEN_LLVM_VALUES_HPP
#define CODEGEN_LLVM_VALUES_HPP

#include "types.hpp"
#include <llvm/IR/Value.h>

class CodeGenLLVM_Value
{
private:
    llvm::Value *llvmValue_;
    CodeGenLLVM_Type valueType_;

public:
    CodeGenLLVM_Value(llvm::Value *llvmValue, CodeGenLLVM_Type valueType) : llvmValue_(llvmValue), valueType_(valueType) {}

    llvm::Value *getLLVMValue() const { return llvmValue_; }
    CodeGenLLVM_Type getValueType() const { return valueType_; }
};

#endif // CODEGEN_LLVM_VALUES_HPP
