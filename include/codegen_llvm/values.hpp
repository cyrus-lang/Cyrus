#ifndef CODEGEN_LLVM_VALUES_HPP
#define CODEGEN_LLVM_VALUES_HPP

#include "types.hpp"
#include <llvm/IR/Value.h>

class CodeGenLLVM_Value
{
private:
    llvm::Value *llvmValue_;
    CodeGenLLVM_Type *valueType_;

public:
    CodeGenLLVM_Value(llvm::Value *llvmValue, CodeGenLLVM_Type *valueType) : llvmValue_(llvmValue), valueType_(valueType) {}
    ~CodeGenLLVM_Value()
    {
        delete valueType_;
    }

    llvm::Value *getLLVMValue() const { return llvmValue_; }
    CodeGenLLVM_Type *getValueType() const { return valueType_; }
};

class CodeGenLLVM_EValue
{
public:
    enum class ValueCategory
    {
        LValue,
        RValue
    };

    CodeGenLLVM_EValue(CodeGenLLVM_Value *value, ValueCategory category) : value_(value), category_(category) {}
    ~CodeGenLLVM_EValue()
    {
        delete value_;
    }

    bool isLValue() const { return category_ == ValueCategory::LValue; }
    bool isRValue() const { return category_ == ValueCategory::RValue; }

    CodeGenLLVM_Value *asValue() const { return value_; }

private:
    CodeGenLLVM_Value *value_;
    ValueCategory category_;
};

#endif // CODEGEN_LLVM_VALUES_HPP
