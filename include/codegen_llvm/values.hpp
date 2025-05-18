#ifndef CODEGEN_LLVM_VALUES_HPP
#define CODEGEN_LLVM_VALUES_HPP

#include "types.hpp"
#include <llvm/IR/Value.h>
#include <memory>

class CodeGenLLVM_Value
{
private:
    llvm::Value *llvmValue_;
    std::shared_ptr<CodeGenLLVM_Type> valueType_;

public:
    CodeGenLLVM_Value(llvm::Value *llvmValue, std::shared_ptr<CodeGenLLVM_Type> valueType)
        : llvmValue_(llvmValue), valueType_(std::move(valueType)) {}
    ~CodeGenLLVM_Value() = default;

    llvm::Value *getLLVMValue() const { return llvmValue_; }
    std::shared_ptr<CodeGenLLVM_Type> getValueType() const { return valueType_; }
};

class CodeGenLLVM_EValue
{
public:
    enum class ValueCategory
    {
        LValue,
        RValue
    };

    CodeGenLLVM_EValue(std::shared_ptr<CodeGenLLVM_Value> value, ValueCategory category)
        : value_(std::move(value)), category_(category) {}
    ~CodeGenLLVM_EValue() = default;

    bool isLValue() const { return category_ == ValueCategory::LValue; }
    bool isRValue() const { return category_ == ValueCategory::RValue; }

    std::shared_ptr<CodeGenLLVM_Value> asValue() const { return value_; }

private:
    std::shared_ptr<CodeGenLLVM_Value> value_;
    ValueCategory category_;
};

#endif // CODEGEN_LLVM_VALUES_HPP
