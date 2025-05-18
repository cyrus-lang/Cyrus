#ifndef CODEGEN_LLVM_TYPES_HPP
#define CODEGEN_LLVM_TYPES_HPP

#include <memory>
#include <variant>
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"

class CodeGenLLVM_Type
{
public:
    enum class TypeKind
    {
        Int8,
        Int16,
        Int32,
        Int64,
        Int128,
        UInt8,
        UInt16,
        UInt32,
        UInt64,
        UInt128,
        Int,
        UInt,
        Float32,
        Float64,
        Float128,
        Char,
        Byte,
        Bool,
        Void,
        String,
        Struct,
        Function,
        Reference,
        Pointer,
        Error
    };

private:
    TypeKind typeKind_;
    bool isConst_ = false;

    std::variant<llvm::Type *, std::shared_ptr<CodeGenLLVM_Type>> payload_;

public:
    CodeGenLLVM_Type(llvm::Type *llvmType, TypeKind kind)
        : typeKind_(kind), payload_(llvmType) {}

    CodeGenLLVM_Type(std::shared_ptr<CodeGenLLVM_Type> nested, TypeKind kind)
        : typeKind_(kind), payload_(std::move(nested)) {}

    static std::shared_ptr<CodeGenLLVM_Type> createPointerType(std::shared_ptr<CodeGenLLVM_Type> pointee)
    {
        return std::make_shared<CodeGenLLVM_Type>(std::move(pointee), TypeKind::Pointer);
    }

    static std::shared_ptr<CodeGenLLVM_Type> createReferenceType(std::shared_ptr<CodeGenLLVM_Type> pointee)
    {
        return std::make_shared<CodeGenLLVM_Type>(std::move(pointee), TypeKind::Reference);
    }

    TypeKind getKind() const { return typeKind_; }
    bool isConst() const { return isConst_; }
    void setConst() { isConst_ = true; }

    llvm::Type *getLLVMType() const
    {
        if (std::holds_alternative<llvm::Type *>(payload_))
        {
            return std::get<llvm::Type *>(payload_);
        }
        else if (typeKind_ == TypeKind::Pointer || typeKind_ == TypeKind::Reference)
        {
            auto nested = std::get<std::shared_ptr<CodeGenLLVM_Type>>(payload_);
            auto pointeeLLVM = nested->getLLVMType();
            return llvm::PointerType::getUnqual(pointeeLLVM);
        }
        return nullptr;
    }

    std::shared_ptr<CodeGenLLVM_Type> getNestedType() const
    {
        if (std::holds_alternative<std::shared_ptr<CodeGenLLVM_Type>>(payload_))
            return std::get<std::shared_ptr<CodeGenLLVM_Type>>(payload_);
        return nullptr;
    }

    // Clone method
    std::shared_ptr<CodeGenLLVM_Type> clone() const
    {
        if (std::holds_alternative<llvm::Type *>(payload_))
        {
            auto copy = std::make_shared<CodeGenLLVM_Type>(std::get<llvm::Type *>(payload_), typeKind_);
            if (isConst_)
                copy->setConst();
            return copy;
        }
        else if (std::holds_alternative<std::shared_ptr<CodeGenLLVM_Type>>(payload_))
        {
            auto nestedClone = std::get<std::shared_ptr<CodeGenLLVM_Type>>(payload_)->clone();
            auto copy = std::make_shared<CodeGenLLVM_Type>(nestedClone, typeKind_);
            if (isConst_)
                copy->setConst();
            return copy;
        }
        return nullptr;
    }
};

#endif // CODEGEN_LLVM_TYPES_HPP