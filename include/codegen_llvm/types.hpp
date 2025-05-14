#ifndef CODEGEN_LLVM_TYPES_HPP
#define CODEGEN_LLVM_TYPES_HPP

#include <variant>
#include "llvm/IR/Type.h"

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

    std::variant<llvm::Type *, CodeGenLLVM_Type *> payload_;

public:
    CodeGenLLVM_Type(llvm::Type *llvmType, TypeKind kind)
        : typeKind_(kind), payload_(llvmType) {}

    CodeGenLLVM_Type(CodeGenLLVM_Type *nested, TypeKind kind)
        : typeKind_(kind), payload_(nested) {}

    ~CodeGenLLVM_Type()
    {
        if ((typeKind_ == TypeKind::Pointer || typeKind_ == TypeKind::Reference) &&
            std::holds_alternative<CodeGenLLVM_Type *>(payload_))
        {
            delete std::get<CodeGenLLVM_Type *>(payload_);
        }
    }

    static CodeGenLLVM_Type *createPointerType(CodeGenLLVM_Type *pointee)
    {
        return new CodeGenLLVM_Type(pointee, TypeKind::Pointer);
    }

    static CodeGenLLVM_Type *createReferenceType(CodeGenLLVM_Type *pointee)
    {
        return new CodeGenLLVM_Type(pointee, TypeKind::Reference);
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
            auto nested = std::get<CodeGenLLVM_Type *>(payload_);
            auto pointeeLLVM = nested->getLLVMType();
            return llvm::PointerType::getUnqual(pointeeLLVM);
        }
    }

    CodeGenLLVM_Type *getNestedType() const
    {
        if (std::holds_alternative<CodeGenLLVM_Type *>(payload_))
            return std::get<CodeGenLLVM_Type *>(payload_);
        return nullptr;
    }
};

#endif // CODEGEN_LLVM_TYPES_HPP
