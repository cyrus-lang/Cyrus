#include <memory>
#include "codegen_llvm/values.hpp"
#include "codegen_llvm/compiler.hpp"

llvm::Value *CodeGenLLVM_Module::createZeroInitializedValue(std::shared_ptr<CodeGenLLVM_Type> type)
{
    using TypeKind = CodeGenLLVM_Type::TypeKind;

    switch (type->getKind())
    {
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::Int128:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::UInt128:
    case TypeKind::Int:
    case TypeKind::UInt:
    case TypeKind::Char:
    case TypeKind::Byte:
    case TypeKind::Bool:
        return llvm::ConstantInt::get(type->getLLVMType(), 0);

    case TypeKind::Float32:
    case TypeKind::Float64:
    case TypeKind::Float128:
        return llvm::ConstantFP::get(type->getLLVMType(), 0.0);

    case TypeKind::Pointer:
    case TypeKind::String: // String is treated as a pointer type
        return llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(type->getLLVMType()));

    case TypeKind::Struct:
    case TypeKind::Function:
        return llvm::Constant::getNullValue(type->getLLVMType());

    case TypeKind::Void:
        return nullptr;

    case TypeKind::Reference:
        std::cerr << "(Error) Reference cannot be zero-initialized." << std::endl;
        return nullptr;

    case TypeKind::Error:
        std::cerr << "Error type cannot be zero-initialized" << std::endl;
        return nullptr;

    default:
        std::cerr << "Unhandled type kind in createZeroInitializedValue." << std::endl;
        return nullptr;
    }
}
