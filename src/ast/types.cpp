#include <ast/types.hpp>

std::string ASTTypeSpecifier::formatInternalType() const
{
    switch (type_)
    {
    case ASTInternalType::Int:
        return "int";
    case ASTInternalType::Int8:
        return "int8";
    case ASTInternalType::Int16:
        return "int16";
    case ASTInternalType::Int32:
        return "int32";
    case ASTInternalType::Int64:
        return "int64";
    case ASTInternalType::Int128:
        return "int128";
    case ASTInternalType::UInt:
        return "uint";
    case ASTInternalType::UInt8:
        return "uint8";
    case ASTInternalType::UInt16:
        return "uint16";
    case ASTInternalType::UInt32:
        return "uint32";
    case ASTInternalType::UInt64:
        return "uint64";
    case ASTInternalType::UInt128:
        return "uint128";
    case ASTInternalType::Void:
        return "void";
    case ASTInternalType::Char:
        return "char";
    case ASTInternalType::Byte:
        return "byte";
    case ASTInternalType::String:
        return "string";
    case ASTInternalType::Float32:
        return "float32";
    case ASTInternalType::Float64:
        return "float64";
    case ASTInternalType::Float128:
        return "float128";
    case ASTInternalType::Bool:
        return "bool";
    case ASTInternalType::Error:
        return "error";
    case ASTInternalType::Pointer:
        return "PointerOf";
    case ASTInternalType::Reference:
        return "Reference";
    case ASTInternalType::Identifier:
        return "Identifier";
    case ASTInternalType::Const:
        return "Const";
    default:
        return "Unknown Type";
    }
}
