#ifndef AST_TYPES_HPP
#define AST_TYPES_HPP

#include "node.hpp"

class ASTTypeSpecifier : public ASTNode
{
public:
    enum class ASTInternalType
    {
        Int,
        Int8,
        Int16,
        Int32,
        Int64,
        Int128,
        UInt,
        UInt8,
        UInt16,
        UInt32,
        UInt64,
        UInt128,
        Void,
        Char,
        Byte,
        String,
        Float,
        Float32,
        Float64,
        Float128,
        Bool,
        Error,
    };

    ASTTypeSpecifier(ASTInternalType type) : type_(type) {}
    NodeType getType() const override { return NodeType::TypeSpecifier; }
    ASTInternalType getTypeValue() const { return type_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "TypeSpecifier: " << formatInternalType(type_);
    }

private:
    ASTInternalType type_;

    std::string formatInternalType(ASTInternalType type) const
    {
        switch (type)
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
        case ASTInternalType::Float:
            return "float";
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
        default:
            return "Unknown Type";
        }
    }
};

#endif // AST_TYPES_HPP
