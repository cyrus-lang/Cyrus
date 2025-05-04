#ifndef AST_TYPES_HPP
#define AST_TYPES_HPP

#include "node.hpp"
#include <memory>
#include <any>
#include <stdexcept>
#include <string>

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
        Identifier,
        AddressOf, // nested
        Pointer,   // nested
        Const,     // nested
        Volatile,  // nested
    };

    ASTTypeSpecifier(ASTInternalType type) : type_(type), nestedValue_(nullptr) {}
    ASTTypeSpecifier(ASTInternalType type, std::any nestedValue)
        : type_(type), nestedValue_(nestedValue)
    {
        if (!allowedToHaveNestedValue())
        {
            throw std::runtime_error("Internal type " + formatInternalType() + " cannot have a nested type.");
        }
    }

    NodeType getType() const override { return NodeType::TypeSpecifier; }
    ASTInternalType getTypeValue() const { return type_; }
    std::any getNestedValue() const { return nestedValue_; }

    std::string formatInternalType() const;

    void print(int indent) const override
    {
        std::cout << formatInternalType() << " ";
        if (nestedValue_.has_value())
        {
            if (auto nestedType = std::any_cast<ASTTypeSpecifier>(&nestedValue_))
            {
                nestedType->print(0);
            }
            else if (auto identifier = std::any_cast<std::string>(&nestedValue_))
            {
                std::cout << *identifier;
            }
        }
        std::cout << std::endl;
    }

private:
    ASTInternalType type_;
    std::any nestedValue_;

    bool allowedToHaveNestedValue() const
    {
        return type_ == ASTInternalType::Pointer || type_ == ASTInternalType::AddressOf || type_ == ASTInternalType::Identifier || type_ == ASTInternalType::Const || type_ == ASTInternalType::Volatile;
    }
};

#endif // AST_TYPES_HPP
