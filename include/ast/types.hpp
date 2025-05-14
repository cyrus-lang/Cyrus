#ifndef AST_TYPES_HPP
#define AST_TYPES_HPP

#include "node.hpp"
#include <memory>
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
        Float32,
        Float64,
        Float128,
        Bool,
        Error,
        Identifier, // nested
        Reference,  // nested
        Pointer,    // nested
        Const,      // nested
    };

    ASTTypeSpecifier(ASTInternalType type) : type_(type), inner_(nullptr) {}
    ASTTypeSpecifier(ASTInternalType type, ASTNodePtr inner)
        : type_(type), inner_(inner)
    {
        if (!allowedToHaveNestedValue())
        {
            throw std::runtime_error("Internal type " + formatInternalType() + " cannot have a inner value.");
        }
    }
    ~ASTTypeSpecifier()
    {
        delete inner_;
    }

    NodeType getType() const override { return NodeType::TypeSpecifier; }
    ASTInternalType getTypeValue() const { return type_; }
    ASTNodePtr getInner() const { return inner_; }

    std::string formatInternalType() const;

    void print(int indent) const override
    {
        if (type_ == ASTInternalType::Identifier)
        {
            std::cout << "Identifier";
            return;
        }

        std::cout << formatInternalType() << " ";
        if (inner_)
        {
            inner_->print(indent);
        }
        std::cout << std::endl;
    }

private:
    ASTInternalType type_;
    ASTNodePtr inner_;

    bool allowedToHaveNestedValue() const
    {
        return type_ == ASTInternalType::Pointer || type_ == ASTInternalType::Reference || type_ == ASTInternalType::Identifier || type_ == ASTInternalType::Const;
    }
};

#endif // AST_TYPES_HPP
