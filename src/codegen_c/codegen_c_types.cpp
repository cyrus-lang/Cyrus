#include <iostream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr generateCType(const std::string &typeName)
{
    return new CodeGenCValue(typeName, std::string(), CodeGenCValue::ValueType::Type);
}

CodeGenCValuePtr codeGenC_IdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
CodeGenCValuePtr codeGenC_PointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
CodeGenCValuePtr codeGenC_ConstTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
CodeGenCValuePtr codeGenC_VolatileTypeSpecifier(ASTTypeSpecifier *typeSpecifier);

CodeGenCValuePtr codeGenC_TypeSpecifier(ASTNodePtr nodePtr)
{
    ASTTypeSpecifier *node = static_cast<ASTTypeSpecifier *>(nodePtr);
    switch (node->getTypeValue())
    {
    case ASTTypeSpecifier::ASTInternalType::Int:
        return generateCType("int");
    case ASTTypeSpecifier::ASTInternalType::Int8:
        return generateCType("int8_t");
    case ASTTypeSpecifier::ASTInternalType::Int16:
        return generateCType("int16_t");
    case ASTTypeSpecifier::ASTInternalType::Int32:
        return generateCType("int32_t");
    case ASTTypeSpecifier::ASTInternalType::Int64:
        return generateCType("int64_t");
    case ASTTypeSpecifier::ASTInternalType::UInt:
        return generateCType("unsigned int");
    case ASTTypeSpecifier::ASTInternalType::UInt8:
        return generateCType("uint8_t");
    case ASTTypeSpecifier::ASTInternalType::UInt16:
        return generateCType("uint16_t");
    case ASTTypeSpecifier::ASTInternalType::UInt32:
        return generateCType("uint32_t");
    case ASTTypeSpecifier::ASTInternalType::UInt64:
        return generateCType("uint64_t");
    case ASTTypeSpecifier::ASTInternalType::Char:
        return generateCType("char");
    case ASTTypeSpecifier::ASTInternalType::Float32:
        return generateCType("float");
    case ASTTypeSpecifier::ASTInternalType::Float64:
        return generateCType("double");
    case ASTTypeSpecifier::ASTInternalType::Float128:
        return generateCType("long double");
    case ASTTypeSpecifier::ASTInternalType::Void:
        return generateCType("void");
    case ASTTypeSpecifier::ASTInternalType::String:
        return generateCType("const char*");
    case ASTTypeSpecifier::ASTInternalType::Bool:
        return generateCType("bool");
    case ASTTypeSpecifier::ASTInternalType::Identifier:
        return codeGenC_IdentifierTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Pointer:
        return codeGenC_PointerTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Const:
        return codeGenC_ConstTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Volatile:
        return codeGenC_VolatileTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::AddressOf:
        return codeGenC_PointerTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Byte:
        std::cerr << "Byte type not implemented yet." << std::endl;
        exit(1);
    case ASTTypeSpecifier::ASTInternalType::Error:
        std::cerr << "Error type not implemented yet." << std::endl;
        exit(1);
    default:
        std::cerr << "Unable to generate C code for unsupported ASTTypeSpecifier::ASTInternalType." << std::endl;
        exit(1);
    };
}

CodeGenCValuePtr codeGenC_IdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Identifier type specifier has no inner identifier." << std::endl;
        exit(1);
    }

    if (inner->getType() != ASTTypeSpecifier::NodeType::Identifier)
    {
        std::cerr << "Inner node is not an identifier." << std::endl;
        exit(1);
    }

    ASTIdentifier *identifier = static_cast<ASTIdentifier *>(inner);
    return new CodeGenCValue(identifier->getName(), std::string(), CodeGenCValue::ValueType::Type);
}

CodeGenCValuePtr codeGenC_PointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Pointer type specifier has no inner type." << std::endl;
        exit(1);
    }

    CodeGenCValuePtr innerType = codeGenC_TypeSpecifier(inner);
    return new CodeGenCValue(innerType->getSource() + "*", std::string(), CodeGenCValue::ValueType::Type);
}

CodeGenCValuePtr codeGenC_ConstTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Const type specifier has no inner type." << std::endl;
        exit(1);
    }

    CodeGenCValuePtr innerType = codeGenC_TypeSpecifier(inner);
    return new CodeGenCValue("const " + innerType->getSource(), std::string(), CodeGenCValue::ValueType::Type);
}

CodeGenCValuePtr codeGenC_VolatileTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Volatile type specifier has no inner type." << std::endl;
        exit(1);
    }

    CodeGenCValuePtr innerType = codeGenC_TypeSpecifier(inner);
    return new CodeGenCValue("volatile " + innerType->getSource(), std::string(), CodeGenCValue::ValueType::Type);
}