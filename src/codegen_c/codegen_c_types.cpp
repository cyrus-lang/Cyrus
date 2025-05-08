#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"

CodeGenCValuePtr codeGenC_IdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier);

CodeGenCValuePtr codeGenC_TypeSpecifier(ASTNodePtr nodePtr)
{
    ASTTypeSpecifier *node = static_cast<ASTTypeSpecifier *>(nodePtr);
    switch (node->getTypeValue())
    {
    case ASTTypeSpecifier::ASTInternalType::Int:
        return new CodeGenCValue("int", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int8:
        return new CodeGenCValue("int8_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int16:
        return new CodeGenCValue("int16_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int32:
        return new CodeGenCValue("int32_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int64:
        return new CodeGenCValue("int64_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt:
        return new CodeGenCValue("unsigned int", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt8:
        return new CodeGenCValue("uint8_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt16:
        return new CodeGenCValue("uint16_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt32:
        return new CodeGenCValue("uint32_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt64:
        return new CodeGenCValue("uint64_t", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Char:
        return new CodeGenCValue("char", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Float32:
        return new CodeGenCValue("float", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Float64:
        return new CodeGenCValue("double", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Float128:
        return new CodeGenCValue("long double", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Void:
        return new CodeGenCValue("void", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Byte:
        std::cerr << "Byte type not implemented yet." << std::endl;
        exit(1);
        break;
    case ASTTypeSpecifier::ASTInternalType::String:
        return new CodeGenCValue("const char*", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Bool:
        return new CodeGenCValue("bool", std::string(), CodeGenCValue::ValueType::Type);
        break;
    case ASTTypeSpecifier::ASTInternalType::Error:
        std::cerr << "Error type not implemented yet." << std::endl;
        exit(1);
        break;
    case ASTTypeSpecifier::ASTInternalType::Identifier:
        return codeGenC_IdentifierTypeSpecifier(node);
        break;
    case ASTTypeSpecifier::ASTInternalType::Pointer:
    {
        return codeGenC_PointerTypeSpecifier(node);
        exit(1);
    }
    break;
    default:
        std::cerr << "Unable to generate C code for unsupported ASTTypeSpecifier::ASTInternalType." << std::endl;
        exit(1);
        break;
    };
}

CodeGenCValuePtr codeGenC_IdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTIdentifier *identifier = static_cast<ASTIdentifier *>(typeSpecifier->getInner());
    return new CodeGenCValue(identifier->getName(), std::string(), CodeGenCValue::ValueType::Type);
}

CodeGenCValuePtr codeGenC_PointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTIdentifier *pointer = static_cast<ASTIdentifier *>(typeSpecifier->getInner());
    return new CodeGenCValue(identifier->getName(), std::string(), CodeGenCValue::ValueType::Type);
}