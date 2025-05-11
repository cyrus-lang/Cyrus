#include <iostream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"
#include "codegen_c/codegen_c_generator.hpp"

CodeGenCValuePtr CodeGenCGenerator::generateTypeSpecifier(ASTNodePtr nodePtr)
{
    ASTTypeSpecifier *node = static_cast<ASTTypeSpecifier *>(nodePtr);
    switch (node->getTypeValue())
    {
    case ASTTypeSpecifier::ASTInternalType::Int:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int), "int", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int8:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int8), "int8_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int16:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int16), "int16_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int32:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int32), "int32_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Int64:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int64), "int64_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt), "unsigned int", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt8:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt8), "uint8_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt16:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt16), "uint16_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt32:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt32), "uint32_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::UInt64:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt64), "uint64_t", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Char:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Char), "char", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Float32:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float32), "float", CodeGenCValue::ValueKind::Float);
        break;
    case ASTTypeSpecifier::ASTInternalType::Float64:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float64), "double", CodeGenCValue::ValueKind::Float);
        break;
    case ASTTypeSpecifier::ASTInternalType::Float128:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float128), "long double", CodeGenCValue::ValueKind::Float);
        break;
    case ASTTypeSpecifier::ASTInternalType::Void:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Void), "void", CodeGenCValue::ValueKind::Void);
        break;
    case ASTTypeSpecifier::ASTInternalType::String:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::String), "const char*", CodeGenCValue::ValueKind::String);
        break;
    case ASTTypeSpecifier::ASTInternalType::Bool:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Bool), "bool", CodeGenCValue::ValueKind::Int);
        break;
    case ASTTypeSpecifier::ASTInternalType::Identifier:
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Identifier), generateIdentifierTypeSpecifier(node)->getValue(), CodeGenCValue::ValueKind::Identifier);
        break;
    case ASTTypeSpecifier::ASTInternalType::Pointer:
    {
        CodeGenCValuePtr innerType = generatePointerTypeSpecifier(node);
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Pointer), innerType->getValue(), CodeGenCValue::ValueKind::Pointer);
    }
    break;
    case ASTTypeSpecifier::ASTInternalType::AddressOf:
    {
        CodeGenCValuePtr innerType = generatePointerTypeSpecifier(node);
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Pointer), innerType->getValue(), CodeGenCValue::ValueKind::Pointer);
    }
    break;
    case ASTTypeSpecifier::ASTInternalType::Const:
    {
        CodeGenCValuePtr innerType = generateConstTypeSpecifier(node);
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Const), innerType->getValue(), innerType->getValueKind());
    }
    break;
    case ASTTypeSpecifier::ASTInternalType::Volatile:
    {
        CodeGenCValuePtr innerType = generateVolatileTypeSpecifier(node);
        return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Volatile), innerType->getValue(), innerType->getValueKind());
    }
    break;
    case ASTTypeSpecifier::ASTInternalType::Byte:
        std::cerr << "Byte type not implemented yet." << std::endl;
        exit(1);
        break;
    case ASTTypeSpecifier::ASTInternalType::Error:
        std::cerr << "Error type not implemented yet." << std::endl;
        exit(1);
        break;
    default:
        std::cerr << "Unable to generate C code for unsupported ASTTypeSpecifier::ASTInternalType." << std::endl;
        exit(1);
        break;
    };
}

CodeGenCValuePtr CodeGenCGenerator::generateIdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
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
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Identifier, identifier), identifier->getName(), CodeGenCValue::ValueKind::Identifier);
}

CodeGenCValuePtr CodeGenCGenerator::generatePointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Pointer type specifier has no inner type." << std::endl;
        exit(1);
    }

    CodeGenCValuePtr innerType = generateTypeSpecifier(inner);
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Pointer), innerType->getValue() + "*", CodeGenCValue::ValueKind::Pointer);
}

CodeGenCValuePtr CodeGenCGenerator::generateConstTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Const type specifier has no inner type." << std::endl;
        exit(1);
    }

    CodeGenCValuePtr innerType = generateTypeSpecifier(inner);
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Const), "const " + innerType->getValue(), innerType->getValueKind());
}

CodeGenCValuePtr CodeGenCGenerator::generateVolatileTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Volatile type specifier has no inner type." << std::endl;
        exit(1);
    }

    CodeGenCValuePtr innerType = generateTypeSpecifier(inner);
    return new CodeGenCValue(new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Volatile), "volatile " + innerType->getValue(), innerType->getValueKind());
}

std::string CodeGenCGenerator::generateStorageClassSpecifier(ASTStorageClassSpecifier storageClassSpecifier)
{
    switch (storageClassSpecifier)
    {
    case ASTStorageClassSpecifier::Extern:
        return "extern";
    case ASTStorageClassSpecifier::Static:
        return "static";
    case ASTStorageClassSpecifier::Register:
        return "register";
    default:
        std::cerr << "Unable to generate C code for unsupported ASTStorageClassSpecifier." << std::endl;
        exit(1);
    }
}
