#include <iostream>
#include "ast/ast.hpp"
#include "codegen_c/codegen_c.hpp"
#include "codegen_c/codegen_c_generator.hpp"

std::string CodeGenCGenerator::generateTypeSpecifier(ASTNodePtr nodePtr)
{
    ASTTypeSpecifier *node = static_cast<ASTTypeSpecifier *>(nodePtr);
    switch (node->getTypeValue())
    {
    case ASTTypeSpecifier::ASTInternalType::Int:
        return "int";
    case ASTTypeSpecifier::ASTInternalType::Int8:
        return "int8_t";
    case ASTTypeSpecifier::ASTInternalType::Int16:
        return "int16_t";
    case ASTTypeSpecifier::ASTInternalType::Int32:
        return "int32_t";
    case ASTTypeSpecifier::ASTInternalType::Int64:
        return "int64_t";
    case ASTTypeSpecifier::ASTInternalType::UInt:
        return "unsigned int";
    case ASTTypeSpecifier::ASTInternalType::UInt8:
        return "uint8_t";
    case ASTTypeSpecifier::ASTInternalType::UInt16:
        return "uint16_t";
    case ASTTypeSpecifier::ASTInternalType::UInt32:
        return "uint32_t";
    case ASTTypeSpecifier::ASTInternalType::UInt64:
        return "uint64_t";
    case ASTTypeSpecifier::ASTInternalType::Char:
        return "char";
    case ASTTypeSpecifier::ASTInternalType::Float32:
        return "float";
    case ASTTypeSpecifier::ASTInternalType::Float64:
        return "double";
    case ASTTypeSpecifier::ASTInternalType::Float128:
        return "long double";
    case ASTTypeSpecifier::ASTInternalType::Void:
        return "void";
    case ASTTypeSpecifier::ASTInternalType::String:
        return "const char*";
    case ASTTypeSpecifier::ASTInternalType::Bool:
        return "bool";
    case ASTTypeSpecifier::ASTInternalType::Identifier:
        return generateIdentifierTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Pointer:
    case ASTTypeSpecifier::ASTInternalType::AddressOf:
        return generatePointerTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Const:
        return generateConstTypeSpecifier(node);
    case ASTTypeSpecifier::ASTInternalType::Volatile:
        return generateVolatileTypeSpecifier(node);
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

std::string CodeGenCGenerator::generateIdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
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
    return identifier->getName();
}

std::string CodeGenCGenerator::generatePointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Pointer type specifier has no inner type." << std::endl;
        exit(1);
    }

    return generateTypeSpecifier(inner) + "*";
}

std::string CodeGenCGenerator::generateConstTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Const type specifier has no inner type." << std::endl;
        exit(1);
    }

    return "const " + generateTypeSpecifier(inner);
}

std::string CodeGenCGenerator::generateVolatileTypeSpecifier(ASTTypeSpecifier *typeSpecifier)
{
    ASTNodePtr inner = typeSpecifier->getInner();
    if (inner == nullptr)
    {
        std::cerr << "Volatile type specifier has no inner type." << std::endl;
        exit(1);
    }

    return "volatile " + generateTypeSpecifier(inner);
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
