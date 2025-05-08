#include <iostream>
#include <sstream>
#include "codegen_c/codegen_c.hpp"

void compileExecutable(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);

// Generation Step

void CodeGenC::generate()
{
    for (auto &&module : modules_)
    {
        module->saveModule();
    }
}

std::pair<std::string, std::string> codeGenCStatementList(ASTNodeList nodeList)
{
    std::stringstream sourceStream;
    std::stringstream headerStream;

    for (auto &&statement : nodeList)
    {
        auto [source, header] = codeGenCStatement(statement);
        sourceStream << source;
        headerStream << header;
    }

    sourceStream << std::endl;
    return std::make_pair(sourceStream.str(), headerStream.str());
}

CodeGenCValuePtr codeGenCStatementList(ASTNodePtr nodePtr)
{
    if (nodePtr->getType() != ASTNode::NodeType::StatementList)
    {
        std::cerr << "Unable to generate C code for non-statement list." << std::endl;
        exit(1);
    }

    auto [source, header] = codeGenCStatementList(static_cast<ASTStatementList *>(nodePtr)->getStatements());
    return new CodeGenCValue("{\n" + source + "\n}", header, CodeGenCValue::ValueType::Instruction);
}

std::pair<std::string, std::string> codeGenCStatementListTopLevel(ASTNodeList nodeList)
{
    std::stringstream sourceStream;
    std::stringstream headerStream;

    for (auto &&statement : nodeList)
    {
        switch (statement->getType())
        {
        case ASTNode::NodeType::ImportStatement:
            std::cerr << "Import statement not implemented yet." << std::endl;
            exit(1);
            break;
        case ASTNode::NodeType::VariableDeclaration:
            headerStream << codeGenC_VariableDeclaration(statement)->getSource();
            break;
        default:
            auto [source, header] = codeGenCStatement(statement);
            sourceStream << source;
            headerStream << header;
            break;
        }
    }

    sourceStream << std::endl;
    return std::make_pair(sourceStream.str(), headerStream.str());
}

std::pair<std::string, std::string> CodeGenCSourceFile::generate()
{
    auto [source, header] = codeGenCStatementListTopLevel(program_->getStatements());

    std::string finalHeader;
    finalHeader += "#include <stdio.h>\n";
    finalHeader += "#include <stdint.h>\n";
    finalHeader += "#include <stdbool.h>\n";
    finalHeader += "\n";
    finalHeader += header;

    std::string finalSource;
    finalSource += "#include \"" + moduleName_ + ".h\"" + "\n";
    finalSource += "\n";
    finalSource += source;

    return std::make_pair(finalSource, finalHeader);
}

// Compilation Step

const std::string CodeGenCModule::generateObjectFile()
{
    std::string headerPath = moduleCIROutputPath_ + "/" + moduleName_ + ".h";
    std::string sourcePath = moduleCIROutputPath_ + "/" + moduleName_ + ".c";
    std::string objectPath = opts_.getOutputDirectory() + "/" + OBJ_DIR;

    util::ensureDirectoryExists(objectPath);

    objectPath += "/" + moduleName_ + ".o";

    std::string command = "gcc -c -o " + objectPath + " " + sourcePath + " -I " + moduleCIROutputPath_;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to compile module `" << moduleName_ << "`." << std::endl;
        exit(1);
    }

    return objectPath;
}

void CodeGenC::compile()
{
    std::vector<std::string> modulePaths;

    for (auto &&module : modules_)
    {
        const std::string objectPath = module->generateObjectFile();
        modulePaths.push_back(objectPath);
    }

    switch (outputKind_)
    {
    case CodeGenC::OutputKind::Assembly:
        /* code */
        break;
    case CodeGenC::OutputKind::CIR:
        /* code */
        break;
    case CodeGenC::OutputKind::ObjectFile:
        /* code */
        break;
    case CodeGenC::OutputKind::DynamicLibrary:
        /* code */
        break;
    case CodeGenC::OutputKind::StaticLibrary:
        /* code */
        break;
    case CodeGenC::OutputKind::Executable:
        compileExecutable(modulePaths, getObjectsOutputPath(), getExecutableOutputPath());
        break;
    default:
        std::cerr << "(Error) Unsupported output kind." << std::endl;
        exit(1);
    }
}

void compileExecutable(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath)
{
    std::string objectFiles;
    for (auto &&modulePath : modulePaths)
    {
        objectFiles += modulePath + " ";
    }

    std::string command = "gcc " + objectFiles + " -o " + outputPath;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to link object files." << std::endl;
        exit(1);
    }
}
