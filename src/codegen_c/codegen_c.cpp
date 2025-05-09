#include <iostream>
#include <sstream>
#include "codegen_c/codegen_c.hpp"
#include "codegen_c/codegen_c_scope.hpp"

void compileStaticLibrary(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);
void compileExecutable(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);
void compileDylib(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);

// Generation Step

void CodeGenC::generate()
{
    for (auto &&module : modules_)
    {
        module->saveModule();
    }
}

std::pair<std::string, std::string> codeGenCStatementList(ScopePtr scope, ASTNodeList nodeList)
{
    std::stringstream sourceStream;
    std::stringstream headerStream;

    for (auto &&statement : nodeList)
    {
        auto [source, header] = codeGenCStatement(scope, statement);
        sourceStream << source;
        headerStream << header;
    }

    sourceStream << std::endl;
    return std::make_pair(sourceStream.str(), headerStream.str());
}

CodeGenCValuePtr codeGenCStatementList(ScopePtr scope, ASTNodePtr nodePtr)
{
    if (nodePtr->getType() != ASTNode::NodeType::StatementList)
    {
        std::cerr << "Unable to generate C code for non-statement list." << std::endl;
        exit(1);
    }

    auto [source, header] = codeGenCStatementList(scope, static_cast<ASTStatementList *>(nodePtr)->getStatements());
    return new CodeGenCValue("{\n" + source + "\n}", header, CodeGenCValue::ValueType::Instruction);
}

std::pair<std::string, std::string> codeGenCStatementListTopLevel(ASTNodeList nodeList)
{
    std::stringstream sourceStream;
    std::stringstream headerStream;

    CodeGenCValuePtr value;
    for (auto &&statement : nodeList)
    {
        switch (statement->getType())
        {
        case ASTNode::NodeType::ImportStatement:
            std::cerr << "Import statement not implemented yet." << std::endl;
            exit(1);
            break;
        case ASTNode::NodeType::VariableDeclaration:
            headerStream << codeGenC_VariableDeclaration(nullptr, statement)->getSource();
            break;
        case ASTNode::NodeType::FunctionDefinition:
            value = codeGenC_FunctionDefinition(statement);
            sourceStream << value->getSource();
            headerStream << value->getHeader();
            break;
        case ASTNode::NodeType::FunctionDeclaration:
            value = codeGenC_FunctionDeclaration(statement, false);
            sourceStream << value->getSource();
            headerStream << value->getHeader();
            break;
        default:
            auto [source, header] = codeGenCStatement(nullptr, statement);
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
        compileDylib(modulePaths, getObjectsOutputPath(), getDylibOutputPath());
        break;
    case CodeGenC::OutputKind::StaticLibrary:
        compileStaticLibrary(modulePaths, getObjectsOutputPath(), getStaticLibraryOutputPath());
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

void compileDylib(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath)
{
    std::string objectFiles;
    for (auto &&modulePath : modulePaths)
    {
        objectFiles += modulePath + " ";
    }

    std::string command = "gcc -shared -o " + outputPath + " " + objectFiles;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to create dynamic library." << std::endl;
        exit(1);
    }
}

void compileStaticLibrary(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath)
{
    std::string objectFiles;
    for (auto &&modulePath : modulePaths)
    {
        objectFiles += modulePath + " ";
    }

    std::string command = "ar rcs " + outputPath + " " + objectFiles;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to create static library." << std::endl;
        exit(1);
    }
}
