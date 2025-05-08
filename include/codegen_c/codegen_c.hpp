#ifndef CODEGEN_C_HPP
#define CODEGEN_C_HPP

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <optional>
#include "ast/ast.hpp"
#include "util/util.hpp"

const std::string CIR_DIR = "cir";
const std::string OBJ_DIR = "objects";

class CodeGenCModule;
class CodeGenCSourceFile;

class CodeGenCOptions
{
private:
    std::string outputDirectory_;

public:
    std::string getOutputDirectory() const { return outputDirectory_; }
    void setOutputDirectory(const std::string &outputDirectory) { outputDirectory_ = outputDirectory; }
};

class CodeGenCSourceFile
{
private:
    ASTProgram *program_;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
    std::string moduleName_;

public:
    CodeGenCSourceFile(ASTProgram *program, const std::string &moduleName) : program_(program), moduleName_(moduleName) {}
    ~CodeGenCSourceFile()
    {
        delete program_;
    }

    const std::string &getModuleName()
    {
        return moduleName_;
    }

    std::pair<std::string, std::string> generate();
};

class CodeGenCModule
{
private:
    std::string moduleName_;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
    std::string moduleCIROutputPath_;
    CodeGenCOptions opts_;

public:
    CodeGenCModule(CodeGenCOptions opts, std::string moduleName) : moduleName_(moduleName), opts_(opts)
    {
        util::ensureDirectoryExists(opts_.getOutputDirectory() + "/" + CIR_DIR);
        util::ensureDirectoryExists(opts_.getOutputDirectory() + "/" + CIR_DIR + "/" + moduleName);
        moduleCIROutputPath_ = opts_.getOutputDirectory() + "/" + CIR_DIR + "/" + moduleName;
    };
    ~CodeGenCModule() {}

    void addSourceFile(CodeGenCSourceFile *sourceFile)
    {
        std::pair<std::string, std::string> generatedCode = sourceFile->generate();
        sourceStream_ << generatedCode.first;
        headerStream_ << generatedCode.second;
        delete sourceFile;
    }

    const std::string &getModuleName() { return moduleName_; }
    const std::string &getModuleCIROutputPath() { return moduleCIROutputPath_; }
    const std::string generateObjectFile();
    void saveModule()
    {
        const std::string cirOutputDirectory = getModuleCIROutputPath();

        std::ofstream outSourceFile(cirOutputDirectory + "/" + moduleName_ + ".c");
        std::ofstream outHeaderFile(cirOutputDirectory + "/" + moduleName_ + ".h");

        if (outSourceFile.is_open() && outHeaderFile.is_open())
        {
            outSourceFile << sourceStream_.str() << std::endl;
            outHeaderFile << headerStream_.str() << std::endl;

            outSourceFile.close();
            outHeaderFile.close();
        }
        else
        {
            std::cerr << "(Error) Opening file for writing failed." << std::endl;
            std::cerr << "        Given path: " << cirOutputDirectory << std::endl;

            if (!outSourceFile.is_open())
            {
                std::cerr << "          Failed to open source file: " << cirOutputDirectory + "/" + moduleName_ + ".c" << std::endl;
            }

            if (!outHeaderFile.is_open())
            {
                std::cerr << "          Failed to open header file: " << cirOutputDirectory + "/" + moduleName_ + ".h" << std::endl;
            }
        }
    }
};

class CodeGenC
{
public:
    enum class OutputKind
    {
        Assembly,
        CIR,
        ObjectFile,
        DynamicLibrary,
        StaticLibrary,
        Executable,
    };

    CodeGenC(CodeGenCModule *headModule, OutputKind outputKind, CodeGenCOptions opts) : modules_{headModule}, outputKind_(outputKind), opts_(opts) {};
    ~CodeGenC()
    {
        for (auto &&module : modules_)
        {
            // warning: deleting pointer to incomplete type 'CodeGenCModule' is incompatible with C++2c and may cause undefined behavior [-Wdelete-incomplete]
            delete module;
        }
    }

    const std::string getExecutableOutputPath()
    {
#ifdef _WIN32
        return opts_.getOutputDirectory() + "/" + modules_[0]->getModuleName() + ".exe";
#else
        return opts_.getOutputDirectory() + "/" + modules_[0]->getModuleName();
#endif
    }

    const std::string getObjectsOutputPath()
    {
        return opts_.getOutputDirectory() + "/" + OBJ_DIR;
    }

    void generate();
    void compile();

private:
    std::vector<CodeGenCModule *> modules_;
    OutputKind outputKind_;
    CodeGenCOptions opts_;
};

class CodeGenCValue
{
public:
    enum class ValueType
    {
        LValue,
        RValue,
        Type,
        Instruction
    };

    CodeGenCValue(std::string source, std::string header, ValueType type) : source_(source), header_(header), type_(type) {};

    std::string getSource() const { return source_; }
    void setSource(std::string source) { source_ = source; }

    std::string getHeader() const { return header_; }
    void setHeader(std::string header) { header_ = header; }

    ValueType getType() const { return type_; }
    void setType(ValueType type) { type_ = type; }

private:
    std::string source_;
    std::string header_;
    ValueType type_;
};

using CodeGenCValuePtr = CodeGenCValue *;

CodeGenCValuePtr codeGenC_StorageClassSpecifier(ASTStorageClassSpecifier storageClassSpecifier);
std::pair<std::string, std::string> codeGenCStatementList(ASTNodeList nodeList);
std::pair<std::string, std::string> codeGenCStatement(ASTNodePtr statement);
CodeGenCValuePtr codeGenC_VariableDeclaration(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_TypeSpecifier(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenCStatementList(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenCExpression(ASTNodePtr nodePtr);

#endif // CODEGEN_C_HPP
