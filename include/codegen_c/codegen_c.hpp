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

    void generate();
    void compile();

private:
    std::vector<CodeGenCModule *> modules_;
    OutputKind outputKind_;
    CodeGenCOptions opts_;
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
    std::string moduleName;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
    // TODO Collect generated modules names
    // This gonna be used later when compiling through clang/gcc.

public:
    CodeGenCModule(std::string moduleName) : moduleName(moduleName) {};
    ~CodeGenCModule() {}

    void addSourceFile(CodeGenCSourceFile *sourceFile)
    {
        std::pair<std::string, std::string> generatedCode = sourceFile->generate();
        sourceStream_ << generatedCode.first;
        headerStream_ << generatedCode.second;
        delete sourceFile;
    }

    void saveModule(std::string outputPath)
    {
        const std::string cirOutputDirectory = outputPath + "/" + CIR_DIR + "/" + moduleName;

        util::ensureDirectoryExists(outputPath);
        util::ensureDirectoryExists(outputPath + "/" + CIR_DIR);
        util::ensureDirectoryExists(outputPath + "/" + CIR_DIR + "/" + moduleName);

        std::ofstream outSourceFile(cirOutputDirectory + "/" + moduleName + ".c");
        std::ofstream outHeaderFile(cirOutputDirectory + "/" + moduleName + ".h");

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
            std::cerr << "        Given path: " << outputPath << std::endl;

            if (!outSourceFile.is_open())
            {
                std::cerr << "          Failed to open source file: " << outputPath + "/" + moduleName + ".c" << std::endl;
            }

            if (!outHeaderFile.is_open())
            {
                std::cerr << "          Failed to open header file: " << outputPath + "/" + moduleName + ".h" << std::endl;
            }
        }
    }
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

using CodeGenCValuePtr = CodeGenCValue*;

std::pair<std::string, std::string> codeGenCStatement(ASTNodePtr statement);
CodeGenCValuePtr codeGenCExpression(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_TypeSpecifier(ASTNodePtr nodePtr);

#endif // CODEGEN_C_HPP
