#ifndef CODEGEN_C_HPP
#define CODEGEN_C_HPP

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include "codegen_c_program.hpp"

class CodeGenCModule;
class CodeGenCSourceFile;

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

    CodeGenC(CodeGenCModule *headModule, OutputKind outputKind) : modules_{headModule}, outputKind_(outputKind) {};
    ~CodeGenC()
    {
        for (auto &&module : modules_)
        {
            delete module;
        }
    }

    void generate();
    void compile();
private:
    std::vector<CodeGenCModule *> modules_;
    OutputKind outputKind_;
};

class CodeGenCModule
{
private:
    std::string moduleName;
    std::vector<CodeGenCSourceFile*> sourceFiles_;
public:
    CodeGenCModule(std::string moduleName) : moduleName(moduleName) {};
    ~CodeGenCModule() {};

    void addSourceFile(const CodeGenCSourceFile *sourceFile)
    {
        // sourceFiles_.push_back(sourceFile);
    }

    void saveModule(std::string outputPath)
    {
        std::ofstream outFile(outputPath + "/" + moduleName + ".c");
        if (outFile.is_open())
        {
            for (const auto &sourceFile : sourceFiles_)
            {
                // outFile << sourceFile->generate() << std::endl;
            }

            outFile.close();
        }
        else
        {
            std::cerr << "(Error) Opening file for writing failed." << std::endl;
        }
    }
};

class CodeGenCSourceFile
{
private:
    ASTProgram *program_;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
public:
    CodeGenCSourceFile(ASTProgram *program) : program_(program) {}
    ~CodeGenCSourceFile() {}

    std::string generate() {
        return "";
    }
};

#endif // CODEGEN_C_HPP
