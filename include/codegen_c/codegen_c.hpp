#ifndef CODEGEN_C_HPP
#define CODEGEN_C_HPP

#include <iostream>
#include <sstream>
#include <vector>
#include "functions.hpp"

class CodeGenCModule
{
private:
    std::string moduleName;
    std::stringstream sourceStream;
    std::stringstream headerStream;
public:
    CodeGenCModule(std::string moduleName) : moduleName(moduleName) {};
    ~CodeGenCModule() {};
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

    CodeGenC(CodeGenCModule *mainModule, OutputKind outputKind) : modules_{mainModule}, outputKind_(outputKind) {};
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

#endif // CODEGEN_C_HPP
