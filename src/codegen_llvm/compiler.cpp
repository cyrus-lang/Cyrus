#include <iostream>
#include <parser/parser.hpp>
#include "codegen_llvm/compiler.hpp"

void new_codegen_llvm(CodeGenLLVM_Options opts)
{
    CodeGenLLVM_Context ctx;

    if (opts.getInputFiles().has_value())
    {
        // compiler triggered to compile single files
        for (auto &&filePath : opts.getInputFiles().value())
        {
            ASTProgram *program = parseProgram(filePath);
            ctx.compileProgram(program);
        }
    }
    else
    {
        // compiler gonna get options from a configuration file
        std::cerr << "(Error) Compile with Project.toml is not supported yet." << std::endl;
        exit(1);
    }
}