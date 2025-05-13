#include <iostream>
#include <util/util.hpp>
#include <parser/parser.hpp>
#include "codegen_llvm/compiler.hpp"

void new_codegen_llvm(CodeGenLLVM_Options opts)
{
    CodeGenLLVM_Context ctx;

    if (opts.getInputFile().has_value())
    {
        // compiler triggered to compile single files
        std::string filePath = opts.getInputFile().value();
        ASTProgram *program = parseProgram(filePath);
        ctx.compileProgram(program, filePath);
    }
    else
    {
        // compiler gonna get options from a configuration file
        std::cerr << "(Error) Compile with Project.toml is not supported yet." << std::endl;
        exit(1);
    }
}

void CodeGenLLVM_Context::compileProgram(ASTProgram *program, const std::string &filePath)
{
    std::string moduleName = util::getFileNameWithStem(filePath);
    if (program->getModuleName().has_value())
    {
        moduleName = program->getModuleName().value();
    }

    util::isValidModuleName(moduleName, filePath);
    llvm::Module *module = createModule(moduleName);

    std::cout << moduleName << "\n";
    // Start compiling statements

    delete program;
}
