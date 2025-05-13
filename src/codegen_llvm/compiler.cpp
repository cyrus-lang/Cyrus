#include <iostream>
#include <util/util.hpp>
#include <parser/parser.hpp>
#include "codegen_llvm/compiler.hpp"

void new_codegen_llvm(CodeGenLLVM_Options opts)
{
    CodeGenLLVM_Context context;

    if (opts.getInputFile().has_value())
    {
        // compiler triggered to compile single files
        std::string filePath = opts.getInputFile().value();
        ASTProgram *program = parseProgram(filePath);

        std::string moduleName = util::getFileNameWithStem(filePath);
        util::isValidModuleName(moduleName, filePath);
        CodeGenLLVM_Module *module = context.createModule(moduleName);

        module->compileProgram(program);
    }
    else
    {
        // compiler gonna get options from a configuration file
        std::cerr << "(Error) Compile with Project.toml is not supported yet." << std::endl;
        exit(1);
    }
}

void CodeGenLLVM_Module::compileProgram(ASTProgram *program)
{
    for (auto &&statement : program->getStatements())
    {
        statement->print(0);
    }

    delete program;
}
