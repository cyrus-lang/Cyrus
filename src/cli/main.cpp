#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <cstdio>
#include <map>
#include "util/argh.h"
#include "lexer/lexer.hpp"
#include "util/util.hpp"
#include "parser/parser.hpp"
#include "parser/cyrus.tab.hpp"
#include "codegen_llvm/options.hpp"
#include "codegen_llvm/compiler.hpp"

void compileCommandHelp();
void llvmIRCommandHelp();

CodeGenLLVM_Options collectCompilerOptions(argh::parser &cmdl, CodeGenLLVM_OutputKind outputKind)
{
    CodeGenLLVM_Options opts;

    if (cmdl.size() < 3)
    {
        std::cerr << "(Error) Incorrect number of arguments." << std::endl;
        std::cerr << "        Checkout `cyrus compile --help` for more information." << std::endl;
        exit(1);
    }

    if (cmdl[{"-h", "--help"}])
    {
        compileCommandHelp();
        exit(1);
    }

    for (auto &param : cmdl.params())
    {
        if (param.first == "o" || param.first == "output")
            opts.setOutputPath(param.second);
        if (param.first == "build-dir")
            opts.setBuildDirectory(param.second);
    }

    util::checkInputFileExtension(cmdl[2]);
    opts.setInputFile(cmdl[2]);

    opts.setOutputKind(outputKind);

    return opts;
}

void compileCommand(argh::parser &cmdl)
{
    if (cmdl[{"-h", "--help"}])
    {
        compileCommandHelp();
        exit(1);
    }

    CodeGenLLVM_Options opts = collectCompilerOptions(cmdl, CodeGenLLVM_OutputKind::Executable);
    new_codegen_llvm(opts);
}

void llvmIRCommand(argh::parser &cmdl)
{
    if (cmdl[{"-h", "--help"}])
    {
        llvmIRCommandHelp();
        exit(1);
    }

    CodeGenLLVM_Options opts = collectCompilerOptions(cmdl, CodeGenLLVM_OutputKind::LLVMIR);
    new_codegen_llvm(opts);
}

void compileDylibCommand(argh::parser &cmdl)
{
    // std::string inputFile = cmdl[2];
    // util::checkInputFileExtension(inputFile);

    // ASTProgram *program = parseProgram(inputFile);

    // std::string fileName = util::getFileNameWithStem(inputFile);
    // util::isValidModuleName(fileName, inputFile);

    // CodeGenCOptions opts = collectCodeGenCOptions(cmdl);

    // std::string headModuleName = fileName;
    // CodeGenCModule *headModule = new CodeGenCModule(opts, headModuleName);

    // CodeGenCSourceFile *sourceFile = new CodeGenCSourceFile(program, headModuleName);
    // headModule->addSourceFile(sourceFile);

    // CodeGenC *codegen_c = new CodeGenC(headModule, CodeGenC::OutputKind::DynamicLibrary, opts);
    // codegen_c->generate();
    // codegen_c->compile();
}

void compileStaticlibCommand(argh::parser &cmdl)
{
    // std::string inputFile = cmdl[2];
    // util::checkInputFileExtension(inputFile);

    // ASTProgram *program = parseProgram(inputFile);

    // std::string fileName = util::getFileNameWithStem(inputFile);
    // util::isValidModuleName(fileName, inputFile);

    // CodeGenCOptions opts = collectCodeGenCOptions(cmdl);

    // std::string headModuleName = fileName;
    // CodeGenCModule *headModule = new CodeGenCModule(opts, headModuleName);

    // CodeGenCSourceFile *sourceFile = new CodeGenCSourceFile(program, headModuleName);
    // headModule->addSourceFile(sourceFile);

    // CodeGenC *codegen_c = new CodeGenC(headModule, CodeGenC::OutputKind::StaticLibrary, opts);
    // codegen_c->generate();
    // codegen_c->compile();
}

void compileObjCommand(argh::parser &cmdl)
{
    // std::string inputFile = cmdl[2];
    // util::checkInputFileExtension(inputFile);

    // ASTProgram *program = parseProgram(inputFile);

    // std::string fileName = util::getFileNameWithStem(inputFile);
    // util::isValidModuleName(fileName, inputFile);

    // CodeGenCOptions opts = collectCodeGenCOptions(cmdl);

    // std::string headModuleName = fileName;
    // CodeGenCModule *headModule = new CodeGenCModule(opts, headModuleName);

    // CodeGenCSourceFile *sourceFile = new CodeGenCSourceFile(program, headModuleName);
    // headModule->addSourceFile(sourceFile);

    // CodeGenC *codegen_c = new CodeGenC(headModule, CodeGenC::OutputKind::ObjectFile, opts);
    // codegen_c->generate();
    // codegen_c->compile();
}

void compileAsmCommand(argh::parser &cmdl)
{
    // std::string inputFile = cmdl[2];
    // util::checkInputFileExtension(inputFile);

    // ASTProgram *program = parseProgram(inputFile);

    // std::string fileName = util::getFileNameWithStem(inputFile);
    // util::isValidModuleName(fileName, inputFile);

    // CodeGenCOptions opts = collectCodeGenCOptions(cmdl);

    // std::string headModuleName = fileName;
    // CodeGenCModule *headModule = new CodeGenCModule(opts, headModuleName);

    // CodeGenCSourceFile *sourceFile = new CodeGenCSourceFile(program, headModuleName);
    // headModule->addSourceFile(sourceFile);

    // CodeGenC *codegen_c = new CodeGenC(headModule, CodeGenC::OutputKind::Assembly, opts);
    // codegen_c->generate();
    // codegen_c->compile();
}

void runCommand(argh::parser &cmdl)
{
}

void parseOnlyCommand(argh::parser &cmdl)
{
    if (cmdl.size() != 3)
    {
        std::cerr << "(Error) Incorrect number of arguments." << std::endl;
        std::cerr << "        Checkout `cyrus parse-only --help` for more information." << std::endl;
        return;
    }

    std::string inputFile = cmdl[2];
    util::checkInputFileExtension(inputFile);

    ASTProgram *program = parseProgram(inputFile);

    if (program)
    {
        program->print(0);
    }
    else
    {
        std::cerr << "(Error) ASTProgram is not initialized correctly.'" << std::endl;
        std::exit(1);
    }

    delete program;
}

void lexOnlyCommand(argh::parser &cmdl)
{
    if (cmdl.size() != 3)
    {
        std::cerr << "(Error) Incorrect number of arguments." << std::endl;
        std::cerr << "        Checkout `cyrus lex-only --help` for more information." << std::endl;
        return;
    }

    std::string inputFile = cmdl[2];
    util::checkInputFileExtension(inputFile);

    yyin = nullptr;
    yyin = fopen(inputFile.c_str(), "r");
    if (!yyin)
    {
        std::cerr << "(Error) Could not open file '" << inputFile << "'." << std::endl;
        std::exit(1);
    }

    yyfilename = (char *)inputFile.c_str();

    int token_kind;
    while ((token_kind = yylex()))
    {
        yytokentype tokenType = static_cast<yytokentype>(token_kind);
        Token token(yytext, tokenType);

        std::cout << "Token: " << token.visit() << std::endl;
    }

    yylex_destroy();
}

void helpCommand()
{
    std::cout << "Usage: program [command] [options]" << std::endl;
    std::cout << "Commands:" << std::endl;
    std::cout << "  run                     Execute a program." << std::endl;
    std::cout << "  compile                 Compile source code." << std::endl;
    std::cout << "  compile-dylib           Compile source code into a dynamic library." << std::endl;
    std::cout << "  compile-staticlib       Compile source code into a static library." << std::endl;
    std::cout << "  compile-obj             Compile source code into an object file." << std::endl;
    std::cout << "  compile-asm             Compile source code into assembly code." << std::endl;
    std::cout << "  llvmir                  Compile source code into llvm-ir." << std::endl;
    std::cout << "  parse-only              Parse and visit source tree." << std::endl;
    std::cout << "  lex-only                Lex and visit tokens." << std::endl;
    std::cout << "  help                    Display this help message." << std::endl;
    std::cout << "  version                 Display the program version." << std::endl;
}

void versionCommand()
{
    std::cout << "Cyrus v1.0.0" << std::endl;
}

int main(int argc, char *argv[])
{
    argh::parser cmdl(argc, argv);

    if (argc > 1)
    {
        std::string command = argv[1];
        if (command == "run")
            runCommand(cmdl);
        else if (command == "compile")
            compileCommand(cmdl);
        else if (command == "compile-dylib")
            compileDylibCommand(cmdl);
        else if (command == "compile-staticlib")
            compileStaticlibCommand(cmdl);
        else if (command == "compile-obj")
            compileObjCommand(cmdl);
        else if (command == "compile-asm")
            compileAsmCommand(cmdl);
        else if (command == "llvmir")
            llvmIRCommand(cmdl);
        else if (command == "parse-only")
            parseOnlyCommand(cmdl);
        else if (command == "lex-only")
            lexOnlyCommand(cmdl);
        else if (command == "version")
            versionCommand();
        else if (command == "help")
            helpCommand();
        else
        {
            std::cerr << "(Error) Unknown command '" << command << "'." << std::endl;
            return 1;
        }
    }
    else
    {
        helpCommand();
    }

    return 0;
}

void compileCommandHelp()
{
    std::cout << "Usage: cyrus compile <input_file> [options]" << std::endl;
    std::cout << std::endl;
    std::cout << "Compile a Cyrus source file." << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "  -o, --output=<dirpath>       Specify the output directory for the final executable." << std::endl;
    std::cout << "      --build-dir=<dirpath>    Specify the directory to store intermediate build files" << std::endl;
    std::cout << "                               such as object files and LLVM IR." << std::endl;
    std::cout << "  -h, --help                   Display this help message." << std::endl;
}

void llvmIRCommandHelp()
{
    std::cout << "Usage: cyrus llvmir <input_file> [options]" << std::endl;
    std::cout << std::endl;
    std::cout << "Compile Cyrus source files into llvm-ir." << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "  -o, --output=<dirpath>       Specify the output directory for the llvm-ir files." << std::endl;
    std::cout << "      --build-dir=<dirpath>    Specify the directory to store intermediate build files" << std::endl;
    std::cout << "                               such as object files and LLVM IR." << std::endl;
    std::cout << "  -h, --help                   Display this help message." << std::endl;
}