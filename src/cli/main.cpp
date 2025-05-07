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
#include "codegen_c/codegen_c.hpp"

ASTProgram *parseProgram(const std::string &inputFile)
{
    astProgram = nullptr;
    yyin = nullptr;
    yyin = fopen(inputFile.c_str(), "r");
    if (!yyin)
    {
        std::cerr << "(Error) Could not open file '" << inputFile << "'." << std::endl;
        std::exit(1);
    }

    yyfilename = (char *)inputFile.c_str();

    if (yyparse() == 0)
    {
        return (ASTProgram *)astProgram;
    }
    else
    {
        std::string errorMsg = yyerrormsg;
        util::displayErrorPanel(inputFile, util::readFileContent(inputFile), yylineno, errorMsg);
        std::exit(1);
    }

    fclose(yyin);
}

void compileCommandHelp()
{
    std::cout << "Usage: cyrus compile <input_file> [options]" << std::endl;
    std::cout << std::endl;
    std::cout << "Compile a Cyrus source file." << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "  -o, --output=<dirpath>  Specify the output directory for the compiled executable." << std::endl;
    std::cout << "  -h, --help           Display this help message." << std::endl;
}

CodeGenCOptions collectCodeGenCOptions(argh::parser &cmdl)
{
    CodeGenCOptions opts;

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

    std::optional<std::string> outputDirectory;

    for (auto &param : cmdl.params())
    {
        if (param.first == "o" || param.first == "output")
            outputDirectory = param.second;
    }

    if (!outputDirectory.has_value())
        opts.setOutputDirectory("./build");
    else
        opts.setOutputDirectory(outputDirectory.value());

    return opts;
}

void compileCommand(argh::parser &cmdl)
{
    std::string inputFile = cmdl[2];
    util::checkInputFileExtension(inputFile);

    ASTProgram *program = parseProgram(inputFile);

    std::string fileName = util::getFileNameWithStem(inputFile);
    util::isValidModuleName(fileName, inputFile);

    std::string headModuleName = fileName;
    CodeGenCModule *headModule = new CodeGenCModule(headModuleName);

    CodeGenCSourceFile *sourceFile = new CodeGenCSourceFile(program);
    headModule->addSourceFile(sourceFile);

    CodeGenCOptions opts = collectCodeGenCOptions(cmdl);

    CodeGenC *codegen_c = new CodeGenC(headModule, CodeGenC::OutputKind::Executable, opts);
    codegen_c->generate();
    codegen_c->compile();
}

void compileDylibCommand(argh::parser &cmdl)
{
}

void compileObjCommand(argh::parser &cmdl)
{
}

void compileAsmCommand(argh::parser &cmdl)
{
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

    fclose(yyin);
}

void helpCommand()
{
    std::cout << "Usage: program [command] [options]" << std::endl;
    std::cout << "Commands:" << std::endl;
    std::cout << "  run             Execute a program." << std::endl;
    std::cout << "  compile         Compile source code." << std::endl;
    std::cout << "  compile-dylib   Compile source code into a dynamic library." << std::endl;
    std::cout << "  compile-obj     Compile source code into an object file." << std::endl;
    std::cout << "  compile-asm     Compile source code into assembly code." << std::endl;
    std::cout << "  parse-only      Parse and visit source tree." << std::endl;
    std::cout << "  lex-only        Lex and visit tokens." << std::endl;
    std::cout << "  help            Display this help message." << std::endl;
    std::cout << "  version         Display the program version." << std::endl;
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
        else if (command == "compile-obj")
            compileObjCommand(cmdl);
        else if (command == "compile-asm")
            compileAsmCommand(cmdl);
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