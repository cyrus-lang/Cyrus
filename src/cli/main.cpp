#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <map>
#include <algorithm>
#include <cstdio>
#include "lexer/lexer.hpp"
#include "parser/cyrus.tab.hpp"
#include "cli/cli.hpp"

int main(int argc, char *argv[]) {
    std::vector<std::string> args(argv + 1, argv + argc);
    std::map<std::string, std::vector<std::string>> parsedArgs = parseArguments(args);

    if (parsedArgs.count("command") > 0) {
        const std::string& command = parsedArgs["command"][0];

        if (command == "run") {
            runCommand(args);
        } else if (command == "compile") {
            compileCommand(args);
        } else if (command == "compile-dylib") {
            compileDylibCommand(args);
        } else if (command == "compile-obj") {
            compileObjCommand(args);
        } else if (command == "compile-asm") {
            compileAsmCommand(args);
        } else if (command == "parse-only") {
            parseOnlyCommand(args);
        } else if (command == "lex-only") {
            lexOnlyCommand(args);
        } else if (command == "help") {
            helpCommand();
        } else if (command == "version") {
            versionCommand();
        } else {
            std::cerr << "Error: Unknown command '" << command << "'." << std::endl;
            return 1;
        }
    } else {
        helpCommand();
        return 1;
    }

    return 0;
}

void runCommand(const std::vector<std::string>& args) {
    
}

void compileCommand(const std::vector<std::string>& args) {
    
}

void compileDylibCommand(const std::vector<std::string>& args) {
   
}

void compileObjCommand(const std::vector<std::string>& args) {
   
}

void compileAsmCommand(const std::vector<std::string>& args) {
   
}

void parseOnlyCommand(const std::vector<std::string>& args) {
    
}

void lexOnlyCommand(const std::vector<std::string>& args) {
    if (args.size() != 2) {
        std::cerr << "Error: Incorrect number of arguments." << std::endl;
        return;   
    }

    std::string inputFile = args[1];

    size_t dotPos = inputFile.rfind('.');
    if (dotPos == std::string::npos || inputFile.substr(dotPos) != ".cyr") {
        std::cerr << "Error: Input file '" << inputFile << "' does not have the required '.cyr' extension." << std::endl;
        return;
    }

    yyin = fopen(inputFile.c_str(), "r");
    if (!yyin) {
        std::cerr << "Error: Could not open file '" << inputFile << "'." << std::endl;
        return;
    }

    int token_kind;
    while((token_kind = yylex()))
    {   
        yytokentype token = static_cast<yytokentype>(token_kind);

        std::cout << "token: " << token << std::endl;
    }
}

void helpCommand() {
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

void versionCommand() {
    std::cout << "Cyrus v1.0.0" << std::endl;
}