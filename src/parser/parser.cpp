#include <memory>
#include "parser/parser.hpp"
#include "lexer/lexer.hpp"
#include "util/util.hpp"

std::pair<std::shared_ptr<std::string>, ASTProgram *> parseProgram(const std::string &inputFile)
{
    astProgram = nullptr;
    yyin = nullptr;
    yyin = fopen(inputFile.c_str(), "r");
    if (!yyin)
    {
        std::cerr << "(Error) Could not open file '" << inputFile << "'." << std::endl;
        std::exit(1);
    }

    set_lex_only_option(0);
    yyfilename = (char *)inputFile.c_str();

    const std::string fileContent = util::readFileContent(inputFile);

    if (yyparse() != 0)
    {
        std::string errorMsg = yyerrormsg;
        util::displayErrorPanel(inputFile, fileContent, yylineno, errorMsg);
        std::exit(1);
    }

    fclose(yyin);
    yylex_destroy();

    if (!astProgram)
    {
        std::cerr << "(Error) ASTProgram is not initialized correctly." << std::endl;
        std::cerr << "        File: " << inputFile << std::endl;
        std::exit(1);
    }

    return std::make_pair(std::make_shared<std::string>(fileContent), (ASTProgram *)astProgram);
}
