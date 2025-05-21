#include <memory>
#include "parser/parser.hpp"
#include "lexer/lexer.hpp"
#include "util/util.hpp"

std::pair<std::shared_ptr<std::string>, std::shared_ptr<ASTProgram>> parseProgram(const std::string &inputFile)
{
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

    yy::parser parser;
    if (parser.parse() != 0)
    {   
        // FIXME
        // util::displayErrorPanel(inputFile, fileContent, yylineno, yyerrormsg);
        // std::exit(1);
    }

    fclose(yyin);
    yylex_destroy();

    if (!astProgram)
    {
        std::cerr << "(Error) ASTProgram is not initialized correctly." << std::endl;
        std::cerr << "        File: " << inputFile << std::endl;
        std::exit(1);
    }

    return std::make_pair(std::make_shared<std::string>(fileContent), astProgram);
}
