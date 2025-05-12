#include "parser/parser.hpp"
#include "lexer/lexer.hpp"
#include "util/util.hpp"

ASTProgram *parseProgram(const std::string& inputFile)
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
