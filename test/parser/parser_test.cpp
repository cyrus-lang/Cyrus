#include <gtest/gtest.h>
#include "lexer/lexer.hpp"
#include "parser/parser.hpp"
#include "util/util.hpp"
#include "ast/ast.hpp"
#include <string>

#include "function_test.cpp"
#include "expression_test.cpp"

const std::string unitTestFileName = "unit-test";

ASTNodePtr quickParse(std::string input)
{
    yyin = nullptr;
    astProgram = nullptr;

    yyin = fmemopen((void *)input.c_str(), input.size(), "r");
    if (!yyin)
    {
        std::cerr << "(Error) Could not open input stream." << std::endl;
        std::exit(1);
    }

    set_lex_only_option(0);
    yyfilename = (char *)unitTestFileName.c_str();

    if (yyparse() == 0)
    {
        if (astProgram == nullptr)
        {
            std::cerr << "(Error) ASTProgram is not initialized correctly.'" << std::endl;
            std::exit(1);
        }
        fclose(yyin);
        yylex_destroy();

        return astProgram;
    }
    else
    {
        std::string errorMsg = yyerrormsg;
        util::displayErrorPanel("unit-test", input, yylineno, errorMsg);
        std::exit(1);
    }

    fclose(yyin);
    yylex_destroy();
    return nullptr;
}

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}