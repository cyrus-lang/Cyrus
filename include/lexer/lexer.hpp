#ifndef LEXER_H
#define LEXER_H

#include <cstdio>
#include <string>
#include "parser/cyrus.tab.hpp"

extern FILE *yyin;
extern int yylineno;
extern char *yytext;
extern char *yyerrormsg;
extern char *yyfilename;
extern int yylex(void);

class Token
{
public:
    std::string text;
    yytokentype type;

    Token(std::string text, yytokentype type) : text(text), type(type) {};
    ~Token() {};

    std::string visit()
    {
        return text;
    }
};

#endif // LEXER_H
