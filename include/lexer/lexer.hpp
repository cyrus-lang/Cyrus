#ifndef LEXER_H
#define LEXER_H

#include <cstdio>

extern FILE* yyin;
extern char* yytext;
extern int yylex(void);

#endif // LEXER_H