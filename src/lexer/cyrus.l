%option yylineno
%option c++ 

%{
	#include <stdio.h>
	#include <math.h>
	#include "parser/cyrus.tab.hpp"

    static char* process_string_literal(const char *text, int length);
    float strtof(const char *str);
    double strtod(const char *str);
    void display_error(const char *msg);

    int lex_only_option = 1;
    char* yyfilename;
%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%%
[/][*]([^*]|[*]*[^*/])*[*]+[/]          ;
"//"(\\.|[^\n])*                        ;
[ \t\n]									;

"#"										{ return(yy::parser::token::TOK_HASH); }
"fn"									{ return(yy::parser::token::TOK_FUNCTION); }
"import"								{ return(yy::parser::token::TOK_IMPORT); }
"type"									{ return(yy::parser::token::TOK_TYPEDEF); }

"struct"								{ return(yy::parser::token::TOK_STRUCT); }
"class"									{ return(yy::parser::token::TOK_CLASS); }
"interface"								{ return(yy::parser::token::TOK_INTERFACE); }
"public"								{ return(yy::parser::token::TOK_PUBLIC); }
"private"								{ return(yy::parser::token::TOK_PRIVATE); }
"abstract"								{ return(yy::parser::token::TOK_ABSTRACT); }
"virtual"								{ return(yy::parser::token::TOK_VIRTUAL); }
"override"								{ return(yy::parser::token::TOK_OVERRIDE); }
"protected"								{ return(yy::parser::token::TOK_PROTECTED); }

"int"									{ return(yy::parser::token::TOK_INT); }
"int8"									{ return(yy::parser::token::TOK_INT8); }
"int16"									{ return(yy::parser::token::TOK_INT16); }
"int32"									{ return(yy::parser::token::TOK_INT32); }
"int64"									{ return(yy::parser::token::TOK_INT64); }
"int128"								{ return(yy::parser::token::TOK_INT128); }
"uint"									{ return(yy::parser::token::TOK_UINT); }
"unt8"									{ return(yy::parser::token::TOK_UINT8); }
"unt16"									{ return(yy::parser::token::TOK_UINT16); }
"unt32"									{ return(yy::parser::token::TOK_UINT32); }
"unt64"									{ return(yy::parser::token::TOK_UINT64); }
"unt128"								{ return(yy::parser::token::TOK_UINT128); }
"void"									{ return(yy::parser::token::TOK_VOID); }
"char"									{ return(yy::parser::token::TOK_CHAR); }
"byte"									{ return(yy::parser::token::TOK_BYTE); }
"string"								{ return(yy::parser::token::TOK_STRING); }
"float32"								{ return(yy::parser::token::TOK_FLOAT32); }
"float64"								{ return(yy::parser::token::TOK_FLOAT64); }
"float128"								{ return(yy::parser::token::TOK_FLOAT128); }
"bool"									{ return(yy::parser::token::TOK_BOOL); }
"error"									{ return(yy::parser::token::TOK_ERROR); }

"true"									{ return(yy::parser::token::TOK_TRUE); }
"false"									{ return(yy::parser::token::TOK_FALSE); }

"break"									{ return(yy::parser::token::TOK_BREAK); }
"case"									{ return(yy::parser::token::TOK_CASE); }
"char"									{ return(yy::parser::token::TOK_CHAR); }
"const"									{ return(yy::parser::token::TOK_CONST); }
"continue"								{ return(yy::parser::token::TOK_CONTINUE); }
"default"								{ return(yy::parser::token::TOK_DEFAULT); }
"do"									{ return(yy::parser::token::TOK_DO); }
"else"									{ return(yy::parser::token::TOK_ELSE); }
"enum"									{ return(yy::parser::token::TOK_ENUM); }
"for"									{ return(yy::parser::token::TOK_FOR); }
"if"									{ return(yy::parser::token::TOK_IF); }
"return"								{ return(yy::parser::token::TOK_RETURN); }
"switch"								{ return(yy::parser::token::TOK_SWITCH); }
"while"									{ return(yy::parser::token::TOK_WHILE); }

"extern"								{ return(yy::parser::token::TOK_EXTERN); }
"inline"								{ return(yy::parser::token::TOK_INLINE); }

"..."									{ return(yy::parser::token::TOK_ELLIPSIS); }
">>="									{ return(yy::parser::token::TOK_RIGHT_ASSIGN); }
"<<="									{ return(yy::parser::token::TOK_LEFT_ASSIGN); }
"+="									{ return(yy::parser::token::TOK_ADD_ASSIGN); }
"-="									{ return(yy::parser::token::TOK_SUB_ASSIGN); }
"*="									{ return(yy::parser::token::TOK_MUL_ASSIGN); }
"/="									{ return(yy::parser::token::TOK_DIV_ASSIGN); }
"%="									{ return(yy::parser::token::TOK_MOD_ASSIGN); }
"&="									{ return(yy::parser::token::TOK_AND_ASSIGN); }
"^="									{ return(yy::parser::token::TOK_XOR_ASSIGN); }
"|="									{ return(yy::parser::token::TOK_OR_ASSIGN); }
">>"									{ return(yy::parser::token::TOK_RIGHT_OP); }
"<<"									{ return(yy::parser::token::TOK_LEFT_OP); }
"++"									{ return(yy::parser::token::TOK_INC_OP); }
"--"									{ return(yy::parser::token::TOK_DEC_OP); }
"->"									{ return(yy::parser::token::TOK_PTR_OP); }
"&&"									{ return(yy::parser::token::TOK_AND_OP); }
"||"									{ return(yy::parser::token::TOK_OR_OP); }
"<="									{ return(yy::parser::token::TOK_LE_OP); }
">="									{ return(yy::parser::token::TOK_GE_OP); }
"=="									{ return(yy::parser::token::TOK_EQ_OP); }
"!="									{ return(yy::parser::token::TOK_NE_OP); }
";"										{ return(';'); }
("{"|"<%")								{ return('{'); }
("}"|"%>")								{ return('}'); }
","										{ return(','); }
":"										{ return(':'); }
"="										{ return('='); }
"("										{ return('('); }
")"										{ return(')'); }
("["|"<:")								{ return('['); }
("]"|":>")								{ return(']'); }
"."										{ return('.'); }
"&"										{ return('&'); }
"!"										{ return('!'); }
"~"										{ return('~'); }
"-"										{ return('-'); }
"+"										{ return('+'); }
"*"										{ return('*'); }
"/"										{ return('/'); }
"%"										{ return('%'); }
"<"										{ return('<'); }
">"										{ return('>'); }
"^"										{ return('^'); }
"|"										{ return('|'); }
"?"										{ return('?'); }


[ \t\v\n\f ]								;
.										{ /* ignore bad characters */ }

%%

float strtof(const char *str) {
    char *endptr;
    float value;

    errno = 0; 
    value = strtof(str, &endptr);

    if ((errno == ERANGE && (value == HUGE_VALF || value == -HUGE_VALF)) || (errno != 0 && value == 0)) {
        display_error("Strtof failed.");
        return 0.0f; 
    }

    if (endptr == str) {
        display_error("No digits were found.");
        return 0.0f;
    }

    // check for invalid characters
    if (*endptr != '\0' && !strchr("fFlL", *endptr)) { 
        display_error("Invalid characters after float.");
        return 0.0f;
    }

    return value;
}

double strtod(const char *str) {
    char *endptr;
    double value;

    errno = 0; 
    value = strtod(str, &endptr);

    if ((errno == ERANGE && (value == HUGE_VAL || value == -HUGE_VAL)) || (errno != 0 && value == 0)) {
        display_error("Strtod failed.");
        return 0.0; 
    }

    if (endptr == str) {
        display_error("No digits were found.");
        return 0.0;
    }

    // check for invalid characters.
    if (*endptr != '\0' && !strchr("fFlL", *endptr)) { 
        display_error("Invalid characters after double.");
        return 0.0;
    }
    
    return value;
}

static char* process_string_literal(const char *text, int length) {
    const char *start = text;
    if (*start == 'L') {
        start++; 
    }
    start++; 

    const char *end = text + length - 1; 
    size_t len = end - start;

    char *str = (char *)malloc(len + 1);
    if (!str) {
        fprintf(stderr, "(Error) Failed to allocate memory for string literal.\n");
        exit(1);
    }

    memcpy(str, start, len);
    str[len] = '\0';

    return str;
}

void set_lex_only_option(int value) {
    lex_only_option = value;
}

int yywrap(void)
{
    return 1;
}