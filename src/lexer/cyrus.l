%option yylineno

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

"#"										{ return(HASH); }
"fn"									{ return(FUNCTION); }
"import"								{ return(IMPORT); }
"module"								{ return(MODULE); }
"type"									{ return(TYPEDEF); }

"struct"								{ return(STRUCT); }
"class"									{ return(CLASS); }
"interface"								{ return(INTERFACE); }
"public"								{ return(PUBLIC); }
"private"								{ return(PRIVATE); }
"abstract"								{ return(ABSTRACT); }
"virtual"								{ return(VIRTUAL); }
"override"								{ return(OVERRIDE); }
"protected"								{ return(PROTECTED); }

"int"									{ return(INT); }
"int8"									{ return(INT8); }
"int16"									{ return(INT16); }
"int32"									{ return(INT32); }
"int64"									{ return(INT64); }
"int128"								{ return(INT128); }
"uint"									{ return(UINT); }
"unt8"									{ return(UINT8); }
"unt16"									{ return(UINT16); }
"unt32"									{ return(UINT32); }
"unt64"									{ return(UINT64); }
"unt128"								{ return(UINT128); }
"void"									{ return(VOID); }
"char"									{ return(CHAR); }
"byte"									{ return(BYTE); }
"string"								{ return(STRING); }
"float32"								{ return(FLOAT32); }
"float64"								{ return(FLOAT64); }
"float128"								{ return(FLOAT128); }
"bool"									{ return(BOOL); }
"error"									{ return(ERROR); }

"break"									{ return(BREAK); }
"case"									{ return(CASE); }
"char"									{ return(CHAR); }
"const"									{ return(CONST); }
"continue"								{ return(CONTINUE); }
"default"								{ return(DEFAULT); }
"do"									{ return(DO); }
"else"									{ return(ELSE); }
"enum"									{ return(ENUM); }
"extern"								{ return(EXTERN); }
"for"									{ return(FOR); }
"if"									{ return(IF); }
"return"								{ return(RETURN); }
"switch"								{ return(SWITCH); }
"while"									{ return(WHILE); }

"static"								{ return(STATIC); }
"register"								{ return(REGISTER); }
"volatile"								{ return(VOLATILE); }

"..."									{ return(ELLIPSIS); }
">>="									{ return(RIGHT_ASSIGN); }
"<<="									{ return(LEFT_ASSIGN); }
"+="									{ return(ADD_ASSIGN); }
"-="									{ return(SUB_ASSIGN); }
"*="									{ return(MUL_ASSIGN); }
"/="									{ return(DIV_ASSIGN); }
"%="									{ return(MOD_ASSIGN); }
"&="									{ return(AND_ASSIGN); }
"^="									{ return(XOR_ASSIGN); }
"|="									{ return(OR_ASSIGN); }
">>"									{ return(RIGHT_OP); }
"<<"									{ return(LEFT_OP); }
"++"									{ return(INC_OP); }
"--"									{ return(DEC_OP); }
"->"									{ return(PTR_OP); }
"&&"									{ return(AND_OP); }
"||"									{ return(OR_OP); }
"<="									{ return(LE_OP); }
">="									{ return(GE_OP); }
"=="									{ return(EQ_OP); }
"!="									{ return(NE_OP); }
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

{L}({L}|{D})*							{
                                            if (lex_only_option != 1) {
                                                yylval.sval = strdup(yytext);
                                                if (!yylval.sval) {
                                                    display_error("(Error) Failed to allocate memory for identifier.");
                                                }
                                            }

                                            return IDENTIFIER;
                                        }

0[xX]{H}+{IS}?   						{ yylval.ival = strtol(yytext, NULL, 16); return INTEGER_CONSTANT; }
0{D}+{IS}?       						{ yylval.ival = strtol(yytext, NULL, 8);  return INTEGER_CONSTANT; }
{D}+{IS}?        						{ yylval.ival = strtol(yytext, NULL, 10); return INTEGER_CONSTANT; }

[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?[fFlL]?     {
                                               yylval.fval = strtof(yytext);
                                               return FLOAT_CONSTANT;
                                            }

[0-9]+[eE][+-]?[0-9]+[fFlL]?                {
                                               yylval.fval = strtof(yytext);
                                               return FLOAT_CONSTANT;
                                            }

\.[0-9]+([eE][+-]?[0-9]+)?[fFlL]?           {
                                               yylval.fval = strtof(yytext);
                                               return FLOAT_CONSTANT;
                                            }

0[xX][0-9a-fA-F]+\.[0-9a-fA-F]*[pP][+-]?[0-9]+[fFlL]?   {   
                                                            yylval.fval = strtof(yytext);
                                                            return DOUBLE_CONSTANT;
                                                        }
0[xX][0-9a-fA-F]+[pP][+-]?[0-9]+[fFlL]?                 {
                                                            yylval.dval = strtod(yytext);
                                                            return DOUBLE_CONSTANT;
                                                        }

[0-9]+\'[0-9]+\.[0-9\'\"]+              {
                                            char *cleaned_text = strdup(yytext);
                                            int j = 0;
                                                for (int i = 0; cleaned_text[i] != '\0'; i++) {
                                                if (cleaned_text[i] != '\'') {
                                                    cleaned_text[j++] = cleaned_text[i];
                                                }
                                            }
                                            cleaned_text[j] = '\0';
                                            yylval.fval = strtof(cleaned_text);
                                            free(cleaned_text);
                                            return FLOAT_CONSTANT;
                                        }

.|\n                                    {
                                            display_error("Invalid character sequence.");
                                        }

L?\"(\\.|[^\\\"])*\"                    {
                                            char *str = process_string_literal(yytext, yyleng);
                                            if (str) {
                                                yylval.sval = str;
                                                return STRING_CONSTANT;
                                            } else {
                                                fprintf(stderr, "(Error) Failed to allocate memory for string literal.\n");
                                                exit(1);
                                            }
                                        }

[ \t\v\n\f ]								;
.										{ /* ignore bad characters */ }

%%

void display_error(const char *msg) {
    fprintf(stderr, "(Error) %s:%d %s\n", yyfilename, yylineno, msg);
    exit(1);
}

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