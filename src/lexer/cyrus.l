%option yylineno

%{
	#include <stdio.h>
	#include <math.h>
	#include "parser/cyrus.tab.hpp"
%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%%
"//"(\\.|[^\n])*[\n]					;
[/][*]([^*]|[*]*[^*/])*[*]+[/]          ;
[ \t\n]									;

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
"goto"									{ return(GOTO); }
"if"									{ return(IF); }
"return"								{ return(RETURN); }
"sizeof"								{ return(SIZEOF); }
"switch"								{ return(SWITCH); }
"union"									{ return(UNION); }
"while"									{ return(WHILE); }

"static"								{ return(STATIC); }
"register"								{ return(REGISTER); }
"volatile"								{ return(VOLATILE); }

"struct"								{ return(STRUCT); }

"interface"								{ return(INTERFACE); }
"class"									{ return(CLASS); }
"public"								{ return(PUBLIC); }
"private"								{ return(PRIVATE); }
"abstract"								{ return(ABSTRACT); }
"virtual"								{ return(VIRTUAL); }
"override"								{ return(OVERRIDE); }
"protected"								{ return(PROTECTED); }

"type"									{ return(TYPEDEF); }

"unsigned"								{ return(UNSIGNED); }
"double"								{ return(DOUBLE); }
"float"									{ return(FLOAT); }
"int"									{ return(INT); }
"long"									{ return(LONG); }
"short"									{ return(SHORT); }
"signed"								{ return(SIGNED); }
"void"									{ return(VOID); }

{L}({L}|{D})*							{ return(IDENTIFIER); }

0[xX]{H}+{IS}?							{ return(CONSTANT); }
0{D}+{IS}?								{ return(CONSTANT); }
{D}+{IS}?								{ return(CONSTANT); }
L?'(\\.|[^\\'])+'						{ return(CONSTANT); }

{D}+{E}{FS}?							{ return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?					{ return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?					{ return(CONSTANT); }

L?\"(\\.|[^\\"])*\"						{ return(STRING_LITERAL); }

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

[ \t\v\n\f]								;
.										{ /* ignore bad characters */ }

%%

int yywrap(void)
{
    return 1;
}