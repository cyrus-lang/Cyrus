%option yylineno

%{
	#include <stdio.h>
	#include "cyrus.tab.hpp"
	#include <math.h>
%}
%%
"#include"([ ]+)?((<(\\.|[^>])+>)|(\"(\\.|[^"])+\"))					                                    {return HEADER;}
"#define"[ ]+[a-zA-z_][a-zA-z_0-9]* 																	    {return DEFINE;}
"auto"|"register"|"static"|"extern"|"typedef"																{return storage_const;}
"void"|"char"|"short"|"int"|"long"|"float"|"double"|"signed"|"unsigned"										{return type_const;}	
"const"|"volatile"																							{return qual_const;}
"enum"																										{return enum_const;}
"case"																										{return CASE;}	
"default"																									{return DEFAULT;}
"if"																										{return IF;}
"switch"																									{return SWITCH;}
"else"																										{return ELSE;}
"for"																										{return FOR;}
"do"																										{return DO;}
"while"																										{return WHILE;}
"goto"																										{return GOTO;}
"continue"																									{return CONTINUE;}
"break"																										{return BREAK;}
"struct"|"union"																							{return struct_const;}
"return"																									{return RETURN;}
"sizeof"																									{return SIZEOF;}
"||" 																										{return or_const;}
"&&"																										{return and_const;}
"..."																										{return param_const;}
"=="|"!="																									{return eq_const;}
"<="|">="																									{return rel_const;}
">>"|"<<"																									{return shift_const;}
"++"|"--"																									{return inc_const;}
"->"																										{return point_const;}
";"|"="|","|"{"|"}"|"("|")"|"["|"]"|"*"|"+"|"-"|"/"|"?"|":"|"&"|"|"|"^"|"!"|"~"|"%"|"<"|">"					{return yytext[0];}
"*="|"/="|"+="|"%="|">>="|"-="|"<<="|"&="|"^="|"|="															{return PUNC;}
[0-9]+																									    {return int_const;}
[0-9]+"."[0-9]+																						        {return float_const;}	
"'"."'"																										{return char_const;}
[a-zA-z_][a-zA-z_0-9]*			 																			{return id;}
\"(\\.|[^\"])*\"																							{return string;}
"//"(\\.|[^\n])*[\n]																						;
[/][*]([^*]|[*]*[^*/])*[*]+[/]                                                                              ;
[ \t\n]																										;
%%

int yywrap(void)
{
	printf("hello world");
    return 1;
}