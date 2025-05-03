%{
    #include <stdio.h>
    #include <memory>

    char* yyerrormsg;
    int yylex(void);
    int yyerror(const char *s);
%}

%code requires {
    #include "ast/ast.hpp"
}

%token UINT128 VOID CHAR BYTE STRING FLOAT FLOAT32 FLOAT64 FLOAT128 BOOL ERROR 
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token CLASS PUBLIC PRIVATE INTERFACE ABSTRACT VIRTUAL OVERRIDE PROTECTED
%token INT INT8 INT16 INT32 INT64 INT128 UINT UINT8 UINT16 UINT32 UINT64
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token IMPORT TYPEDEF FUNCTION EXTERN STATIC VOLATILE REGISTER HASH 
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN SIZEOF 
%token XOR_ASSIGN OR_ASSIGN STRUCT UNION ENUM ELLIPSIS CONST

%union {
    int ival;
    double dval;
    char* sval;
    ASTNodePtr node;
}

%token <ival> INTEGER_CONSTANT
%token <dval> FLOAT_CONSTANT  
%token <sval> STRING_CONSTANT 
%token <sval> IDENTIFIER      

%type <node> primary_expression
%type <node> additive_expression
%type <node> multiplicative_expression
%type <node> relational_expression
%type <node> equality_expression
%type <node> cast_expression
%type <node> shift_expression
%type <node> and_expression
%type <node> exclusive_or_expression
%type <node> inclusive_or_expression
%type <node> logical_and_expression
%type <node> logical_or_expression

%define parse.error verbose
%start translation_unit
%%

primary_expression
    : IDENTIFIER                    { $$ = new Identifier($1); free($1); }
    | STRING_CONSTANT               { $$ = new StringLiteral($1); free($1); }
    | INTEGER_CONSTANT              { $$ = new IntegerLiteral($1); }
    | FLOAT_CONSTANT                { $$ = new FloatLiteral($1); }
    | '(' expression ')'
    ;

imported_symbol_access
    : IDENTIFIER ':' ':' IDENTIFIER
    | IDENTIFIER ':' ':' IDENTIFIER '(' ')'
    | IDENTIFIER ':' ':' IDENTIFIER '(' argument_expression_list ')'
    ;

postfix_expression
    : primary_expression
    | postfix_expression '[' expression ']'
    | postfix_expression '(' ')'
    | postfix_expression '(' argument_expression_list ')'
    | postfix_expression '.' IDENTIFIER
    | postfix_expression PTR_OP IDENTIFIER
    | imported_symbol_access
    | postfix_expression INC_OP
    | postfix_expression DEC_OP
    ;

argument_expression_list
    : assignment_expression
    | argument_expression_list ',' assignment_expression
    ;

unary_expression
    : postfix_expression
    | INC_OP unary_expression
    | DEC_OP unary_expression
    | unary_operator cast_expression
    | SIZEOF unary_expression
    | SIZEOF '(' type_specifier ')'
    ;

unary_operator
    : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | '!'
    ;

cast_expression
    : unary_expression
    | '(' type_specifier ')' cast_expression
    ;

multiplicative_expression
    : cast_expression                                       { $$ = $1; }
    | multiplicative_expression '*' cast_expression         { $$ = new BinaryExpression($1, BinaryExpression::Operator::Multiply, $3); }
    | multiplicative_expression '/' cast_expression         { $$ = new BinaryExpression($1, BinaryExpression::Operator::Divide, $3); }
    | multiplicative_expression '%' cast_expression         { $$ = new BinaryExpression($1, BinaryExpression::Operator::Remainder, $3); }
    ;

additive_expression
    : multiplicative_expression                             { $$ = $1; }
    | additive_expression '+' multiplicative_expression     { $$ = new BinaryExpression($1, BinaryExpression::Operator::Add, $3); }
    | additive_expression '-' multiplicative_expression     { $$ = new BinaryExpression($1, BinaryExpression::Operator::Subtract, $3); }
    ;

shift_expression
    : additive_expression                                   { $$ = $1; }
    | shift_expression LEFT_OP additive_expression          { $$ = new BinaryExpression($1, BinaryExpression::Operator::LeftShift, $3); }
    | shift_expression RIGHT_OP additive_expression         { $$ = new BinaryExpression($1, BinaryExpression::Operator::RightShift, $3); }
    ;

relational_expression
    : shift_expression                                      { $$ = $1; }
    | relational_expression '<' shift_expression            { $$ = new BinaryExpression($1, BinaryExpression::Operator::LessThan, $3); }
    | relational_expression '>' shift_expression            { $$ = new BinaryExpression($1, BinaryExpression::Operator::GreaterThan, $3); }
    | relational_expression LE_OP shift_expression          { $$ = new BinaryExpression($1, BinaryExpression::Operator::LessEqual, $3); }
    | relational_expression GE_OP shift_expression          { $$ = new BinaryExpression($1, BinaryExpression::Operator::GreaterEqual, $3); }
    ;

equality_expression
    : relational_expression                                 { $$ = $1; }
    | equality_expression EQ_OP relational_expression       { $$ = new BinaryExpression($1, BinaryExpression::Operator::Equal, $3); }    
    | equality_expression NE_OP relational_expression       { $$ = new BinaryExpression($1, BinaryExpression::Operator::NotEqual, $3); }
    ;

and_expression
    : equality_expression                                   { $$ = $1; }
    | and_expression '&' equality_expression                { $$ = new BinaryExpression($1, BinaryExpression::Operator::BitwiseAnd, $3); }
    ;

exclusive_or_expression
    : and_expression                                        { $$ = $1; }
    | exclusive_or_expression '^' and_expression            { $$ = new BinaryExpression($1, BinaryExpression::Operator::BitwiseXor, $3); }
    ;

inclusive_or_expression
    : exclusive_or_expression                               { $$ = $1; }
    | inclusive_or_expression '|' exclusive_or_expression   { $$ = new BinaryExpression($1, BinaryExpression::Operator::BitwiseOr, $3); }
    ;

logical_and_expression
    : inclusive_or_expression                               { $$ = $1; }
    | logical_and_expression AND_OP inclusive_or_expression { $$ = new BinaryExpression($1, BinaryExpression::Operator::LogicalAnd, $3); }
    ;

logical_or_expression
    : logical_and_expression                                { $$ = $1; }
    | logical_or_expression OR_OP logical_or_expression     { $$ = new BinaryExpression($1, BinaryExpression::Operator::LogicalOr, $3); }
    ;

conditional_expression
    : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
    ;

assignment_expression
    : conditional_expression
    | unary_expression assignment_operator assignment_expression
    ;

assignment_operator
    : '='
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | LEFT_ASSIGN
    | RIGHT_ASSIGN
    | AND_ASSIGN
    | XOR_ASSIGN
    | OR_ASSIGN
    ;

expression
    : assignment_expression
    | expression ',' assignment_expression
    ;

constant_expression
    : conditional_expression
    ;

declaration
    :  var_declaration 
    |  function_definition
    |  typedef_specifier
    |  declaration_specifiers 
    ;

declaration_specifiers
    : type_specifier
    | type_qualifier type_specifier
    | storage_class_specifier type_specifier
    | storage_class_specifier type_qualifier type_specifier
    | access_specifier type_specifier
    | access_specifier type_qualifier type_specifier
    | access_specifier storage_class_specifier type_specifier
    | access_specifier storage_class_specifier type_qualifier type_specifier
    ;

storage_class_specifier
    : EXTERN
    | STATIC
    | REGISTER
    ;

access_specifier_optional
    :
    | access_specifier
    ;

access_specifier
    : PUBLIC
    | PRIVATE
    | ABSTRACT
    | VIRTUAL
    | OVERRIDE
    | PROTECTED
    ;

primitive_type_specifier
    : INT 
    | INT8 
    | INT16 
    | INT32 
    | INT64 
    | INT128 
    | UINT 
    | UINT8 
    | UINT16 
    | UINT32 
    | UINT64
    | UINT128 
    | VOID 
    | CHAR 
    | BYTE 
    | STRING 
    | FLOAT 
    | FLOAT32 
    | FLOAT64 
    | FLOAT128 
    | BOOL
    | ERROR
    | IDENTIFIER
    | struct_or_union_specifier
    | enum_specifier
    ;

typedef_specifier
    : access_specifier typedef_declarator
    | typedef_declarator
    ;

typedef_declarator
    : TYPEDEF IDENTIFIER '=' type_specifier ';' 
    ;

import_specifier
    : IMPORT import_submodules_list ';'
    ;

import_submodules_list
    : IDENTIFIER
    | import_submodules_list ':' ':' IDENTIFIER
    ;

struct_or_union_specifier
    : struct_or_union IDENTIFIER '{' struct_declaration_list '}'
    | struct_or_union '{' struct_declaration_list '}'
    | struct_or_union IDENTIFIER
    ;

struct_or_union
    : STRUCT
    | UNION
    ;

struct_declaration_list
    : 
    | struct_declaration
    | struct_declaration_list struct_declaration
    ;

struct_declaration
    : access_specifier struct_field_declaration
    | storage_class_specifier struct_field_declaration
    | access_specifier storage_class_specifier struct_field_declaration
    | struct_field_declaration
    | access_specifier storage_class_specifier function_definition
    | access_specifier function_definition
    | function_definition
    ;

struct_field_declaration
    : IDENTIFIER declaration_specifiers ';'
    | IDENTIFIER declaration_specifiers '=' expression ';'
    ;

// REVIEW
specifier_qualifier_list
    : primitive_type_specifier specifier_qualifier_list
    | primitive_type_specifier
    | type_qualifier specifier_qualifier_list
    | type_qualifier
    ;

struct_init_specifier
    : IDENTIFIER '{' field_initializer_list_optional '}'
    ;

field_initializer_list_optional
    :
    | field_initializer_list    
    ;

field_initializer_list
    : struct_init_field
    | field_initializer_list ',' struct_init_field
    ;

struct_init_field
    : IDENTIFIER ':' assignment_expression
    ;

enum_specifier
    : ENUM '{' enumerator_list '}'
    | ENUM IDENTIFIER '{' enumerator_list '}'
    | ENUM IDENTIFIER
    ;

enumerator_list
    : 
    | enumerator
    | enumerator_list ',' enumerator
    ;

enumerator
    : IDENTIFIER
    | IDENTIFIER '=' constant_expression
    | IDENTIFIER '(' type_specifier_list ')'
    ;

type_qualifier
    : CONST
    | VOLATILE
    ;

declarator
    : pointer direct_declarator
    | address direct_declarator
    | direct_declarator
    ;

direct_declarator
    : IDENTIFIER
    | '(' declarator ')'
    | direct_declarator '[' constant_expression ']'
    | direct_declarator '[' ']'
    | direct_declarator '(' parameter_type_list ')'
    | direct_declarator '(' identifier_list ')'
    | direct_declarator '(' ')'
    ;

pointer
    : '*'
    | '*' type_qualifier_list
    | '*' pointer
    | '*' type_qualifier_list pointer
    ;

address
    : '&'
    | '&' type_qualifier_list
    | '&' pointer
    | '&' type_qualifier_list pointer
    ;

type_qualifier_list
    : type_qualifier
    | type_qualifier_list type_qualifier
    ;

parameter_type_list
    : parameter_list
    | parameter_list ',' ELLIPSIS
    ;

parameter_list
    : parameter_declaration
    | parameter_list ',' parameter_declaration
    ;

parameter_declaration
    : declaration_specifiers declarator
    | declaration_specifiers abstract_declarator
    | declaration_specifiers
    ;

identifier_list
    : IDENTIFIER
    | identifier_list ',' IDENTIFIER
    ;

type_specifier_list
    : type_specifier
    | type_specifier_list ',' type_specifier
    ;

type_specifier
    : specifier_qualifier_list
    | specifier_qualifier_list abstract_declarator
    ;

abstract_declarator
    : pointer
    | direct_abstract_declarator
    | pointer direct_abstract_declarator
    ;

direct_abstract_declarator
    : '(' abstract_declarator ')'
    | '[' ']'
    | '[' constant_expression ']'
    | direct_abstract_declarator '[' ']'
    | direct_abstract_declarator '[' constant_expression ']'
    | '(' ')'
    | '(' parameter_type_list ')'
    | direct_abstract_declarator '(' ')'
    | direct_abstract_declarator '(' parameter_type_list ')'
    ;

statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

labeled_statement
    : IDENTIFIER ':' statement
    | CASE constant_expression ':' statement
    | DEFAULT ':' statement
    ;

compound_statement
    : '{' '}'
    | '{' statement_list '}'
    | '{' declaration_list '}'  
    | '{' declaration_list statement_list '}'
    ;

declaration_list
    : declaration
    | declaration_list declaration
    ;

statement_list
    : statement
    | statement_list statement
    ;

expression_statement
    : ';'
    | expression ';'
    ;

selection_statement
    : IF '(' expression ')' statement
    | IF '(' expression ')' statement ELSE statement
    | SWITCH '(' expression ')' statement
    ;

iteration_statement
    : WHILE '(' expression ')' statement
    | DO statement WHILE '(' expression ')' ';'
    | FOR '(' expression_statement expression_statement ')' statement
    | FOR '(' expression_statement expression_statement expression ')' statement
    ;

jump_statement
    : GOTO IDENTIFIER ';'
    | CONTINUE ';'
    | BREAK ';'
    | RETURN ';'
    | RETURN expression ';'
    ;

translation_unit
    : /* empty */
    | import_specifier
    | external_declaration
    | translation_unit external_declaration
    ;

external_declaration
    : function_definition
    | declaration
    ;

function_definition
    : FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement
    | FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement
    ;

parameter_list_optional
    : /* empty */
    | parameter_list
    ;

var_declaration 
    : HASH IDENTIFIER ':' type_specifier ';'
    | HASH IDENTIFIER ':' type_specifier '=' assignment_expression ';'
    | HASH IDENTIFIER '=' assignment_expression ';'
    ;

%%

int yyerror(const char *msg)
{
    yyerrormsg = (char *) msg;
    return 1;
}
