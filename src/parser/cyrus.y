%{
    #include <stdio.h>
    #include <memory>
    #include "ast/ast.hpp"

    char* yyerrormsg;
    int yylex(void);
    int yyerror(const char *s);
    extern ASTNode* astProgram = nullptr;
%}

%code requires {
    #include "ast/ast.hpp"
}

%token UINT128 VOID CHAR BYTE STRING FLOAT FLOAT32 FLOAT64 FLOAT128 BOOL ERROR 
%token CLASS PUBLIC PRIVATE INTERFACE ABSTRACT VIRTUAL OVERRIDE PROTECTED
%token INT INT8 INT16 INT32 INT64 INT128 UINT UINT8 UINT16 UINT32 UINT64
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR CONTINUE BREAK RETURN
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token IMPORT TYPEDEF FUNCTION EXTERN STATIC VOLATILE REGISTER HASH 
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token XOR_ASSIGN OR_ASSIGN STRUCT ENUM ELLIPSIS CONST
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN 

%union {
    std::pair<std::vector<ASTStructField>, std::vector<ASTFunctionDefinition>>* structMembersAndMethods;
    ASTAssignmentExpression::Operator assignmentOperator;
    std::vector<ASTFunctionParameter>* paramsListPtr;
    ASTTypeSpecifier::ASTInternalType internalType;
    ASTStorageClassSpecifier storageClassSpecifier;
    ASTUnaryExpression::Operator unaryOperator;
    std::vector<std::string>* stringListPtr;
    ASTAccessSpecifier accessSpecifier;
    ASTTypeSpecifier* typeSpecifier;
    ASTFunctionDefinition* funcDef;
    ASTStructField* structField;
    ASTNodePtr node;
    double dval;
    char* sval;
    int ival;
}

%token <ival> INTEGER_CONSTANT
%token <dval> FLOAT_CONSTANT  
%token <sval> STRING_CONSTANT 
%token <sval> IDENTIFIER      

%type <assignmentOperator> assignment_operator
%type <structMembersAndMethods> struct_declaration_list
%type <structField> struct_field_declaration
%type <funcDef> struct_method_declaration
%type <internalType> type_qualifier
%type <internalType> pointer
%type <internalType> address
%type <storageClassSpecifier> storage_class_specifier
%type <accessSpecifier> access_specifier
%type <stringListPtr> import_submodules_list
%type <paramsListPtr> parameter_list_optional
%type <paramsListPtr> parameter_list
%type <typeSpecifier> primitive_type_specifier
%type <typeSpecifier> specifier_qualifier_list
%type <typeSpecifier> type_specifier
%type <unaryOperator> unary_operator

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
%type <node> translation_unit
%type <node> unary_expression
%type <node> expression
%type <node> import_specifier
%type <node> external_declaration
%type <node> variable_declaration
%type <node> function_definition
%type <node> declaration
%type <node> declaration_list
%type <node> declaration_specifiers
%type <node> assignment_expression
%type <node> postfix_expression
%type <node> compound_statement
%type <node> statement_list
%type <node> statement
%type <node> typedef_specifier
%type <node> struct_specifier
%type <node> conditional_expression
%type <node> expression_statement
%type <node> enum_specifier

%define parse.error verbose
%start translation_unit
%%

import_specifier
    : IMPORT import_submodules_list ';'                                         { $$ = new ASTImportStatement(*$2); delete $2; }
    ;

import_submodules_list
    : IDENTIFIER                                                                { $$ = new std::vector<std::string>(); $$->push_back($1); free($1); }
    | import_submodules_list ':' ':' IDENTIFIER                                 { if ($$) $$->push_back($4); free($4); }
    ;

primary_expression
    : IDENTIFIER                                                                { $$ = new ASTIdentifier($1); free($1); }
    | STRING_CONSTANT                                                           { $$ = new ASTStringLiteral($1); free($1); }
    | INTEGER_CONSTANT                                                          { $$ = new ASTIntegerLiteral($1); }
    | FLOAT_CONSTANT                                                            { $$ = new ASTFloatLiteral($1); }
    | '(' expression ')'                                                        { $$ = $2; }
    ;

imported_symbol_access
    : IDENTIFIER ':' ':' IDENTIFIER
    | IDENTIFIER ':' ':' IDENTIFIER '(' ')'
    | IDENTIFIER ':' ':' IDENTIFIER '(' argument_expression_list ')'
    ;

postfix_expression
    : primary_expression                                                        { $$ = $1; }
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
    : postfix_expression                                                            { $$ = $1; }
    | INC_OP unary_expression                                                       { $$ = new ASTUnaryExpression(ASTUnaryExpression::Operator::PreIncrement, $2); }
    | DEC_OP unary_expression                                                       { $$ = new ASTUnaryExpression(ASTUnaryExpression::Operator::PreDecrement, $2); }
    | unary_operator cast_expression                                                { $$ = new ASTUnaryExpression($1, $2); }
    ;

unary_operator
    : '&'                                                                           { $$ = ASTUnaryExpression::Operator::AddressOf; }
    | '*'                                                                           { $$ = ASTUnaryExpression::Operator::Dereference; }
    | '+'                                                                           { $$ = ASTUnaryExpression::Operator::Plus; }
    | '-'                                                                           { $$ = ASTUnaryExpression::Operator::Negate; }
    | '~'                                                                           { $$ = ASTUnaryExpression::Operator::BitwiseNot; }
    | '!'                                                                           { $$ = ASTUnaryExpression::Operator::LogicalNot; }
    ;

cast_expression
    : unary_expression                                                              { $$ = $1; }
    | '(' type_specifier ')' cast_expression                                        { $$ = new ASTCastExpression(*$2, $4); }
    ;

multiplicative_expression
    : cast_expression                                                               { $$ = $1; }
    | multiplicative_expression '*' cast_expression                                 { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::Multiply, $3); }
    | multiplicative_expression '/' cast_expression                                 { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::Divide, $3); }
    | multiplicative_expression '%' cast_expression                                 { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::Remainder, $3); }
    ;

additive_expression
    : multiplicative_expression                                                     { $$ = $1; }
    | additive_expression '+' multiplicative_expression                             { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::Add, $3); }
    | additive_expression '-' multiplicative_expression                             { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::Subtract, $3); }
    ;

shift_expression
    : additive_expression                                                           { $$ = $1; }
    | shift_expression LEFT_OP additive_expression                                  { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::LeftShift, $3); }
    | shift_expression RIGHT_OP additive_expression                                 { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::RightShift, $3); }
    ;

relational_expression
    : shift_expression                                                              { $$ = $1; }
    | relational_expression '<' shift_expression                                    { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::LessThan, $3); }
    | relational_expression '>' shift_expression                                    { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::GreaterThan, $3); }
    | relational_expression LE_OP shift_expression                                  { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::LessEqual, $3); }
    | relational_expression GE_OP shift_expression                                  { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::GreaterEqual, $3); }
    ;

equality_expression
    : relational_expression                                                         { $$ = $1; }
    | equality_expression EQ_OP relational_expression                               { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::Equal, $3); }    
    | equality_expression NE_OP relational_expression                               { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::NotEqual, $3); }
    ;

and_expression
    : equality_expression                                                           { $$ = $1; }
    | and_expression '&' equality_expression                                        { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::BitwiseAnd, $3); }
    ;

exclusive_or_expression
    : and_expression                                                                { $$ = $1; }
    | exclusive_or_expression '^' and_expression                                    { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::BitwiseXor, $3); }
    ;

inclusive_or_expression
    : exclusive_or_expression                                                       { $$ = $1; }
    | inclusive_or_expression '|' exclusive_or_expression                           { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::BitwiseOr, $3); }
    ;

logical_and_expression
    : inclusive_or_expression                                                       { $$ = $1; }
    | logical_and_expression AND_OP inclusive_or_expression                         { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::LogicalAnd, $3); }
    ;

logical_or_expression
    : logical_and_expression                                                        { $$ = $1; }
    | logical_or_expression OR_OP logical_or_expression                             { $$ = new ASTBinaryExpression($1, ASTBinaryExpression::Operator::LogicalOr, $3); }
    ;

conditional_expression
    : logical_or_expression                                                         { $$ = $1; }
    | logical_or_expression '?' expression ':' conditional_expression               { $$ = new ASTConditionalExpression($1, $3, $5); }
    ;

assignment_expression
    : conditional_expression                                                        { $$ = $1; }
    | unary_expression assignment_operator assignment_expression                    { $$ = new ASTAssignmentExpression($1, $2, $3); }
    ;

assignment_operator
    : '='                                                                           { $$ = ASTAssignmentExpression::Operator::Assign; }
    | MUL_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::MultiplyAssign; }
    | DIV_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::DivideAssign; }
    | MOD_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::RemainderAssign; }
    | ADD_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::AddAssign; }
    | SUB_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::SubtractAssign; }
    | LEFT_ASSIGN                                                                   { $$ = ASTAssignmentExpression::Operator::LeftShiftAssign; }
    | RIGHT_ASSIGN                                                                  { $$ = ASTAssignmentExpression::Operator::RightShiftAssign; }
    | AND_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::BitwiseAndAssign; }
    | XOR_ASSIGN                                                                    { $$ = ASTAssignmentExpression::Operator::BitwiseXorAssign; }
    | OR_ASSIGN                                                                     { $$ = ASTAssignmentExpression::Operator::BitwiseOrAssign; }
    ;

expression
    : assignment_expression                                                         { $$ = $1; }
    ;

constant_expression
    : conditional_expression
    ;

declaration
    : variable_declaration             { $$ = $1; }
    | function_definition              { $$ = $1; }
    | typedef_specifier                { $$ = $1; }
    | struct_specifier                 { $$ = $1; }
    | enum_specifier                   { $$ = $1; }
    | declaration_specifiers           { $$ = $1; }
    ;

// REVIEW
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
    : EXTERN                         { $$ = ASTStorageClassSpecifier::Extern; }
    | STATIC                         { $$ = ASTStorageClassSpecifier::Static; }
    | REGISTER                       { $$ = ASTStorageClassSpecifier::Register; }
    ;

access_specifier
    : PUBLIC                         { $$ = ASTAccessSpecifier::Public; }
    | PRIVATE                        { $$ = ASTAccessSpecifier::Private; }
    | ABSTRACT                       { $$ = ASTAccessSpecifier::Abstract; }
    | VIRTUAL                        { $$ = ASTAccessSpecifier::Virtual; }
    | OVERRIDE                       { $$ = ASTAccessSpecifier::Override; }
    | PROTECTED                      { $$ = ASTAccessSpecifier::Protected; }
    ;

primitive_type_specifier
    : INT                           { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int); }
    | INT8                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int8); }
    | INT16                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int16); }
    | INT32                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int32); }
    | INT64                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int64); }
    | INT128                        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Int128); }
    | UINT                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt); }
    | UINT8                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt8); }
    | UINT16                        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt16); }
    | UINT32                        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt32); }
    | UINT64                        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt64); }
    | UINT128                       { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::UInt128); }
    | VOID                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Void); }
    | CHAR                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Char); }
    | BYTE                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Byte); }
    | STRING                        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::String); }
    | FLOAT                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float); }
    | FLOAT32                       { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float32); }
    | FLOAT64                       { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float64); }
    | FLOAT128                      { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float128); }
    | BOOL                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Bool); }
    | ERROR                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Error); }
    | IDENTIFIER                    { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Identifier, (std::any) new ASTIdentifier($1)); }
    ;

typedef_specifier
    : access_specifier TYPEDEF IDENTIFIER '=' type_specifier ';'                { $$ = new ASTTypeDefStatement($3, *$5, $1); }
    | TYPEDEF IDENTIFIER '=' type_specifier ';'                                 { $$ = new ASTTypeDefStatement($2, *$4); }
    ;

struct_specifier
    : STRUCT IDENTIFIER '{' struct_declaration_list '}'                         { $$ = new ASTStructDefinition($2, $4->first, $4->second); }
    | STRUCT '{' struct_declaration_list '}'                                    { $$ = new ASTStructDefinition(std::nullopt, $3->first, $3->second); }
    | STRUCT IDENTIFIER '{'  '}'                                                { $$ = new ASTStructDefinition($2, {}, {}); }
    | STRUCT IDENTIFIER ';'                                                     { $$ = new ASTStructDefinition($2, {}, {}); }
    ;

struct_declaration_list
    :                                                                           { $$ = new std::pair<std::vector<ASTStructField>, std::vector<ASTFunctionDefinition>>(); }
    | struct_declaration_list struct_field_declaration                          {   
                                                                                    $$->first.push_back(*$2);
                                                                                }
    | struct_declaration_list struct_method_declaration                         {
                                                                                    $$->second.push_back(*$2);
                                                                                }
    ;

struct_field_declaration
    : access_specifier IDENTIFIER type_specifier ';'                            { $$ = new ASTStructField($2, *$3, $1); } 
    | IDENTIFIER type_specifier ';'                                             { $$ = new ASTStructField($1, *$2); }
    ;

struct_method_declaration
    : access_specifier function_definition                                      { $$ = static_cast<ASTFunctionDefinition *>($2); }
    | function_definition                                                       { $$ = static_cast<ASTFunctionDefinition *>($1); }
    ;

specifier_qualifier_list
    : primitive_type_specifier specifier_qualifier_list                         { $$ = new ASTTypeSpecifier($1->getTypeValue(), $2); }
    | type_qualifier specifier_qualifier_list                                   { $$ = new ASTTypeSpecifier($1, *$2); }
    | primitive_type_specifier                                                  { $$ = new ASTTypeSpecifier($1->getTypeValue()); }
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
    : CONST                                                             { $$ = ASTTypeSpecifier::ASTInternalType::Const; }
    | VOLATILE                                                          { $$ = ASTTypeSpecifier::ASTInternalType::Volatile; }
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

// FIXME
pointer
    : '*'                                                           
    | '*' type_qualifier_list                                           { $$ = ASTTypeSpecifier::ASTInternalType::Pointer; }    
    | '*' pointer
    | '*' type_qualifier_list pointer
    ;

// FIXME
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
    : compound_statement        
    | expression_statement      
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

compound_statement                                              
    : '{' '}'                                               { $$ = new ASTStatementList(); }
    | '{' statement_list '}'                                { $$ = $2; }
    | '{' declaration_list '}'                              { $$ = $2; }
    | '{' expression_statement '}'                          { $$ = new ASTStatementList($2); }
    | '{' declaration_list statement_list '}'               { 
                                                                ASTStatementList* list = static_cast<ASTStatementList*>($2);
                                                                $$ = list;
                                                            }
    | '{' declaration_list expression_statement '}'         {
                                                                ASTStatementList* list = static_cast<ASTStatementList*>($2);
                                                                list->addStatement($3);
                                                                $$ = list;
                                                            }
    ;

declaration_list
    : declaration                                           { $$ = new ASTStatementList($1); }
    | declaration_list declaration                          {
                                                                if ($$) 
                                                                {
                                                                    ASTStatementList* list = static_cast<ASTStatementList*>($$);
                                                                    list->addStatement($2);
                                                                } 
                                                                else 
                                                                {
                                                                    $$ = new ASTStatementList($2);
                                                                }
                                                            }
    ;

statement_list  
    : statement                                         { $$ = new ASTStatementList($1); }
    | statement_list statement                          { 
                                                            if ($$) 
                                                            {
                                                                ASTStatementList* list = static_cast<ASTStatementList*>($$);
                                                                list->addStatement($2);
                                                            } 
                                                            else 
                                                            {
                                                                $$ = new ASTStatementList($2);
                                                            }
                                                        }
    ;

expression_statement
    : ';'
    | expression ';'                                    { $$ = $1; }
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
    : CONTINUE ';'
    | BREAK ';'
    | RETURN ';'
    | RETURN expression ';'
    ;

translation_unit
    : /* empty */                                   { astProgram = new ASTProgram(ASTNodeList {}); }
    | import_specifier                              { 
                                                        if (astProgram) 
                                                        {
                                                            ASTProgram* program = static_cast<ASTProgram*>(astProgram);
                                                            program->addStatement($1);
                                                        } 
                                                        else 
                                                        {
                                                            astProgram = new ASTProgram(ASTNodeList { $1 });
                                                        }
                                                    }
    | external_declaration                          { 
                                                        if (astProgram) 
                                                        {
                                                            ASTProgram* program = static_cast<ASTProgram*>(astProgram);
                                                            program->addStatement($1);
                                                        } 
                                                        else 
                                                        {
                                                            astProgram = new ASTProgram(ASTNodeList { $1 });
                                                        }
                                                    }
    | translation_unit external_declaration         { 
                                                        if (astProgram) 
                                                        {
                                                            ASTProgram* program = static_cast<ASTProgram*>(astProgram);
                                                            program->addStatement($2);
                                                        } 
                                                        else 
                                                        {
                                                            astProgram = new ASTProgram(ASTNodeList { $2 });
                                                        }
                                                    }
    ;

external_declaration                                
    : function_definition                                                   { $$ = $1; }    
    | declaration                                                           { $$ = $1; }
    ;

function_definition
    : FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement     { $$ = new ASTFunctionDefinition($2, *$4, $6, $7); }
    | FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement                    { $$ = new ASTFunctionDefinition($2, *$4, nullptr, $6); }
    ;

parameter_list_optional
    : /* empty */                                                           { $$ = new std::vector<ASTFunctionParameter>(); }
    | parameter_list                                                        { $$ = $1; }
    ;

variable_declaration 
    : HASH IDENTIFIER ':' type_specifier ';'                                { $$ = new ASTVariableDeclaration($2, $4); }
    | HASH IDENTIFIER '=' assignment_expression ';'                         { $$ = new ASTVariableDeclaration($2, nullptr, $4); }
    | HASH IDENTIFIER ':' type_specifier '=' assignment_expression ';'      { $$ = new ASTVariableDeclaration($2, $4, $6); }
    ;

%%

int yyerror(const char *msg)
{
    yyerrormsg = (char *) msg;
    return 1;
}
