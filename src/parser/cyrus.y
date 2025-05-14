%{
    #include <variant>
    #include "ast/ast.hpp"

    char* yyerrormsg;
    int yylex(void);
    int yyerror(const char *s);
    extern ASTNode* astProgram = nullptr;
%}

%code requires {
    #include "ast/ast.hpp"

    using EnumData = std::variant<ASTEnumVariant, std::pair<std::string, std::optional<ASTNodePtr>>, ASTFunctionDefinition>;
}

%token IMPORT TYPEDEF FUNCTION EXTERN INLINE HASH 
%token CLASS PUBLIC PRIVATE INTERFACE ABSTRACT VIRTUAL OVERRIDE PROTECTED
%token UINT128 VOID CHAR BYTE STRING FLOAT32 FLOAT64 FLOAT128 BOOL ERROR 
%token INT INT8 INT16 INT32 INT64 INT128 UINT UINT8 UINT16 UINT32 UINT64
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR CONTINUE BREAK RETURN
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token XOR_ASSIGN OR_ASSIGN STRUCT ENUM ELLIPSIS CONST
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN 

%union {
    std::pair<std::vector<ASTStructField>, std::vector<ASTFunctionDefinition>>* structMembersAndMethods;
    std::vector<std::pair<std::string, ASTNodePtr>>* fieldInitializerList;
    std::pair<std::string, ASTNodePtr>* structFieldInitPair;
    std::vector<ASTEnumVariantItem>* enumVariantItemsList;
    ASTAssignment::Operator assignmentOperator;
    ASTFunctionParameters* paramsListPtr;
    ASTStorageClassSpecifier storageClassSpecifier;
    ASTUnaryExpression::Operator unaryOperator;
    std::vector<std::string>* stringListPtr;
    std::vector<ASTNodePtr>* nodeListPtr;
    std::vector<EnumData>* enumDataList;
    ASTEnumVariantItem* enumVariantItem;
    ASTAccessSpecifier accessSpecifier;
    ASTTypeSpecifier* typeSpecifier;
    ASTFunctionDefinition* funcDef;
    ASTStructField* structField;
    EnumData* enumData;
    ASTNodePtr node;
    float fval;
    double dval;
    char* sval;
    int ival;
}

%token <ival> INTEGER_CONSTANT
%token <fval> FLOAT_CONSTANT  
%token <dval> DOUBLE_CONSTANT  
%token <sval> STRING_CONSTANT 
%token <sval> IDENTIFIER      

%type <structMembersAndMethods> struct_declaration_list
%type <storageClassSpecifier> storage_class_specifier
%type <enumVariantItemsList> enum_variant_items_list
%type <fieldInitializerList> field_initializer_list
%type <typeSpecifier> qualified_type_specifier
%type <assignmentOperator> assignment_operator
%type <typeSpecifier> primitive_type_specifier
%type <paramsListPtr> parameter_list_optional
%type <structFieldInitPair> struct_init_field
%type <stringListPtr> import_submodules_list
%type <nodeListPtr> argument_expression_list
%type <structField> struct_field_declaration
%type <funcDef> struct_method_declaration
%type <enumVariantItem> enum_variant_item
%type <accessSpecifier> access_specifier
%type <typeSpecifier> pointer_type
%type <typeSpecifier> address_type
%type <typeSpecifier> base_type
%type <paramsListPtr> parameter_list
%type <typeSpecifier> type_specifier
%type <unaryOperator> unary_operator
%type <enumDataList> enumerator_list
%type <enumData> enumerator

%type <node> global_variable_declaration
%type <node> iteration_statement
%type <node> parameter_declaration
%type <node> constant_expression
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
%type <node> function_declaration
%type <node> declaration
%type <node> declaration_list
%type <node> assignment_expression
%type <node> postfix_expression
%type <node> compound_statement
%type <node> statement_list
%type <node> statement
%type <node> typedef_specifier
%type <node> struct_specifier
%type <node> struct_init_specifier
%type <node> conditional_expression
%type <node> expression_statement
%type <node> enum_specifier
%type <node> imported_symbol_access
%type <node> jump_statement
%type <node> selection_statement

%define parse.error verbose
%start translation_unit
%%

import_specifier
    : IMPORT import_submodules_list ';'                                         { $$ = new ASTImportStatement(*$2); delete $2; }
    ;

import_submodules_list
    : IDENTIFIER                                                                { $$ = new std::vector<std::string>(); $$->push_back($1); free($1);  }
    | import_submodules_list ':' ':' IDENTIFIER                                 { if ($$) $$->push_back($4); free($4); }
    ;

primary_expression
    : IDENTIFIER                                                                { $$ = new ASTIdentifier($1); free($1); }
    | STRING_CONSTANT                                                           { $$ = new ASTStringLiteral($1); free($1); }
    | INTEGER_CONSTANT                                                          { $$ = new ASTIntegerLiteral($1); }
    | FLOAT_CONSTANT                                                            { $$ = new ASTFloatLiteral($1); }
    | DOUBLE_CONSTANT                                                           { $$ = new ASTFloatLiteral($1); }
    | '(' expression ')'                                                        { $$ = $2; }
    ;

imported_symbol_access
    : import_submodules_list                                                    {
                                                                                    $$ = new ASTImportedSymbolAccess(*$1);
                                                                                    delete $1;
                                                                                }
    | import_submodules_list '(' ')'                                            { 
                                                                                    $$ = new ASTFunctionCall(new ASTImportedSymbolAccess(*$1), {});
                                                                                    delete $1;
                                                                                }
    | import_submodules_list '(' argument_expression_list ')'                   { 
                                                                                    $$ = new ASTFunctionCall(new ASTImportedSymbolAccess(*$1), *$3);
                                                                                    delete $1;
                                                                                    delete $3;
                                                                                }
    ;

postfix_expression
    : primary_expression                                                        { $$ = $1; }
    | postfix_expression '[' expression ']'                                     // TODO Array Index Access
    | postfix_expression '(' ')'                                                { $$ = new ASTFunctionCall($1, {}); }
    | postfix_expression '(' argument_expression_list ')'                       {
                                                                                    $$ = new ASTFunctionCall($1, *$3);
                                                                                    delete $3;
                                                                                }
    | postfix_expression '.' IDENTIFIER                                         { $$ = new ASTFieldAccess($1, $3); free($3); }
    | postfix_expression PTR_OP IDENTIFIER                                      {
                                                                                    ASTFieldAccess fieldAccess($1, $3);
                                                                                    $$ = new ASTPointerFieldAccess(fieldAccess);
                                                                                    free($3);
                                                                                }
    | postfix_expression INC_OP                                                 { $$ = new ASTUnaryExpression(ASTUnaryExpression::Operator::PostIncrement, $1); }
    | postfix_expression DEC_OP                                                 { $$ = new ASTUnaryExpression(ASTUnaryExpression::Operator::PostDecrement, $1); }
    | imported_symbol_access                                                    { $$ = $1; }
    | struct_init_specifier                                                     { $$ = $1; }
    ;

argument_expression_list
    : assignment_expression                                                     { $$ = new std::vector<ASTNodePtr>({$1}); }
    | argument_expression_list ',' assignment_expression                        { $$->push_back($3); }
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
    | '(' type_specifier ')' cast_expression                                        { 
                                                                                        $$ = new ASTCastExpression(*$2, $4);
                                                                                        delete $2;
                                                                                    }
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
    | unary_expression assignment_operator assignment_expression                    { $$ = new ASTAssignment($1, $2, $3); }
    ;

assignment_operator
    : '='                                                                           { $$ = ASTAssignment::Operator::Assign; }
    | MUL_ASSIGN                                                                    { $$ = ASTAssignment::Operator::MultiplyAssign; }
    | DIV_ASSIGN                                                                    { $$ = ASTAssignment::Operator::DivideAssign; }
    | MOD_ASSIGN                                                                    { $$ = ASTAssignment::Operator::RemainderAssign; }
    | ADD_ASSIGN                                                                    { $$ = ASTAssignment::Operator::AddAssign; }
    | SUB_ASSIGN                                                                    { $$ = ASTAssignment::Operator::SubtractAssign; }
    | LEFT_ASSIGN                                                                   { $$ = ASTAssignment::Operator::LeftShiftAssign; }
    | RIGHT_ASSIGN                                                                  { $$ = ASTAssignment::Operator::RightShiftAssign; }
    | AND_ASSIGN                                                                    { $$ = ASTAssignment::Operator::BitwiseAndAssign; }
    | XOR_ASSIGN                                                                    { $$ = ASTAssignment::Operator::BitwiseXorAssign; }
    | OR_ASSIGN                                                                     { $$ = ASTAssignment::Operator::BitwiseOrAssign; }
    ;

expression
    : assignment_expression                                                         { $$ = $1; }
    ;

constant_expression
    : conditional_expression                                                        { $$ = $1; }
    ;

declaration
    : variable_declaration             { $$ = $1; }
    | function_definition              { $$ = $1; }
    | typedef_specifier                { $$ = $1; }
    | struct_specifier                 { $$ = $1; }
    | enum_specifier                   { $$ = $1; }
    ;

storage_class_specifier
    : EXTERN                         { $$ = ASTStorageClassSpecifier::Extern; }
    | INLINE                         { $$ = ASTStorageClassSpecifier::Inline; }
    ;

access_specifier
    : PUBLIC                         { $$ = ASTAccessSpecifier::Public; }
    | PRIVATE                        { $$ = ASTAccessSpecifier::Private; }
    | ABSTRACT                       { $$ = ASTAccessSpecifier::Abstract; }
    | VIRTUAL                        { $$ = ASTAccessSpecifier::Virtual; }
    | OVERRIDE                       { $$ = ASTAccessSpecifier::Override; }
    | PROTECTED                      { $$ = ASTAccessSpecifier::Protected; }
    ;

typedef_specifier
    : access_specifier TYPEDEF IDENTIFIER '=' type_specifier ';'                { $$ = new ASTTypeDefStatement($3, *$5, $1); free($3); delete $5; }
    | TYPEDEF IDENTIFIER '=' type_specifier ';'                                 { $$ = new ASTTypeDefStatement($2, *$4); free($2); delete $4; }
    ;

struct_specifier
    : STRUCT IDENTIFIER '{' struct_declaration_list '}'                         { $$ = new ASTStructDefinition($2, $4->first, $4->second); free($2); }
    | STRUCT '{' struct_declaration_list '}'                                    { $$ = new ASTStructDefinition(std::nullopt, $3->first, $3->second); }
    | STRUCT IDENTIFIER '{'  '}'                                                { $$ = new ASTStructDefinition($2, {}, {}); free($2); }
    | STRUCT IDENTIFIER ';'                                                     { $$ = new ASTStructDefinition($2, {}, {}); free($2); }
    ;

struct_declaration_list
    :                                                                           { $$ = new std::pair<std::vector<ASTStructField>, std::vector<ASTFunctionDefinition>>(); }
    | struct_declaration_list struct_field_declaration                          {   
                                                                                    $$->first.push_back(*$2);
                                                                                    delete $2;
                                                                                }
    | struct_declaration_list struct_method_declaration                         {
                                                                                    $$->second.push_back(*$2);
                                                                                    delete $2;
                                                                                }
    ;

struct_field_declaration
    : access_specifier IDENTIFIER type_specifier ';'                            { $$ = new ASTStructField($2, *$3, $1); free($2); delete $3; } 
    | IDENTIFIER type_specifier ';'                                             { $$ = new ASTStructField($1, *$2); free($1); delete $2; }
    ;

struct_method_declaration
    : access_specifier function_definition                                      { $$ = static_cast<ASTFunctionDefinition *>($2); }
    | function_definition                                                       { $$ = static_cast<ASTFunctionDefinition *>($1); }
    ;

struct_init_specifier
    : IDENTIFIER '{' '}'                                                        { $$ = new ASTStructInitialization($1, {}); free($1); }
    | IDENTIFIER '{' field_initializer_list '}'                                 { $$ = new ASTStructInitialization($1, *$3); free($1); delete $3; }
    ;

field_initializer_list
    : struct_init_field                                                         { $$ = new std::vector<std::pair<std::string, ASTNodePtr>>{*$1}; delete $1;  }
    | field_initializer_list ';' struct_init_field                              { $$->push_back(*$3); delete $3; }
    ;

struct_init_field
    : IDENTIFIER ':' assignment_expression                                      { $$ = new std::pair<std::string, ASTNodePtr>($1, $3); free($1); }
    ;

enum_specifier
    : ENUM '{' enumerator_list '}'                                              { 
                                                                                    std::vector<ASTEnumVariant> variants;
                                                                                    std::vector<ASTFunctionDefinition> methods;
                                                                                    std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> fields;

                                                                                    for (auto& data : *$3) {
                                                                                        if (std::holds_alternative<ASTEnumVariant>(data)) {
                                                                                            variants.push_back(std::get<ASTEnumVariant>(data));
                                                                                        } else if (std::holds_alternative<ASTFunctionDefinition>(data)) {
                                                                                            methods.push_back(std::get<ASTFunctionDefinition>(data));
                                                                                        } 
                                                                                        else if (std::holds_alternative<std::pair<std::string, std::optional<ASTNodePtr>>>(data)) {
                                                                                            auto pair = std::get<std::pair<std::string, std::optional<ASTNodePtr>>>(data);
                                                                                            fields.push_back(pair);
                                                                                        }
                                                                                    }

                                                                                    $$ = new ASTEnumDefinition(std::nullopt, variants, fields, methods); 
                                                                                    delete $3;
                                                                                }
    | ENUM IDENTIFIER '{' enumerator_list '}'                                   {
                                                                                    std::vector<ASTEnumVariant> variants;
                                                                                    std::vector<ASTFunctionDefinition> methods;
                                                                                    std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> fields;

                                                                                    for (auto& data : *$4) {
                                                                                        if (std::holds_alternative<ASTEnumVariant>(data)) {
                                                                                            variants.push_back(std::get<ASTEnumVariant>(data));
                                                                                        } else if (std::holds_alternative<ASTFunctionDefinition>(data)) {
                                                                                            methods.push_back(std::get<ASTFunctionDefinition>(data));
                                                                                        }
                                                                                        else if (std::holds_alternative<std::pair<std::string, std::optional<ASTNodePtr>>>(data)) {
                                                                                            auto pair = std::get<std::pair<std::string, std::optional<ASTNodePtr>>>(data);
                                                                                            fields.push_back(pair);
                                                                                        }
                                                                                    }

                                                                                    $$ = new ASTEnumDefinition($2, variants, fields, methods); 
                                                                                    free($2);
                                                                                    delete $4;
                                                                                }
    | ENUM IDENTIFIER ';'                                                       { $$ = new ASTEnumDefinition($2, {}, {}, {}); free($2); }
    ;

enumerator_list     
    : enumerator                                                                { $$ = new std::vector<EnumData>{ *$1 }; delete $1; }
    | enumerator_list ',' enumerator                                            { $$->push_back(*$3); delete $3; }
    | enumerator_list ';'   
    | enumerator_list function_definition                                       {   
                                                                                    auto method = static_cast<ASTFunctionDefinition *>($2);
                                                                                    $$->push_back(*method);
                                                                                    delete $2;
                                                                                }                                                    
    ;

enumerator
    : IDENTIFIER                                            { 
                                                                auto unnamedField = new std::pair<std::string, std::optional<ASTNodePtr>>($1, std::nullopt);
                                                                $$ = new EnumData(*unnamedField);
                                                                free($1);
                                                            }
    | IDENTIFIER '=' constant_expression                    { 
                                                                auto field = std::pair<std::string, std::optional<ASTNodePtr>>{$1, $3};
                                                                $$ = new EnumData(field);
                                                                free($1);
                                                            }
    | IDENTIFIER '(' enum_variant_items_list ')'            {
                                                                ASTEnumVariant enumVariant($1, *$3);
                                                                $$ = new EnumData(enumVariant);
                                                                free($1);
                                                                delete $3;
                                                            }
    ;

enum_variant_item
    : type_specifier                                        { $$ = new ASTEnumVariantItem(std::nullopt, *$1); delete $1; }
    | IDENTIFIER type_specifier                             { $$ = new ASTEnumVariantItem($1, *$2); free($1); delete $2; }
    ;   

enum_variant_items_list
    : enum_variant_item                                         {   
                                                                    $$ = new std::vector<ASTEnumVariantItem>{ *$1 };
                                                                    delete $1;
                                                                }
    | enum_variant_items_list ',' enum_variant_item             { 
                                                                    $$->push_back(*$3);
                                                                    delete $3;
                                                                }
    ;

type_specifier
    : qualified_type_specifier
    | pointer_type
    | address_type
    ;

base_type
    : primitive_type_specifier
    | IDENTIFIER                                        {   
                                                            $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Identifier, new ASTIdentifier($1)); 
                                                            free($1);
                                                        }
    ;

qualified_type_specifier
    : CONST base_type                                   { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Const, $2); }
    | base_type                                         { $$ = $1; }
    ;

pointer_type
    : type_specifier '*'
        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Pointer, $1); }
    ;

address_type
    : type_specifier '&'
        { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Reference, $1); }
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
    | FLOAT32                       { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float32); }
    | FLOAT64                       { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float64); }
    | FLOAT128                      { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Float128); }
    | BOOL                          { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Bool); }
    | ERROR                         { $$ = new ASTTypeSpecifier(ASTTypeSpecifier::ASTInternalType::Error); }
    ;
    
parameter_list
    : parameter_declaration                                     {   
                                                                    ASTFunctionParameter* param = static_cast<ASTFunctionParameter*>($1);
                                                                    $$ = new ASTFunctionParameters({ *param });
                                                                    delete $1;
                                                                }
    | parameter_list ',' parameter_declaration                  { 
                                                                    ASTFunctionParameter* param = static_cast<ASTFunctionParameter*>($3);
                                                                    $$->addParameter(*param); 
                                                                    delete $3;
                                                                }
    | parameter_list ',' type_specifier ELLIPSIS                {
                                                                    $$->setVariadic($3);
                                                                }
    | parameter_list ',' ELLIPSIS                               {
                                                                    $$->setVariadic(std::nullopt);
                                                                }
    ;

parameter_declaration
    : IDENTIFIER type_specifier                                  { $$ = new ASTFunctionParameter($1, *$2); free($1); delete $2; }
    | IDENTIFIER type_specifier '=' assignment_expression        { $$ = new ASTFunctionParameter($1, *$2, $4); free($1); delete $2; }
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
    : IF '(' expression ')' statement                                               { $$ = new ASTIfStatement($3, $5); }
    | IF '(' expression ')' statement ELSE statement                                { $$ = new ASTIfStatement($3, $5, $7); }
    | SWITCH '(' expression ')' statement
    ;

iteration_statement
    : FOR '(' expression ')' statement                                              { $$ = new ASTForStatement(std::nullopt, $3, std::nullopt, $5); }
    | FOR '(' variable_declaration expression_statement ')' statement               { $$ = new ASTForStatement($3, $4, std::nullopt, $6); }
    | FOR '(' variable_declaration expression_statement expression ')' statement    { $$ = new ASTForStatement($3, $4, $5, $7); }
    ;

jump_statement
    : CONTINUE ';'                                  { $$ = new ASTContinueStatement(); }
    | BREAK ';'                                     { $$ = new ASTBreakStatement(); }
    | RETURN ';'                                    { $$ = new ASTReturnStatement(std::nullopt); }
    | RETURN expression ';'                         { $$ = new ASTReturnStatement($2); }
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
    | function_declaration                                                  { $$ = $1; }    
    | global_variable_declaration                                           { $$ = $1; }
    ;

function_definition
    : storage_class_specifier access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement                    { $$ = new ASTFunctionDefinition(new ASTIdentifier($4), *$6, nullptr, $8, $2, $1); free($4); delete $6; }
    | storage_class_specifier access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement     { $$ = new ASTFunctionDefinition(new ASTIdentifier($4), *$6, $8, $9, $2, $1); free($4); delete $6; }
    | access_specifier storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement                    { $$ = new ASTFunctionDefinition(new ASTIdentifier($4), *$6, nullptr, $8, $1, $2); free($4); delete $6; }
    | access_specifier storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement     { $$ = new ASTFunctionDefinition(new ASTIdentifier($4), *$6, $8, $9, $1, $2); free($4); delete $6; }
    | access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement                                           { $$ = new ASTFunctionDefinition(new ASTIdentifier($3), *$5, nullptr, $7, $1); free($3); delete $5; }
    | access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement                            { $$ = new ASTFunctionDefinition(new ASTIdentifier($3), *$5, $7, $8, $1); free($3); delete $5; }
    | storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement                    { $$ = new ASTFunctionDefinition(new ASTIdentifier($3), *$5, nullptr, $7, ASTAccessSpecifier::Default, $1); free($3); delete $5; }
    | storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement     { $$ = new ASTFunctionDefinition(new ASTIdentifier($3), *$5, $7, $8, ASTAccessSpecifier::Default, $1); free($3); delete $5; }
    | FUNCTION IDENTIFIER '(' parameter_list_optional ')' compound_statement                                            { $$ = new ASTFunctionDefinition(new ASTIdentifier($2), *$4, nullptr, $6); free($2); delete $4; }
    | FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier compound_statement                             { $$ = new ASTFunctionDefinition(new ASTIdentifier($2), *$4, $6, $7); free($2); delete $4; }
    ;

function_declaration
    : storage_class_specifier access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' ';'                    { $$ = new ASTFunctionDeclaration(new ASTIdentifier($4), *$6, nullptr, $2, $1); free($4); delete $6; }
    | storage_class_specifier access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier ';'     { $$ = new ASTFunctionDeclaration(new ASTIdentifier($4), *$6, $8, $2, $1); free($4); delete $6; }
    | access_specifier storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' ';'                    { $$ = new ASTFunctionDeclaration(new ASTIdentifier($4), *$6, nullptr, $1, $2); free($4); delete $6; }
    | access_specifier storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier ';'     { $$ = new ASTFunctionDeclaration(new ASTIdentifier($4), *$6, $8, $1, $2); free($4); delete $6; }
    | access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' ';'                                            { $$ = new ASTFunctionDeclaration(new ASTIdentifier($3), *$5, nullptr, $1); free($3); delete $5; }
    | access_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier ';'                             { $$ = new ASTFunctionDeclaration(new ASTIdentifier($3), *$5, $7,      $1); free($3); delete $5; }
    | storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' ';'                    { $$ = new ASTFunctionDeclaration(new ASTIdentifier($3), *$5, nullptr, ASTAccessSpecifier::Default, $1); free($3); delete $5; }
    | storage_class_specifier FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier ';'     { $$ = new ASTFunctionDeclaration(new ASTIdentifier($3), *$5, $7, ASTAccessSpecifier::Default, $1); free($3); delete $5; }
    | FUNCTION IDENTIFIER '(' parameter_list_optional ')' ';'                                            { $$ = new ASTFunctionDeclaration(new ASTIdentifier($2), *$4, nullptr); free($2); delete $4; }
    | FUNCTION IDENTIFIER '(' parameter_list_optional ')' type_specifier ';'                             { $$ = new ASTFunctionDeclaration(new ASTIdentifier($2), *$4, $6); free($2); delete $4; }
    ;

parameter_list_optional
    : /* empty */                                                           { $$ = new ASTFunctionParameters({}); }
    | parameter_list                                                        { $$ = $1; }
    ;

variable_declaration 
    : HASH IDENTIFIER ':' type_specifier ';'                                { $$ = new ASTVariableDeclaration($2, $4); free($2); }
    | HASH IDENTIFIER '=' assignment_expression ';'                         { $$ = new ASTVariableDeclaration($2, nullptr, $4); free($2); }
    | HASH IDENTIFIER ':' type_specifier '=' assignment_expression ';'      { $$ = new ASTVariableDeclaration($2, $4, $6); free($2); }
    ;

global_variable_declaration 
    : IDENTIFIER ':' type_specifier ';'                                                                         { $$ = new ASTGlobalVariableDeclaration($1, $3); free($1); }
    | IDENTIFIER '=' assignment_expression ';'                                                                  { $$ = new ASTGlobalVariableDeclaration($1, nullptr, $3); free($1); }
    | IDENTIFIER ':' type_specifier '=' assignment_expression ';'                                               { $$ = new ASTGlobalVariableDeclaration($1, $3, $5); free($1); }
    | access_specifier IDENTIFIER ':' type_specifier ';'                                                        { $$ = new ASTGlobalVariableDeclaration($2, $4); free($2); }
    | access_specifier IDENTIFIER '=' assignment_expression ';'                                                 { $$ = new ASTGlobalVariableDeclaration($2, nullptr, $4); free($2); }
    | access_specifier IDENTIFIER ':' type_specifier '=' assignment_expression ';'                              { $$ = new ASTGlobalVariableDeclaration($2, $4, $6); free($2); }
    | access_specifier storage_class_specifier IDENTIFIER ':' type_specifier ';'                                { $$ = new ASTGlobalVariableDeclaration($3, $5, nullptr, $1, $2); free($3); }
    | access_specifier storage_class_specifier IDENTIFIER '=' assignment_expression ';'                         { $$ = new ASTGlobalVariableDeclaration($3, nullptr, $5, $1, $2); free($3); }
    | access_specifier storage_class_specifier IDENTIFIER ':' type_specifier '=' assignment_expression ';'      { $$ = new ASTGlobalVariableDeclaration($3, $5, $7, $1, $2); free($3); }
    | storage_class_specifier IDENTIFIER ':' type_specifier ';'                                                 { $$ = new ASTGlobalVariableDeclaration($2, $4, nullptr, ASTAccessSpecifier::Default, $1); free($2); }
    | storage_class_specifier IDENTIFIER '=' assignment_expression ';'                                          { $$ = new ASTGlobalVariableDeclaration($2, nullptr, $4, ASTAccessSpecifier::Default, $1); free($2); }
    | storage_class_specifier IDENTIFIER ':' type_specifier '=' assignment_expression ';'                       { $$ = new ASTGlobalVariableDeclaration($2, $4, $6, ASTAccessSpecifier::Default, $1); free($2); }
    ;                   

%%

int yyerror(const char *msg)
{
    yyerrormsg = (char *) msg;
    return 1;
}
