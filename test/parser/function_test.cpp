// #include "ast/ast.hpp"
// #include "parser_test.hpp"

// TEST(ParserFunctionTest, SimpleMainFunction)
// {
//     std::string input = "fn main() {  }";
//     ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(program->getStatements().size(), 1);

//     ASTFunctionDefinition* function = static_cast<ASTFunctionDefinition*>(program->getStatements()[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);
    
//     ASTIdentifier* identifier = static_cast<ASTIdentifier*>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "main");

//     ASSERT_EQ(function->getParameters().size(), 0);
//     ASSERT_EQ(function->getReturnType(), nullptr);

//     ASTStatementList* body = static_cast<ASTStatementList*>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 0);
// }

// TEST(ParserFunctionTest, FunctionWithOneParameter)
// {
//     std::string input = "fn add(a int) {  }";
//     ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(program->getStatements().size(), 1);

//     ASTFunctionDefinition* function = static_cast<ASTFunctionDefinition*>(program->getStatements()[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier* identifier = static_cast<ASTIdentifier*>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "add");

//     ASSERT_EQ(function->getParameters().size(), 1);
//     ASSERT_EQ(function->getReturnType(), nullptr);

//     ASTFunctionParameter param = function->getParameters()[0];
//     ASSERT_EQ(param.getParamName(), "a");
//     // ASSERT_EQ(param.getParamType().getTypeValue(), "int");

//     ASTStatementList* body = static_cast<ASTStatementList*>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 0);
// }

// // TEST(ParserFunctionTest, FunctionWithTwoParameters)
// // {
// //     std::string input = "fn add(a int, b int) {  }";
// //     ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

// //     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
// //     ASSERT_EQ(program->getStatements().size(), 1);

// //     ASTFunctionDefinition* function = static_cast<ASTFunctionDefinition*>(program->getStatements()[0]);
// //     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

// //     ASTIdentifier* identifier = static_cast<ASTIdentifier*>(function->getExpr());
// //     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
// //     ASSERT_EQ(identifier->getName(), "add");

// //     ASSERT_EQ(function->getParameters().size(), 2);
// //     ASSERT_EQ(function->getReturnType(), nullptr);

// //     ASTFunctionParameter param1 = function->getParameters()[0];
// //     ASSERT_EQ(param1.getParamName(), "a");
// //     // ASSERT_EQ(param1.getParamType().getTypeValue(), "int");

// //     ASTFunctionParameter param2 = function->getParameters()[1];
// //     ASSERT_EQ(param2.getParamName(), "b");
// //     // ASSERT_EQ(param2.getParamType().getTypeValue(), "int");

// //     ASTStatementList* body = static_cast<ASTStatementList*>(function->getBody());
// //     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
// //     ASSERT_EQ(body->getStatements().size(), 0);
// // }

// // TEST(ParserFunctionTest, FunctionWithReturnType)
// // {
// //     std::string input = "fn add(a int, b int) int {  }";
// //     ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

// //     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
// //     ASSERT_EQ(program->getStatements().size(), 1);

// //     ASTFunctionDefinition* function = static_cast<ASTFunctionDefinition*>(program->getStatements()[0]);
// //     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

// //     ASTIdentifier* identifier = static_cast<ASTIdentifier*>(function->getExpr());
// //     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
// //     ASSERT_EQ(identifier->getName(), "add");

// //     ASSERT_EQ(function->getParameters().size(), 2);
// //     ASSERT_NE(function->getReturnType(), nullptr);
// //     // ASSERT_EQ(function->getReturnType()->getTypeValue(), "int");

// //     ASTFunctionParameter param1 = function->getParameters()[0];
// //     ASSERT_EQ(param1.getParamName(), "a");
// //     // ASSERT_EQ(param1.getParamType().getTypeValue(), "int");

// //     ASTFunctionParameter param2 = function->getParameters()[1];
// //     ASSERT_EQ(param2.getParamName(), "b");
// //     // ASSERT_EQ(param2.getParamType().getTypeValue(), "int");

// //     ASTStatementList* body = static_cast<ASTStatementList*>(function->getBody());
// //     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
// //     ASSERT_EQ(body->getStatements().size(), 0);
// // }

// TEST(ParserFunctionTest, FunctionWithBody)
// {
//     std::string input = "fn main() { return 0; }";
//     ASTProgram* program = static_cast<ASTProgram*>(quickParse(input));

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(program->getStatements().size(), 1);

//     ASTFunctionDefinition* function = static_cast<ASTFunctionDefinition*>(program->getStatements()[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier* identifier = static_cast<ASTIdentifier*>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "main");

//     ASSERT_EQ(function->getParameters().size(), 0);
//     ASSERT_EQ(function->getReturnType(), nullptr);

//     ASTStatementList* body = static_cast<ASTStatementList*>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 1);

//     ASTReturnStatement* returnStatement = static_cast<ASTReturnStatement*>(body->getStatements()[0]);
//     ASSERT_EQ(returnStatement->getType(), ASTNode::NodeType::ReturnStatement);
//     ASSERT_TRUE(returnStatement->getExpr().has_value());

//     ASTIntegerLiteral* literal = static_cast<ASTIntegerLiteral*>(returnStatement->getExpr().value());
//     ASSERT_EQ(literal->getType(), ASTNode::NodeType::IntegerLiteral);
//     ASSERT_EQ(literal->getValue(), 0);
// }
