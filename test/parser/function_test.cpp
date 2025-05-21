// #include "ast/ast.hpp"
// #include "parser_test.hpp"

// TEST(ParserFunctionTest, SimpleMainFunction)
// {
//     std::string input = "fn main() {  }";
//     ASTProgram *program = static_cast<ASTProgram *>(quickParse(input));
//     ASTNodeList statementsList = program->getStatementList()->getStatements();

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(statementsList.size(), 1);

//     ASTFunctionDefinition *function = static_cast<ASTFunctionDefinition *>(statementsList[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier *identifier = static_cast<ASTIdentifier *>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "main");

//     ASSERT_EQ(function->getReturnType(), std::nullopt);

//     ASTFunctionParameters parameters = function->getParameters();
//     ASSERT_EQ(parameters.getList().size(), 0);
//     ASSERT_EQ(parameters.getTypedVariadic(), std::nullopt);

//     ASTStatementList *body = static_cast<ASTStatementList *>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 0);

//     delete program;
// }

// TEST(ParserFunctionTest, FunctionWithOneParameter)
// {
//     std::string input = "fn add(a int) {  }";
//     ASTProgram *program = static_cast<ASTProgram *>(quickParse(input));
//     ASTNodeList statementsList = program->getStatementList()->getStatements();

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(statementsList.size(), 1);

//     ASTFunctionDefinition *function = static_cast<ASTFunctionDefinition *>(statementsList[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier *identifier = static_cast<ASTIdentifier *>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "add");

//     ASTFunctionParameters parameters = function->getParameters();
//     ASSERT_EQ(parameters.getList().size(), 1);
//     ASSERT_EQ(parameters.getTypedVariadic(), std::nullopt);

//     ASTFunctionParameter param = parameters.getList()[0];
//     ASSERT_EQ(param.getParamName(), "a");
//     ASSERT_EQ(param.getParamType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

//     ASTStatementList *body = static_cast<ASTStatementList *>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 0);

//     delete program;
// }

// TEST(ParserFunctionTest, FunctionWithTwoParameters)
// {
//     std::string input = "fn add(a int, b float128) {  }";
//     ASTProgram *program = static_cast<ASTProgram *>(quickParse(input));
//     ASTNodeList statementsList = program->getStatementList()->getStatements();

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(statementsList.size(), 1);

//     ASTFunctionDefinition *function = static_cast<ASTFunctionDefinition *>(statementsList[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier *identifier = static_cast<ASTIdentifier *>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "add");

//     ASTFunctionParameters parameters = function->getParameters();
//     ASSERT_EQ(parameters.getList().size(), 2);
//     ASSERT_EQ(parameters.getTypedVariadic(), std::nullopt);

//     ASTFunctionParameter param1 = parameters.getList()[0];
//     ASSERT_EQ(param1.getParamName(), "a");
//     ASSERT_EQ(param1.getParamType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

//     ASTFunctionParameter param2 = parameters.getList()[1];
//     ASSERT_EQ(param2.getParamName(), "b");
//     ASSERT_EQ(param2.getParamType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Float128);

//     ASTStatementList *body = static_cast<ASTStatementList *>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 0);

//     delete program;
// }

// TEST(ParserFunctionTest, FunctionWithReturnType)
// {
//     std::string input = "fn add(a int, b int) int {  }";
//     ASTProgram *program = static_cast<ASTProgram *>(quickParse(input));
//     ASTNodeList statementsList = program->getStatementList()->getStatements();

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(statementsList.size(), 1);

//     ASTFunctionDefinition *function = static_cast<ASTFunctionDefinition *>(statementsList[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier *identifier = static_cast<ASTIdentifier *>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "add");

//     ASTFunctionParameters parameters = function->getParameters();
//     ASSERT_EQ(parameters.getList().size(), 2);
//     ASSERT_EQ(parameters.getTypedVariadic(), std::nullopt);
//     ASSERT_NE(function->getReturnType(), std::nullopt);

//     ASSERT_EQ(function->getReturnType().has_value(), true);
//     ASSERT_EQ(function->getReturnType().value()->getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

//     ASTFunctionParameter param1 = parameters.getList()[0];
//     ASSERT_EQ(param1.getParamName(), "a");
//     ASSERT_EQ(param1.getParamType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

//     ASTFunctionParameter param2 = parameters.getList()[1];
//     ASSERT_EQ(param2.getParamName(), "b");
//     ASSERT_EQ(param2.getParamType().getTypeValue(), ASTTypeSpecifier::ASTInternalType::Int);

//     ASTStatementList *body = static_cast<ASTStatementList *>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 0);

//     delete program;
// }

// TEST(ParserFunctionTest, FunctionWithBody)
// {
//     std::string input = "fn main() { return 0; }";
//     ASTProgram *program = static_cast<ASTProgram *>(quickParse(input));
//     ASTNodeList statementsList = program->getStatementList()->getStatements();

//     ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
//     ASSERT_EQ(statementsList.size(), 1);

//     ASTFunctionDefinition *function = static_cast<ASTFunctionDefinition *>(statementsList[0]);
//     ASSERT_EQ(function->getType(), ASTNode::NodeType::FunctionDefinition);

//     ASTIdentifier *identifier = static_cast<ASTIdentifier *>(function->getExpr());
//     ASSERT_EQ(identifier->getType(), ASTNode::NodeType::Identifier);
//     ASSERT_EQ(identifier->getName(), "main");

//     ASTFunctionParameters parameters = function->getParameters();
//     ASSERT_EQ(parameters.getList().size(), 0);
//     ASSERT_EQ(parameters.getTypedVariadic(), std::nullopt);
//     ASSERT_EQ(function->getReturnType(), std::nullopt);

//     ASTStatementList *body = static_cast<ASTStatementList *>(function->getBody());
//     ASSERT_EQ(body->getType(), ASTNode::NodeType::StatementList);
//     ASSERT_EQ(body->getStatements().size(), 1);

//     ASTReturnStatement *returnStatement = static_cast<ASTReturnStatement *>(body->getStatements()[0]);
//     ASSERT_EQ(returnStatement->getType(), ASTNode::NodeType::ReturnStatement);
//     ASSERT_TRUE(returnStatement->getExpr().has_value());

//     ASTIntegerLiteral *literal = static_cast<ASTIntegerLiteral *>(returnStatement->getExpr().value());
//     ASSERT_EQ(literal->getType(), ASTNode::NodeType::IntegerLiteral);
//     ASSERT_EQ(literal->getValue(), 0);

//     delete program;
// }

