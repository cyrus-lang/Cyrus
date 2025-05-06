#include "ast/ast.hpp"
#include "parser_test.hpp"

TEST(ParserProgramTest, Valid)
{
    std::string input = "fn main() {  }";
    ASTNodePtr program = quickParse(input);

    ASSERT_EQ(program->getType(), ASTNode::NodeType::Program);
}