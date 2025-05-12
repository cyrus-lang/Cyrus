#ifndef PARSER_HPP
#define PARSER_HPP

#include "ast/ast.hpp"

extern ASTNodePtr astProgram;

ASTProgram *parseProgram(const std::string& inputFile);

#endif // PARSER_HPP
