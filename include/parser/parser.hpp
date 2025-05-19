#ifndef PARSER_HPP
#define PARSER_HPP

#include "ast/ast.hpp"

extern ASTNodePtr astProgram;

std::pair<std::shared_ptr<std::string>, ASTProgram *> parseProgram(const std::string &inputFile);

#endif // PARSER_HPP
