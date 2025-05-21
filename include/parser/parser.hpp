#ifndef PARSER_HPP
#define PARSER_HPP

#include "ast/ast.hpp"

extern std::shared_ptr<ASTProgram> astProgram;

std::pair<std::shared_ptr<std::string>, std::shared_ptr<ASTProgram>> parseProgram(const std::string &inputFile);

#endif // PARSER_HPP
