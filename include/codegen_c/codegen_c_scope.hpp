#ifndef CODEGEN_C_SCOPE_HPP
#define CODEGEN_C_SCOPE_HPP

#include <map>
#include "ast/ast.hpp"

class ScopeRecord
{
public:
    ScopeRecord(ASTTypeSpecifier type) : type_(type) {}
    ASTTypeSpecifier getType() const { return type_; }

private:
    ASTTypeSpecifier type_;
};

class Scope
{
public:
    Scope() : parent_(nullptr) {}
    Scope(Scope *parent) : parent_(parent) {}

    void set(const std::string &name, ScopeRecord value)
    {
        scope[name] = &value;
    }

    std::optional<ScopeRecord*> get(const std::string &name)
    {
        if (scope.count(name))
        {
            return scope[name];
        }
        if (parent_)
        {
            return parent_->get(name);
        }

        return std::nullopt;
    }

private:
    std::map<std::string, ScopeRecord *> scope;
    Scope *parent_;
};

using ScopePtr = Scope *;

#endif // CODEGEN_C_SCOPE_HPP
