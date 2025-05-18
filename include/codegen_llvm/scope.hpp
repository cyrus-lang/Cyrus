#ifndef CODEGEN_LLVM_SCOPE_HPP
#define CODEGEN_LLVM_SCOPE_HPP

#include <map>
#include <string>
#include <optional>
#include <memory>
#include "values.hpp"

class Scope;

using ScopePtr = Scope *;
using OptionalScopePtr = std::optional<Scope *>;

#define SCOPE_REQUIRED()                                                                    \
    if (!scope)                                                                             \
    {                                                                                       \
        std::cerr << "(Error) Scope is required to compile this instruction." << std::endl; \
        exit(1);                                                                            \
    }

class Scope
{
private:
    std::optional<ScopePtr> parent_;
    std::map<std::string, CodeGenLLVM_EValue *> records_;

public:
    Scope(std::optional<ScopePtr> parent = std::nullopt) : parent_(parent) {}
    ~Scope()
    {
        for (auto const &[_, evalue] : records_)
        {
            delete evalue;
        }
    }

    std::optional<CodeGenLLVM_EValue *> getRecord(const std::string &name) const
    {
        if (records_.count(name))
        {
            return records_.at(name);
        }
        if (parent_)
        {
            return parent_.value()->getRecord(name);
        }
        return std::nullopt;
    }

    void setRecord(const std::string &name, CodeGenLLVM_EValue *evalue)
    {
        records_[name] = evalue;
    }

    ScopePtr clone() const
    {
        ScopePtr clonedScope = new Scope(this->parent_);
        for (const auto &[name, value] : records_)
        {
            clonedScope->setRecord(name, value);
        }
        return clonedScope;
    }
};

#endif // CODEGEN_LLVM_SCOPE_HPP
