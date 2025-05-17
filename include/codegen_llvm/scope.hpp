#ifndef CODEGEN_LLVM_SCOPE_HPP
#define CODEGEN_LLVM_SCOPE_HPP

#include <map>
#include <string>
#include <optional>
#include <memory>
#include "values.hpp"

class Scope
{
private:
    std::optional<ScopePtr> parent_;
    std::map<std::string, CodeGenLLVM_Value *> records_;

public:
    Scope(std::optional<ScopePtr> parent = std::nullopt) : parent_(parent) {}
    ~Scope()
    {
        for (auto const &[_, val] : records_)
        {
            delete val;
        }
    }
    std::optional<CodeGenLLVM_Value *> getRecord(const std::string &name) const
    {
        if (records_.count(name))
        {
            return records_.at(name);
        }
        if (parent)
        {
            return parent.value()->getRecord(name);
        }
        return std::nullopt;
    }

    void setRecord(const std::string &name, CodeGenLLVM_Value *value)
    {
        records_[name] = value;
    }

    ScopePtr clone() const
    {
        ScopePtr clonedScope = std::make_unique<Scope>(this->parent);
        for (const auto &[name, value] : records_)
        {
            clonedScope->setRecord(name, value->clone());
        }
        return clonedScope;
    }
};

using ScopePtr = std::unique_ptr<Scope>;

#endif // CODEGEN_LLVM_SCOPE_HPP
