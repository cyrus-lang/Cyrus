#ifndef CODEGEN_C_HPP
#define CODEGEN_C_HPP

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <optional>
#include <any>
#include "ast/ast.hpp"
#include "util/util.hpp"
#include "codegen_c_scope.hpp"
#include "codegen_c_module.hpp"
#include "codegen_c_options.hpp"

class CodeGenC
{
public:
    enum class OutputKind
    {
        Assembly,
        CIR,
        ObjectFile,
        DynamicLibrary,
        StaticLibrary,
        Executable,
    };

    CodeGenC(CodeGenCModule *headModule, OutputKind outputKind, CodeGenCOptions opts) : modules_{headModule}, outputKind_(outputKind), opts_(opts) {};
    ~CodeGenC()
    {
        for (auto &&module : modules_)
        {
            // FIXME
            // warning: deleting pointer to incomplete type 'CodeGenCModule' is incompatible with C++2c and may cause undefined behavior [-Wdelete-incomplete]
            delete module;
        }
    }

    const std::string getExecutableOutputPath()
    {
#ifdef _WIN32
        return opts_.getOutputDirectory() + "/" + modules_[0]->getModuleName() + ".exe";
#else
        return opts_.getOutputDirectory() + "/" + modules_[0]->getModuleName();
#endif
    }

    const std::string getDylibOutputPath()
    {
#ifdef _WIN32
        return opts_.getOutputDirectory() + "/dylib/" + modules_[0]->getModuleName() + ".dll";
#elif __APPLE__
        return opts_.getOutputDirectory() + "/dylib/" + modules_[0]->getModuleName() + ".dylib";
#else
        return opts_.getOutputDirectory() + "/dylib/" + modules_[0]->getModuleName() + ".so";
#endif
    }

    const std::string getStaticLibraryOutputPath()
    {
#ifdef _WIN32
        return opts_.getOutputDirectory() + "/staticlib/" + modules_[0]->getModuleName() + ".lib";
#else
        return opts_.getOutputDirectory() + "/staticlib/" + modules_[0]->getModuleName() + ".a";
#endif
    }

    const std::string getObjectsOutputPath()
    {
        return opts_.getOutputDirectory() + "/" + OBJ_DIR;
    }

    void generate();
    void compile();

private:
    std::vector<CodeGenCModule *> modules_;
    OutputKind outputKind_;
    CodeGenCOptions opts_;
};

class CodeGenCValue
{
public:
    enum class ValueType
    {
        LValue,
        RValue,
        Type,
        Instruction
    };

    CodeGenCValue(std::string source, std::string header, ValueType type) : source_(source), header_(header), type_(type) {};

    std::string getSource() const { return source_; }
    void setSource(std::string source) { source_ = source; }

    std::string getHeader() const { return header_; }
    void setHeader(std::string header) { header_ = header; }

    ValueType getType() const { return type_; }
    void setType(ValueType type) { type_ = type; }

private:
    std::string source_;
    std::string header_;
    ValueType type_;
};

using CodeGenCValuePtr = CodeGenCValue *;

CodeGenCValuePtr codeGenC_StorageClassSpecifier(ASTStorageClassSpecifier storageClassSpecifier);
std::pair<std::string, std::string> codeGenCStatementList(ScopePtr scope, ASTNodeList nodeList);
std::pair<std::string, std::string> codeGenCStatement(ScopePtr scope, ASTNodePtr statement);
CodeGenCValuePtr codeGenC_VariableDeclaration(ScopePtr scope, ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenCStatementList(ScopePtr scope, ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_TypeSpecifier(ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenCExpression(ScopePtr scope, ASTNodePtr nodePtr);
CodeGenCValuePtr codeGenC_FunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater);
CodeGenCValuePtr codeGenC_FunctionDefinition(ASTNodePtr nodePtr);

#endif // CODEGEN_C_HPP
