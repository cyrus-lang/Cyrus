#ifndef CODEGEN_C_GENERATOR_HPP
#define CODEGEN_C_GENERATOR_HPP

#include "ast/ast.hpp"

class CodeGenCSourceFile
{
private:
    ASTProgram *program_;
    std::string moduleName_;
    std::string source_;
    std::string header_;

public:
    CodeGenCSourceFile(ASTProgram *program, const std::string &moduleName) : program_(program), moduleName_(moduleName) {}
    ~CodeGenCSourceFile()
    {
        delete program_;
    }

    const std::string &getModuleName()
    {
        return moduleName_;
    }

    const ASTProgram &getProgram()
    {
        return *program_;
    }

    std::string &getSource()
    {
        return source_;
    }

    std::string &getHeader()
    {
        return header_;
    }

    std::pair<std::string, std::string> generate();
};

class CodeGenCFuncTableRecord
{
private:
    ASTFunctionDeclaration *ast_;

public:
    CodeGenCFuncTableRecord(ASTFunctionDeclaration *ast) : ast_(ast) {}
    ASTFunctionDeclaration *getAst() { return ast_; }
};

class CodeGenCValue
{
public:
    enum class ValueKind
    {
        Function,
        Struct,
        Enum,
        Array,
        Int,
        String,
        Float,
        Void,
        Pointer,
        Instruction,
        Identifier,
    };

    CodeGenCValue(ASTTypeSpecifier *type, const std::string &value, ValueKind kind) : type_(type), value_(value), kind_(kind) {}
    ~CodeGenCValue()
    {
        delete type_;
    }

    std::string &getValue() { return value_; }
    ASTTypeSpecifier *getType() const { return type_; }
    ValueKind getValueKind() const { return kind_; }

    CodeGenCValue &operator<<(const std::string &str)
    {
        value_ += str;
        return *this;
    }

private:
    ASTTypeSpecifier *type_;
    std::string value_;
    ValueKind kind_;
};

using CodeGenCValuePtr = CodeGenCValue *;

class CodeGenCGenerator
{
private:
    CodeGenCSourceFile &sourceFile_;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
    std::map<std::string, std::any> global_var_table_;
    std::map<std::string, CodeGenCFuncTableRecord *> func_table_;

    std::string internal_generateFunctionDeclaration(ASTNodePtr nodePtr);

public:
    CodeGenCGenerator(CodeGenCSourceFile &sourceFile) : sourceFile_(sourceFile)
    {
        headerStream_ << "#include <stdint.h>\n";
        headerStream_ << "#include <stdbool.h>\n";
        headerStream_ << "\n";

        sourceStream_ << "#include \"" + sourceFile.getModuleName() + ".h\"" + "\n";
        sourceStream_ << "\n";

        generateTopLevel(sourceFile_.getProgram().getStatements());
    }

    std::stringstream &getSourceStream()
    {
        return sourceStream_;
    }

    std::stringstream &getHeaderStream()
    {
        return headerStream_;
    }

    void addFunction(ASTFunctionDeclaration *funcDecl)
    {
        std::string funcName = generateExpression(nullptr, funcDecl->getExpr())->getValue();

        if (func_table_.count(funcName) == 0)
        {
            func_table_[funcName] = new CodeGenCFuncTableRecord(funcDecl);
        }
        else
        {
            std::cerr << "Function '" << funcName << "' already declared." << std::endl;
            exit(1);
        }
    }

    void generateTopLevel(ASTNodeList nodeList);
    void generateFunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater);
    CodeGenCValuePtr generateStatementList(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateStatementList(ScopePtr scope, ASTNodeList nodeList);
    CodeGenCValuePtr generateStatement(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateVariable(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateReturn(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateFunctionDefinition(ASTNodePtr nodePtr);

    CodeGenCValuePtr generateExpression(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateIntegerLiteral(ASTNodePtr nodePtr);
    CodeGenCValuePtr generateFloatLiteral(ASTNodePtr nodePtr);
    CodeGenCValuePtr generateStringLiteral(ASTNodePtr nodePtr);
    CodeGenCValuePtr generateIdentifier(ASTNodePtr nodePtr);
    CodeGenCValuePtr generateCastExpression(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateBinaryExpression(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateUnaryExpression(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateAssignment(ScopePtr scope, ASTNodePtr nodePtr);
    CodeGenCValuePtr generateIfStatement(ScopePtr scope, ASTNodePtr nodePtr);
    // CodeGenCValuePtr generateImportedSymbolAccess(ScopePtr scope, ASTNodePtr nodePtr);
    // CodeGenCValuePtr generateFunctionCall(ScopePtr scope, ASTNodePtr nodePtr);
    // CodeGenCValuePtr generateConditionalExpression(ScopePtr scope, ASTNodePtr nodePtr);

    CodeGenCValuePtr generateTypeSpecifier(ASTNodePtr nodePtr);
    CodeGenCValuePtr generateIdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    CodeGenCValuePtr generatePointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    CodeGenCValuePtr generateConstTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    CodeGenCValuePtr generateVolatileTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    std::string generateStorageClassSpecifier(ASTStorageClassSpecifier storageClassSpecifier);
};

#endif // CODEGEN_C_GENERATOR_HPP
