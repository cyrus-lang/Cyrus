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

class CodeGenCGenerator
{
private:
    CodeGenCSourceFile &sourceFile_;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
    std::map<std::string, std::any> global_var_table_;
    std::map<std::string, std::any> func_table_;

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

    void generateTopLevel(ASTNodeList nodeList);
    void generateStatementList(ScopePtr scope, ASTNodePtr nodePtr);
    void generateStatementList(ScopePtr scope, ASTNodeList nodeList);
    void generateStatement(ScopePtr scope, ASTNodePtr nodePtr);
    void generateVariable(ScopePtr scope, ASTNodePtr nodePtr);
    void generateReturn(ScopePtr scope, ASTNodePtr nodePtr);
    void generateFunctionDeclaration(ASTNodePtr nodePtr, bool bodyLater);
    void generateFunctionDefinition(ASTNodePtr nodePtr);

    std::string generateExpression(ScopePtr scope, ASTNodePtr nodePtr);
    std::string generateIntegerLiteral(ASTNodePtr nodePtr);
    std::string generateFloatLiteral(ASTNodePtr nodePtr);
    std::string generateStringLiteral(ASTNodePtr nodePtr);
    std::string generateIdentifier(ASTNodePtr nodePtr);
    std::string generateCastExpression(ScopePtr scope, ASTNodePtr nodePtr);
    std::string generateBinaryExpression(ScopePtr scope, ASTNodePtr nodePtr);
    std::string generateUnaryExpression(ScopePtr scope, ASTNodePtr nodePtr);
    std::string generatedImportedSymbolAccess(ScopePtr scope, ASTNodePtr nodePtr);

    std::string generateTypeSpecifier(ASTNodePtr nodePtr);
    std::string generateIdentifierTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    std::string generatePointerTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    std::string generateConstTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    std::string generateVolatileTypeSpecifier(ASTTypeSpecifier *typeSpecifier);
    std::string generateStorageClassSpecifier(ASTStorageClassSpecifier storageClassSpecifier);
};

#endif // CODEGEN_C_GENERATOR_HPP
