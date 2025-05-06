#ifndef AST_HPP
#define AST_HPP

#include <iostream>
#include <vector>
#include <memory>
#include "node.hpp"
#include "types.hpp"

class ASTProgram : public ASTNode
{
private:
    ASTNodeList statements_;

public:
    ASTProgram(ASTNodeList statements) : statements_(statements) {}
    NodeType getType() const override { return NodeType::Program; }
    const ASTNodeList &getStatements() const { return statements_; }
    void addStatement(ASTNodePtr statement) { statements_.push_back(statement); }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Program:" << std::endl;
        for (const auto &stmt : statements_)
        {
            stmt->print(indent + 1);
        }
    }
};

class ASTStatementList : public ASTNode
{
private:
    ASTNodeList statements_;

public:
    ASTStatementList() : statements_() {}
    ASTStatementList(ASTNodePtr statement) : statements_({statement}) {}
    NodeType getType() const override { return NodeType::StatementList; }
    const ASTNodeList &getStatements() const { return statements_; }
    void addStatement(ASTNodePtr statement) { statements_.push_back(statement); }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StatementList:" << std::endl;
        for (const auto &stmt : statements_)
        {
            stmt->print(indent + 1);
        }
    }
};

enum class ASTAccessSpecifier
{
    Default,
    Public,
    Private,
    Abstract,
    Virtual,
    Override,
    Protected
};

enum class ASTStorageClassSpecifier
{
    Extern,
    Static,
    Register
};

void printASTAccessSpecifier(ASTAccessSpecifier accessSpecifier);

class ASTIntegerLiteral : public ASTNode
{
private:
    int value_;

public:
    ASTIntegerLiteral(int value) : value_(value) {}
    NodeType getType() const override { return NodeType::IntegerLiteral; }
    int getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "IntegerLiteral: " << value_;
    }
};

class ASTFloatLiteral : public ASTNode
{
private:
    float value_;

public:
    ASTFloatLiteral(float value) : value_(value) {}
    NodeType getType() const override { return NodeType::FloatLiteral; }
    float getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FloatLiteral: " << value_;
    }
};

class ASTStringLiteral : public ASTNode
{
private:
    std::string value_;

public:
    ASTStringLiteral(std::string value) : value_(value) {}
    NodeType getType() const override { return NodeType::StringLiteral; }
    std::string getValue() const { return value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StringLiteral: " << value_;
    }
};

class ASTIdentifier : public ASTNode
{
public:
    ASTIdentifier(std::string name) : name_(name) {}
    NodeType getType() const override { return NodeType::Identifier; }
    const std::string &getName() const { return name_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "Identifier: " << name_ << std::endl;
    }

private:
    std::string name_;
};

class ASTBinaryExpression : public ASTNode
{
public:
    enum class Operator
    {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder,
        Equal,
        NotEqual,
        LessThan,
        LessEqual,
        GreaterThan,
        GreaterEqual,
        LeftShift,
        RightShift,
        BitwiseAnd,
        BitwiseXor,
        BitwiseOr,
        LogicalAnd,
        LogicalOr
    };

    ASTBinaryExpression(ASTNodePtr left, Operator op, ASTNodePtr right)
        : left_(left), op_(op), right_(right) {}

    NodeType getType() const override { return NodeType::BinaryExpression; }

    ASTNode *getLeft() const { return left_; }
    Operator getOperator() const { return op_; }
    ASTNode *getRight() const { return right_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "BinaryExpression: " << formatOperator(op_) << std::endl;
        left_->print(indent + 1);
        std::cout << std::endl;
        right_->print(indent + 1);
    }

private:
    ASTNodePtr left_;
    Operator op_;
    ASTNodePtr right_;

    std::string formatOperator(Operator op) const
    {
        switch (op)
        {
        case Operator::Add:
            return "+";
        case Operator::Subtract:
            return "-";
        case Operator::Multiply:
            return "*";
        case Operator::Divide:
            return "/";
        case Operator::Remainder:
            return "%";
        case Operator::Equal:
            return "==";
        case Operator::NotEqual:
            return "!=";
        case Operator::LessThan:
            return "<";
        case Operator::LessEqual:
            return "<=";
        case Operator::GreaterThan:
            return ">";
        case Operator::GreaterEqual:
            return ">=";
        case Operator::LeftShift:
            return "<<";
        case Operator::RightShift:
            return ">>";
        case Operator::BitwiseAnd:
            return "&";
        case Operator::BitwiseXor:
            return "^";
        case Operator::BitwiseOr:
            return "|";
        case Operator::LogicalAnd:
            return "&&";
        case Operator::LogicalOr:
            return "||";
        default:
            return "Unknown Operator";
        }
    }
};

class ASTUnaryExpression : public ASTNode
{
public:
    enum class Operator
    {
        Negate,
        LogicalNot,
        BitwiseNot,
        Plus,
        AddressOf,
        Dereference,
        PreIncrement,
        PreDecrement,
        PostIncrement,
        PostDecrement
    };

    ASTUnaryExpression(Operator op, ASTNodePtr operand)
        : op_(op), operand_(operand) {}

    NodeType getType() const override { return NodeType::UnaryExpression; }

    Operator getOperator() const { return op_; }
    ASTNode *getOperand() const { return operand_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "UnaryExpression: " << formatOperator(op_) << std::endl;
        operand_->print(indent + 1);
    }

private:
    Operator op_;
    ASTNodePtr operand_;

    std::string formatOperator(Operator op) const
    {
        switch (op)
        {
        case Operator::Negate:
            return "-";
        case Operator::LogicalNot:
            return "!";
        case Operator::BitwiseNot:
            return "~";
        case Operator::Plus:
            return "+";
        case Operator::AddressOf:
            return "&";
        case Operator::PreIncrement:
            return "++";
        case Operator::PreDecrement:
            return "--";
        case Operator::PostIncrement:
            return "++";
        case Operator::PostDecrement:
            return "--";
        default:
            return "Unknown Operator";
        }
    }
};

class ASTCastExpression : public ASTNode
{
private:
    ASTNodePtr expression_;
    ASTTypeSpecifier targetType_;

public:
    ASTCastExpression(ASTTypeSpecifier targetType, ASTNodePtr expression)
        : expression_(expression), targetType_(targetType) {}

    NodeType getType() const override { return NodeType::CastExpression; }
    ASTNode *getExpression() const { return expression_; }
    const ASTTypeSpecifier &getTargetType() const { return targetType_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "CastExpression:" << std::endl;

        printIndent(indent + 1);
        std::cout << "Target Type: ";
        targetType_.print(indent + 1);
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "Expression:" << std::endl;
        expression_->print(indent + 2);
    }
};

class ASTImportStatement : public ASTNode
{
private:
    std::vector<std::string> modulePath_;

public:
    ASTImportStatement(std::vector<std::string> modulePath) : modulePath_(modulePath) {}
    NodeType getType() const override { return NodeType::ImportStatement; }
    const std::vector<std::string> &getModulePath() const { return modulePath_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ImportStatement: ";
        for (size_t i = 0; i < modulePath_.size(); ++i)
        {
            std::cout << modulePath_[i];
            if (i < modulePath_.size() - 1)
            {
                std::cout << "::";
            }
        }
        std::cout << std::endl;
    }
};

class ASTImportedSymbolAccess : public ASTNode
{
private:
    std::vector<std::string> symbolPath_;

public:
    ASTImportedSymbolAccess(std::vector<std::string> symbolPath) : symbolPath_(symbolPath) {}
    NodeType getType() const override { return NodeType::ImportedSymbolAccess; }
    const std::vector<std::string> &getSymbolPath() const { return symbolPath_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ImportedSymbolAccess: ";
        for (size_t i = 0; i < symbolPath_.size(); ++i)
        {
            std::cout << symbolPath_[i];
            if (i < symbolPath_.size() - 1)
            {
                std::cout << "::";
            }
        }
        std::cout << std::endl;
    }
};

class ASTTypeDefStatement : public ASTNode
{
private:
    std::string name_;
    ASTTypeSpecifier type_;
    ASTAccessSpecifier accessSpecifier_;

public:
    ASTTypeDefStatement(std::string name, ASTTypeSpecifier type, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)
        : name_(name), type_(type), accessSpecifier_(accessSpecifier) {}

    NodeType getType() const override { return NodeType::TypeDefStatement; }
    const std::string &getName() const { return name_; }
    const ASTTypeSpecifier &getTypeSpecifier() const { return type_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "TypeDefStatement: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_;
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "Type: ";
        type_.print(indent);

        printIndent(indent + 1);
        printASTAccessSpecifier(accessSpecifier_);
    }
};

class ASTFunctionParameter : public ASTNode
{
private:
    std::string param_name_;
    ASTTypeSpecifier param_type_;
    ASTNodePtr default_value_;

public:
    ASTFunctionParameter(std::string param_name, ASTTypeSpecifier param_type, ASTNodePtr default_value = nullptr)
        : param_name_(param_name), param_type_(param_type), default_value_(default_value) {}

    NodeType getType() const override { return NodeType::FunctionParameter; }
    const std::string &getParamName() const { return param_name_; }
    ASTTypeSpecifier getParamType() const { return param_type_; }
    ASTNodePtr getDefaultValue() const { return default_value_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionParameter: " << param_name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Type: ";
        param_type_.print(indent);
        std::cout << std::endl;

        if (default_value_)
        {
            printIndent(indent + 1);
            std::cout << "Default Value:" << std::endl;
            default_value_->print(indent + 2);
        }
    }
};

class ASTFunctionDefinition : public ASTNode
{
private:
    ASTNodePtr expr_;
    std::vector<ASTFunctionParameter> parameters_;
    ASTTypeSpecifier *returnType_;
    ASTNodePtr body_;

public:
    ASTFunctionDefinition(ASTNodePtr expr, std::vector<ASTFunctionParameter> parameters, ASTTypeSpecifier *returnType, ASTNodePtr body)
        : expr_(expr), parameters_(parameters), returnType_(returnType), body_(body) {}

    NodeType getType() const override { return NodeType::FunctionDefinition; }
    ASTNodePtr getExpr() const { return expr_; }
    const std::vector<ASTFunctionParameter> &getParameters() const { return parameters_; }
    ASTTypeSpecifier *getReturnType() const { return returnType_; }
    ASTNode *getBody() const { return body_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionDefinition: " << std::endl;
        expr_->print(indent + 1);

        printIndent(indent + 1);
        std::cout << "Parameters:" << std::endl;
        for (const auto &param : parameters_)
        {
            param.print(indent + 2);
        }

        if (returnType_)
        {
            printIndent(indent + 1);
            std::cout << "Return Type: ";
            returnType_->print(indent);
            std::cout << std::endl;
        }

        printIndent(indent + 1);
        std::cout << "Body:" << std::endl;
        body_->print(indent + 2);
    }
};

class ASTVariableDeclaration : public ASTNode
{
private:
    std::string name_;
    ASTTypeSpecifier *type_;
    ASTNodePtr initializer_;

public:
    ASTVariableDeclaration(std::string name, ASTTypeSpecifier *type, ASTNodePtr initializer = nullptr)
        : name_(name), type_(type), initializer_(initializer) {}

    NodeType getType() const override { return NodeType::VariableDeclaration; }
    const std::string &getName() const { return name_; }
    ASTTypeSpecifier *getTypeValue() const { return type_; }
    ASTNodePtr getInitializer() const { return initializer_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "VariableDeclaration: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_ << std::endl;

        if (type_)
        {
            printIndent(indent + 1);
            std::cout << "Type: ";
            type_->print(indent);
        }

        if (initializer_)
        {
            printIndent(indent + 1);
            std::cout << "Initializer:" << std::endl;
            initializer_->print(indent + 2);
            std::cout << std::endl;
        }
    }
};

class ASTStructField : public ASTNode
{
private:
    std::string name_;
    ASTTypeSpecifier type_;
    ASTAccessSpecifier accessSpecifier_;

public:
    ASTStructField(std::string name, ASTTypeSpecifier type, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)
        : name_(name), type_(type), accessSpecifier_(accessSpecifier) {}

    NodeType getType() const override { return NodeType::StructField; }
    const std::string &getName() const { return name_; }
    const ASTTypeSpecifier &getTypeSpecifier() const { return type_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StructField: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Name: " << name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Type: ";
        type_.print(indent);

        printIndent(indent + 1);
        printASTAccessSpecifier(accessSpecifier_);
    }
};

class ASTStructDefinition : public ASTNode
{
private:
    std::optional<std::string> name_;
    std::vector<ASTStructField> members_;
    std::vector<ASTFunctionDefinition> methods_;
    ASTAccessSpecifier accessSpecifier_;

public:
    ASTStructDefinition(std::optional<std::string> name, std::vector<ASTStructField> members, std::vector<ASTFunctionDefinition> methods, ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)
        : name_(name), members_(members), methods_(methods), accessSpecifier_(accessSpecifier) {}

    NodeType getType() const override { return NodeType::StructDefinition; }
    const std::optional<std::string> &getName() const { return name_; }
    const std::vector<ASTStructField> &getMembers() const { return members_; }
    const std::vector<ASTFunctionDefinition> &getMethods() const { return methods_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StructDefinition: " << std::endl;

        if (name_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Name: " << name_.value() << std::endl;
        }

        printIndent(indent + 1);
        printASTAccessSpecifier(accessSpecifier_);

        printIndent(indent + 1);
        std::cout << "Members:" << std::endl;
        for (const auto &member : members_)
        {
            member.print(indent + 2);
        }

        printIndent(indent + 1);
        std::cout << "Methods:" << std::endl;
        for (const auto &method : methods_)
        {
            method.print(indent + 2);
        }
    }
};

class ASTStructInitialization : public ASTNode
{
private:
    std::string structName_;
    std::vector<std::pair<std::string, ASTNodePtr>> fieldInitializers_;

public:
    ASTStructInitialization(std::string structName, std::vector<std::pair<std::string, ASTNodePtr>> fieldInitializers)
        : structName_(structName), fieldInitializers_(fieldInitializers) {}

    NodeType getType() const override { return NodeType::StructInitialization; }
    const std::string &getStructName() const { return structName_; }
    const std::vector<std::pair<std::string, ASTNodePtr>> &getFieldInitializers() const { return fieldInitializers_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "StructInitialization: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Struct Name: " << structName_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Field Initializers:" << std::endl;
        for (const auto &pair : fieldInitializers_)
        {
            printIndent(indent + 2);
            std::cout << "Field Name: " << pair.first << std::endl;
            pair.second->print(indent + 3);
        }
    }
};

class ASTConditionalExpression : public ASTNode
{
private:
    ASTNodePtr condition_;
    ASTNodePtr trueExpression_;
    ASTNodePtr falseExpression_;

public:
    ASTConditionalExpression(ASTNodePtr condition, ASTNodePtr trueExpression, ASTNodePtr falseExpression)
        : condition_(condition), trueExpression_(trueExpression), falseExpression_(falseExpression) {}

    NodeType getType() const override { return NodeType::ConditionalExpression; }
    ASTNode *getCondition() const { return condition_; }
    ASTNode *getTrueExpression() const { return trueExpression_; }
    ASTNode *getFalseExpression() const { return falseExpression_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "ConditionalExpression:" << std::endl;

        printIndent(indent + 1);
        std::cout << "Condition:" << std::endl;
        condition_->print(indent + 2);

        printIndent(indent + 1);
        std::cout << "True Expression:" << std::endl;
        trueExpression_->print(indent + 2);
        std::cout << std::endl;

        printIndent(indent + 1);
        std::cout << "False Expression:" << std::endl;
        falseExpression_->print(indent + 2);
    }
};

class ASTAssignmentExpression : public ASTNode
{
public:
    enum class Operator
    {
        Assign,
        AddAssign,
        SubtractAssign,
        MultiplyAssign,
        DivideAssign,
        RemainderAssign,
        LeftShiftAssign,
        RightShiftAssign,
        BitwiseAndAssign,
        BitwiseXorAssign,
        BitwiseOrAssign
    };

    ASTAssignmentExpression(ASTNodePtr left, Operator op, ASTNodePtr right)
        : left_(left), op_(op), right_(right) {}

    NodeType getType() const override { return NodeType::AssignmentExpression; }

    ASTNode *getLeft() const { return left_; }
    Operator getOperator() const { return op_; }
    ASTNode *getRight() const { return right_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "AssignmentExpression: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Operator: " << formatOperator(op_) << std::endl;

        left_->print(indent + 1);
        right_->print(indent + 1);
    }

private:
    ASTNodePtr left_;
    Operator op_;
    ASTNodePtr right_;

    std::string formatOperator(Operator op) const
    {
        switch (op)
        {
        case Operator::Assign:
            return "=";
        case Operator::AddAssign:
            return "+=";
        case Operator::SubtractAssign:
            return "-=";
        case Operator::MultiplyAssign:
            return "*=";
        case Operator::DivideAssign:
            return "/=";
        case Operator::RemainderAssign:
            return "%=";
        case Operator::LeftShiftAssign:
            return "<<=";
        case Operator::RightShiftAssign:
            return ">>=";
        case Operator::BitwiseAndAssign:
            return "&=";
        case Operator::BitwiseXorAssign:
            return "^=";
        case Operator::BitwiseOrAssign:
            return "|=";
        default:
            return "Unknown Operator";
        }
    }
};

class ASTFunctionCall : public ASTNode
{
private:
    ASTNodePtr expr_;
    std::vector<ASTNodePtr> arguments_;

public:
    ASTFunctionCall(ASTNodePtr expr, std::vector<ASTNodePtr> arguments)
        : expr_(expr), arguments_(arguments) {}

    NodeType getType() const override { return NodeType::FunctionCall; }
    ASTNodePtr getExpr() const { return expr_; }
    const std::vector<ASTNodePtr> &getArguments() const { return arguments_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FunctionCall: " << std::endl;

        printIndent(indent + 1);
        std::cout << "Function Expr:" << std::endl;
        expr_->print(indent + 2);

        printIndent(indent + 1);
        std::cout << "Arguments:" << std::endl;
        for (const auto &arg : arguments_)
        {
            arg->print(indent + 2);
        }
    }
};

class ASTFieldAccess : public ASTNode
{
private:
    ASTNodePtr operand_;
    std::string field_name_;

public:
    ASTFieldAccess(ASTNodePtr operand, std::string field_name)
        : operand_(operand), field_name_(field_name) {}

    NodeType getType() const override { return NodeType::FieldAccess; }
    ASTNodePtr getOperand() const { return operand_; }
    const std::string &getFieldName() const { return field_name_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "FieldAccess: " << field_name_ << std::endl;
        operand_->print(indent + 1);
    }
};

class ASTPointerFieldAccess : public ASTNode
{
private:
    ASTFieldAccess field_access_;

public:
    ASTPointerFieldAccess(ASTFieldAccess field_access) : field_access_(field_access) {}

    NodeType getType() const override { return NodeType::PointerFieldAccess; }
    ASTFieldAccess getFieldAccess() const { return field_access_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "PointerFieldAccess: " << std::endl;
        field_access_.print(indent + 1);
    }
};

class ASTEnumVariantItem 
{
private:
    std::optional<std::string> name_;
    ASTTypeSpecifier type_;
public:
    ASTEnumVariantItem(std::optional<std::string> name, ASTTypeSpecifier type) : name_(name), type_(type) {}

    const std::optional<std::string> &getName() const { return name_; }
    const ASTTypeSpecifier &getTypeSpecifier() const { return type_; }
};


class ASTEnumVariant : public ASTNode
{
private:
    std::string name_;
    std::vector<ASTEnumVariantItem> items_;

public:
    ASTEnumVariant(std::string name, std::vector<ASTEnumVariantItem> items)
        : name_(name), items_(items) {}

    NodeType getType() const override { return NodeType::EnumVariant; }
    const std::string &getName() const { return name_; }
    const std::vector<ASTEnumVariantItem> &getItems() const { return items_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "EnumVariant: " << std::endl;
        printIndent(indent + 1);

        std::cout << "Name: " << name_ << std::endl;

        printIndent(indent + 1);
        std::cout << "Items:" << std::endl;
        for (const auto &item : items_)
        {
            printIndent(indent + 2);
            std::cout << "Item: " << std::endl;
            if (item.getName().has_value())
            {
                printIndent(indent + 3);
                std::cout << "Name: " << item.getName().value() << std::endl;
            }
            printIndent(indent + 3);
            std::cout << "Type: ";
            item.getTypeSpecifier().print(indent + 3);
        }
    }
};

class ASTEnumDefinition : public ASTNode
{
private:
    std::optional<std::string> name_;
    std::vector<ASTEnumVariant> variants_;
    std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> fields_;
    std::vector<ASTFunctionDefinition> methods_;
    ASTAccessSpecifier accessSpecifier_;

public:
    ASTEnumDefinition(std::optional<std::string> name,
                      std::vector<ASTEnumVariant> variants,
                      std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> fields,
                      std::vector<ASTFunctionDefinition> methods,
                      ASTAccessSpecifier accessSpecifier = ASTAccessSpecifier::Default)

        : name_(name), variants_(variants), fields_(fields), methods_(methods), accessSpecifier_(accessSpecifier)
    {
    }

    NodeType getType() const override { return NodeType::EnumDefinition; }
    const std::optional<std::string> &getName() const { return name_; }
    const std::vector<ASTEnumVariant> &getVariants() const { return variants_; }
    const std::vector<std::pair<std::string, std::optional<ASTNodePtr>>> &getFields() const { return fields_; }
    const std::vector<ASTFunctionDefinition> &getMethods() const { return methods_; }
    ASTAccessSpecifier getAccessSpecifier() const { return accessSpecifier_; }

    void print(int indent) const override
    {
        printIndent(indent);
        std::cout << "EnumDefinition: " << std::endl;

        if (name_.has_value())
        {
            printIndent(indent + 1);
            std::cout << "Name: " << name_.value() << std::endl;
        }

        printIndent(indent + 1);
        printASTAccessSpecifier(accessSpecifier_);

        printIndent(indent + 1);
        std::cout << "Variants:" << std::endl;
        for (const auto &variant : variants_)
        {
            variant.print(indent + 2);
        }

        printIndent(indent + 1);
        std::cout << "Fields:" << std::endl;
        for (const auto &field : fields_)
        {
            std::optional<ASTNodePtr> fieldValue = field.second;
            if (fieldValue.has_value())
            {
                printIndent(indent + 2);
                std::cout << field.first << ": ";
                fieldValue.value()->print(0);
                std::cout << std::endl;
            }
        }

        printIndent(indent + 1);
        std::cout << "Methods:" << std::endl;
        for (const auto &method : methods_)
        {
            method.print(indent + 2);
        }
    }
};

#endif