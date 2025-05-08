#include <iostream>
#include <sstream>
#include "codegen_c/codegen_c.hpp"

// Generation Step

void CodeGenC::generate()
{
    for (auto &&module : modules_)
    {
        module->saveModule(opts_.getOutputDirectory());
    }
}

std::pair<std::string, std::string> codeGenCStatementList(ASTNodeList nodeList)
{
    std::stringstream sourceStream;
    std::stringstream headerStream;

    for (auto &&statement : nodeList)
    {
        auto [source, header] = codeGenCStatement(statement);
        sourceStream << source;
        headerStream << header;
    }

    sourceStream << std::endl;
    return std::make_pair(sourceStream.str(), headerStream.str());
}

std::pair<std::string, std::string> codeGenCStatementListTopLevel(ASTNodeList nodeList)
{
    std::stringstream sourceStream;
    std::stringstream headerStream;

    for (auto &&statement : nodeList)
    {
        switch (statement->getType())
        {
        case ASTNode::NodeType::ImportStatement:
            std::cerr << "Import statement not implemented yet." << std::endl;
            exit(1);
            break;
        case ASTNode::NodeType::VariableDeclaration:
            headerStream << codeGenC_VariableDeclaration(statement)->getSource();
            break;
        default:
            auto [source, header] = codeGenCStatement(statement);
            sourceStream << source;
            headerStream << header;
            break;
        }
    }

    sourceStream << std::endl;
    return std::make_pair(sourceStream.str(), headerStream.str());
}

std::pair<std::string, std::string> CodeGenCSourceFile::generate()
{
    auto [source, header] = codeGenCStatementListTopLevel(program_->getStatements());

    std::string finalHeader;
    finalHeader += "#include <stdio.h>\n";
    finalHeader += "#include <stdint.h>\n";
    finalHeader += "#include <stdbool.h>\n";
    finalHeader += "\n";
    finalHeader += header;

    std::string finalSource;
    finalSource += "#include \"" + moduleName_ + ".h\"" + "\n";
    finalSource += "\n";
    finalSource += source;

    return std::make_pair(finalSource, finalHeader);
}

// Compilation Step

void CodeGenC::compile()
{
}