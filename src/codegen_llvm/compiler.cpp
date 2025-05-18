#include <memory>
#include <iostream>
#include "util/util.hpp"
#include "parser/parser.hpp"
#include "codegen_llvm/compiler.hpp"
#include <llvm/Support/FileSystem.h>

void new_codegen_llvm(CodeGenLLVM_Options opts)
{
    CodeGenLLVM_Context context;

    if (opts.getInputFile().has_value())
    {
        // compiler triggered to compile single files
        std::string filePath = opts.getInputFile().value();
        ASTProgram *program = parseProgram(filePath);

        std::string moduleName = util::getFileNameWithStem(filePath);
        util::isValidModuleName(moduleName, filePath);
        CodeGenLLVM_Module *module = context.createModule(moduleName, filePath);

        module->buildProgramIR(program);

        std::string outputPath;
        if (opts.getOutputPath().has_value())
        {
            outputPath = opts.getOutputPath().value();
        }
        else if (opts.getBuildDirectory().has_value())
        {
            outputPath = opts.getBuildDirectory().value() + "/" + LLVMIR_DIR;
        }
        else
        {
            std::cerr << "(Error) Output path is not specified." << std::endl;
            std::cerr << "        Build directory is not specified too. Consider to specify one these options to continue compilation." << std::endl;
            exit(1);
        }

        switch (opts.getOutputKind())
        {
        case CodeGenLLVM_OutputKind::LLVMIR:
        {
            context.saveIR(outputPath);
            std::cout << "(Success) LLVM IR files are saved to " << outputPath << std::endl;
        }
        break;
        default:
        {
            std::cerr << "(Error) Unsupported output kind." << std::endl;
            exit(1);
        }
        break;
        }
    }
    else
    {
        // compiler gonna get options from a configuration file
        std::cerr << "(Error) Compile with Project.toml is not supported yet." << std::endl;
        exit(1);
    }
}

void CodeGenLLVM_Context::saveIR(const std::string &outputPath)
{
    util::ensureDirectoryExists(outputPath);

    for (auto &&module : modules_)
    {
        std::string moduleName = module.first;
        std::string filePath = outputPath + "/" + moduleName + ".ll";
        std::error_code ec;
        llvm::raw_fd_ostream dest(filePath, ec, llvm::sys::fs::OF_None);

        if (ec)
        {
            llvm::errs() << "(Error) Could not open file: " << ec.message();
            exit(1);
        }

        module.second->getModule()->print(dest, nullptr);
    }
}

void CodeGenLLVM_Module::buildProgramIR(ASTProgram *program)
{
    ASTNodeList statementsList = program->getStatementList()->getStatements();

    for (auto &&statement : statementsList)
    {
        switch (statement->getType())
        {
        case ASTNode::NodeType::VariableDeclaration:
            compileGlobalVariableDeclaration(statement);
            break;
        case ASTNode::NodeType::FunctionDefinition:
            compileFunctionDefinition(statement);
            break;
        case ASTNode::NodeType::StatementList:
        {
            std::cerr << "(Error) Invalid program." << std::endl;
            exit(1);
        }
        default:
            statement->print(0);
            compileStmt(std::nullopt, statement);
            break;
        }
    }

    delete program;
}


llvm::AllocaInst* CodeGenLLVM_Module::createZeroInitializedAlloca(
    const std::string &name,
    std::shared_ptr<CodeGenLLVM_Type> type,
    std::optional<llvm::Value*> init)
{
    llvm::AllocaInst *alloca = builder_.CreateAlloca(type->getLLVMType(), nullptr, name);

    if (init.has_value())
    {
        builder_.CreateStore(init.value(), alloca);
    }
    else
    {
        llvm::Value *zero = createZeroInitializedValue(type);
        builder_.CreateStore(zero, alloca);
    }

    return alloca;
}