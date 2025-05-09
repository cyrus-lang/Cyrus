#ifndef CODEGEN_C_MODULE_HPP
#define CODEGEN_C_MODULE_HPP

#include <ostream>
#include <sstream>
#include "codegen_c_options.hpp"
#include "codegen_c_generator.hpp"

class CodeGenCModule
{
private:
    std::string moduleName_;
    std::stringstream sourceStream_;
    std::stringstream headerStream_;
    std::string moduleCIROutputPath_;
    CodeGenCOptions opts_;

public:
    CodeGenCModule(CodeGenCOptions opts, std::string moduleName) : moduleName_(moduleName), opts_(opts)
    {
        util::ensureDirectoryExists(opts_.getOutputDirectory() + "/" + DYLIB_DIR);
        util::ensureDirectoryExists(opts_.getOutputDirectory() + "/" + STATICLIB_DIR);
        util::ensureDirectoryExists(opts_.getOutputDirectory() + "/" + CIR_DIR);
        util::ensureDirectoryExists(opts_.getOutputDirectory() + "/" + CIR_DIR + "/" + moduleName);
        moduleCIROutputPath_ = opts_.getOutputDirectory() + "/" + CIR_DIR + "/" + moduleName;
    };
    ~CodeGenCModule() {}

    void addSourceFile(CodeGenCSourceFile *sourceFile)
    {
        std::pair<std::string, std::string> generatedCode = sourceFile->generate();
        sourceStream_ << generatedCode.first;
        headerStream_ << generatedCode.second;
        delete sourceFile;
    }

    const std::string &getModuleName() { return moduleName_; }
    const std::string &getModuleCIROutputPath() { return moduleCIROutputPath_; }
    const std::string generateObjectFile();
    void saveModule()
    {
        const std::string cirOutputDirectory = getModuleCIROutputPath();

        std::ofstream outSourceFile(cirOutputDirectory + "/" + moduleName_ + ".c");
        std::ofstream outHeaderFile(cirOutputDirectory + "/" + moduleName_ + ".h");

        if (outSourceFile.is_open() && outHeaderFile.is_open())
        {
            outSourceFile << sourceStream_.str() << std::endl;
            outHeaderFile << headerStream_.str() << std::endl;

            outSourceFile.close();
            outHeaderFile.close();
        }
        else
        {
            std::cerr << "(Error) Opening file for writing failed." << std::endl;
            std::cerr << "        Given path: " << cirOutputDirectory << std::endl;

            if (!outSourceFile.is_open())
            {
                std::cerr << "          Failed to open source file: " << cirOutputDirectory + "/" + moduleName_ + ".c" << std::endl;
            }

            if (!outHeaderFile.is_open())
            {
                std::cerr << "          Failed to open header file: " << cirOutputDirectory + "/" + moduleName_ + ".h" << std::endl;
            }
        }
    }
};

#endif // CODEGEN_C_MODULE_HPP
