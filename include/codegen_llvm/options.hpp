#ifndef CODEGEN_LLVM_OPTIONS_HPP
#define CODEGEN_LLVM_OPTIONS_HPP

#include <vector>

const std::string LLVMIR_DIR = "llvmir";
const std::string DYLIB_DIR = "dylib";
const std::string STATICLIB_DIR = "staticlib";
const std::string OBJ_DIR = "objects";

enum class CodeGenLLVM_OutputKind
{
    Executable,
    ObjectFile,
    StaticLibrary,
    DynamicLibrary,
    Assembly,
    LLVMIR,
};

class CodeGenLLVM_Options
{
private:
    std::string outputPath_;
    std::optional<std::string> buildDirectory_;
    std::optional<std::vector<std::string>> inputFiles_;
    CodeGenLLVM_OutputKind outputKind_;

public:
    std::string getOutputPath() const { return outputPath_; }
    void setOutputPath(const std::string &outputPath) { outputPath_ = outputPath; }

    std::optional<std::string> getBuildDirectory() const { return buildDirectory_; }
    void setBuildDirectory(const std::string &buildDirectory) { buildDirectory_ = buildDirectory; }

    std::optional<std::vector<std::string>> getInputFiles() const { return inputFiles_; }
    void setInputFiles(const std::vector<std::string> &inputFiles) { inputFiles_ = inputFiles; }

    CodeGenLLVM_OutputKind getOutputKind() const { return outputKind_; }
    void setOutputKind(const CodeGenLLVM_OutputKind &outputKind) { outputKind_ = outputKind; }
};

#endif // CODEGEN_LLVM_OPTIONS_HPP
