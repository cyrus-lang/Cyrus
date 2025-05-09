#ifndef CODEGEN_C_OPTIONS
#define CODEGEN_C_OPTIONS

const std::string CIR_DIR = "cir";
const std::string DYLIB_DIR = "dylib";
const std::string STATICLIB_DIR = "staticlib";
const std::string OBJ_DIR = "objects";

class CodeGenCOptions
{
private:
    std::string outputDirectory_;

public:
    std::string getOutputDirectory() const { return outputDirectory_; }
    void setOutputDirectory(const std::string &outputDirectory) { outputDirectory_ = outputDirectory; }
};

#endif // CODEGEN_C_OPTIONS
