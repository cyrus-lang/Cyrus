#include <iostream>
#include <sstream>
#include "codegen_c/codegen_c.hpp"
#include "codegen_c/codegen_c_scope.hpp"
#include "codegen_c/codegen_c_module.hpp"

void compileStaticLibrary(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);
void compileExecutable(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);
void compileDylib(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath);

// Generation Step

void CodeGenC::generate()
{
    for (auto &&module : modules_)
    {
        module->saveModule();
    }
}

// Compilation Step

const std::string CodeGenCModule::generateObjectFile()
{
    std::string headerPath = moduleCIROutputPath_ + "/" + moduleName_ + ".h";
    std::string sourcePath = moduleCIROutputPath_ + "/" + moduleName_ + ".c";
    std::string objectPath = opts_.getOutputDirectory() + "/" + OBJ_DIR;

    util::ensureDirectoryExists(objectPath);

    objectPath += "/" + moduleName_ + ".o";

    std::string command = "gcc -c -o " + objectPath + " " + sourcePath + " -I " + moduleCIROutputPath_;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to compile module `" << moduleName_ << "`." << std::endl;
        exit(1);
    }

    return objectPath;
}

void CodeGenC::compile()
{
    std::vector<std::string> modulePaths;

    for (auto &&module : modules_)
    {
        const std::string objectPath = module->generateObjectFile();
        modulePaths.push_back(objectPath);
    }

    switch (outputKind_)
    {
    case CodeGenC::OutputKind::Assembly:
        /* code */
        break;
    case CodeGenC::OutputKind::CIR:
        /* code */
        break;
    case CodeGenC::OutputKind::ObjectFile:
        /* code */
        break;
    case CodeGenC::OutputKind::DynamicLibrary:
        compileDylib(modulePaths, getObjectsOutputPath(), getDylibOutputPath());
        break;
    case CodeGenC::OutputKind::StaticLibrary:
        compileStaticLibrary(modulePaths, getObjectsOutputPath(), getStaticLibraryOutputPath());
        break;
    case CodeGenC::OutputKind::Executable:
        compileExecutable(modulePaths, getObjectsOutputPath(), getExecutableOutputPath());
        break;
    default:
        std::cerr << "(Error) Unsupported output kind." << std::endl;
        exit(1);
    }
}

void compileExecutable(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath)
{
    std::string objectFiles;
    for (auto &&modulePath : modulePaths)
    {
        objectFiles += modulePath + " ";
    }

    std::string command = "gcc " + objectFiles + " -o " + outputPath;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to link object files." << std::endl;
        exit(1);
    }
}

void compileDylib(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath)
{
    std::string objectFiles;
    for (auto &&modulePath : modulePaths)
    {
        objectFiles += modulePath + " ";
    }

    std::string command = "gcc -shared -o " + outputPath + " " + objectFiles;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to create dynamic library." << std::endl;
        exit(1);
    }
}

void compileStaticLibrary(std::vector<std::string> modulePaths, std::string objectsOutputPath, std::string outputPath)
{
    std::string objectFiles;
    for (auto &&modulePath : modulePaths)
    {
        objectFiles += modulePath + " ";
    }

    std::string command = "ar rcs " + outputPath + " " + objectFiles;
    int result = system(command.c_str());
    if (result != 0)
    {
        std::cerr << "(Error) Failed to create static library." << std::endl;
        exit(1);
    }
}
