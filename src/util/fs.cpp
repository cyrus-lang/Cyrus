#include <iostream>
#include <cstdlib>
#include "util/util.hpp"

namespace util
{
    bool hasFileExtension(const std::string &filename, const std::string &expectedExtension)
    {
        size_t dotPos = filename.rfind('.');
        if (dotPos == std::string::npos)
        {
            return false;
        }
        return filename.substr(dotPos) == expectedExtension;
    }

    void checkInputFileExtension(const std::string &filename)
    {
        if (!hasFileExtension(filename, ".cyr"))
        {
            std::cerr << "Error: Input file '" << filename << "' does not have the required '.cyr' extension." << std::endl;
            std::exit(1);
        }
    }

    std::string readFileContent(const std::string &inputFile)
    {
        FILE *file = fopen(inputFile.c_str(), "r");
        if (!file)
        {
            std::cerr << "Error: Could not open file '" << inputFile << "'." << std::endl;
            std::exit(1);
        }

        std::string filecontent;
        char buffer[1024];
        size_t bytes_read;

        while ((bytes_read = fread(buffer, 1, sizeof(buffer), file)) > 0)
        {
            filecontent.append(buffer, bytes_read);
        }
    }
} // namespace util