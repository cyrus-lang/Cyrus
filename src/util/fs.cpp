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
            std::cerr << "(Error) Input file '" << filename << "' does not have the required '.cyr' extension." << std::endl;
            std::exit(1);
        }
    }

    std::string readFileContent(const std::string &inputFile)
    {
        FILE *file = fopen(inputFile.c_str(), "r");
        if (!file)
        {
            std::cerr << "(Error) Could not open file '" << inputFile << "'." << std::endl;
            std::exit(1);
        }

        std::string fileContent;
        char buffer[1024];
        size_t bytes_read;

        while ((bytes_read = fread(buffer, 1, sizeof(buffer), file)) > 0)
        {
            fileContent.append(buffer, bytes_read);
        }

        return fileContent;
    }

    std::string getFileNameWithStem(const std::string &filePath)
    {
        size_t lastSlashPos = filePath.rfind('/');
        std::string fileName = (lastSlashPos == std::string::npos) ? filePath : filePath.substr(lastSlashPos + 1);

        size_t dotPos = fileName.rfind('.');
        std::string fileNameWithStem = (dotPos == std::string::npos) ? fileName : fileName.substr(0, dotPos);

        return fileNameWithStem;
    }
} // namespace util