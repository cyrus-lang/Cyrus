#include <iostream>
#include <cstdlib>
#include <sys/stat.h>
#include <sys/types.h>
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

    bool isDirectory(const std::string &path)
    {
#ifdef _WIN32
        DWORD fileAttributes = GetFileAttributesA(path.c_str());
        if (fileAttributes == INVALID_FILE_ATTRIBUTES)
        {
            return false;
        }
        return (fileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;
#else
        struct stat statbuf;
        if (stat(path.c_str(), &statbuf) != 0)
        {
            return false;
        }
        return S_ISDIR(statbuf.st_mode);
#endif
    }

    void ensureDirectoryExists(const std::string &path)
    {
        if (!isDirectory(path))
        {
#ifdef _WIN32
            if (CreateDirectoryA(path.c_str(), NULL) == 0)
            {
                std::cerr << "(Error) Could not create directory '" << path << "'." << std::endl;
                std::exit(1);
            }
#else
            if (mkdir(path.c_str(), 0777) != 0)
            {
                std::cerr << "(Error) Could not create directory '" << path << "'." << std::endl;
                std::exit(1);
            }
#endif
        }
    }

    void isValidModuleName(const std::string &moduleName, const std::string &fileName)
    {
        for (char c : moduleName)
        {
            if (!std::isalnum(c) && c != '_' && c != ':')
            {
                std::cerr << "(Error) Module name '" << moduleName << "' in file '" << fileName << "' contains invalid characters. "
                          << "Only alphanumeric characters, underscores, and double colons are allowed." << std::endl;
                std::exit(1);
            }
        }
    }
} // namespace util