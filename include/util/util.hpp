#ifndef UTIL_HPP
#define UTIL_HPP

#include <string>
#include <cstdio>
#include <vector>

namespace util
{
    bool hasFileExtension(const std::string &filename, const std::string &expectedExtension);
    void checkInputFileExtension(const std::string &filename);
    std::vector<std::string> split(const std::string &str, char delimiter);
    void printColoredText(const std::string &text,
                          const std::string &foregroundColor,
                          const std::string &backgroundColor);
    std::string readFileContent(const std::string &inputFile);
    std::string getFileNameWithStem(const std::string &filePath);
    bool isDirectory(const std::string &path);
    void ensureDirectoryExists(const std::string &path);
    void isValidModuleName(const std::string &moduleName, const std::string &fileName);
    void displayErrorPanel(const std::string &fileName, const std::string &fileContent, const int errorLineNumber, const std::string &errorMsg);
} // namespace util

#endif // UTIL_HPP