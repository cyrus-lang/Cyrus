#include <iostream>
#include <vector>
#include <format>
#include "util/util.hpp"

const int errorPanelScope = 3;

namespace util
{
    void displayErrorPanel(const std::string &fileName, const std::string &fileContent, const int errorLineNumber, const std::string &errorMsg)
    {
        std::string line;
        int lineNumber = errorLineNumber;

        while (lineNumber != 1 && lineNumber - errorPanelScope == errorLineNumber)
        {
            lineNumber--;
        }

        std::vector<std::string> lines = util::split(fileContent, '\n');

        for (auto &&line : lines)
        {
            if (lineNumber >= errorLineNumber - errorPanelScope && lineNumber <= errorLineNumber + errorPanelScope) {
                if (lineNumber == errorLineNumber)
                {
                    util::printColoredText(std::format("{}| {}", lineNumber, line), "white", "red");
                }
                else
                {
                    std::cerr << lineNumber << "| " << line << std::endl;
                }

                lineNumber++;
            }
        }

        std::cout << "\n"
                  << "(Error) " << fileName << ":" << errorLineNumber << " :: " << errorMsg << std::endl;
    }
}