#include <iostream>
#include <vector>
#include <format>
#include <unistd.h>
#include "util/util.hpp"

const int errorPanelScope = 3;

namespace util
{
    void displayErrorPanel(const std::string &fileName, const std::string &fileContent, const int errorLineNumber, const std::string &errorMsg)
    {
        std::vector<std::string> lines = util::split(fileContent, '\n');
        int startLine = std::max(1, errorLineNumber - errorPanelScope);
        int endLine = std::min(static_cast<int>(lines.size()), errorLineNumber + errorPanelScope);

        std::cout << "\n";
        for (int i = startLine; i <= endLine; ++i)
        {
            if (i == errorLineNumber)
            {
                if (isatty(fileno(stderr)))
                {
                    util::printColoredText(std::format("{:<3}| {}", i, lines[i - 1]), "white", "red");
                }
                else
                {
                    std::cerr << std::format("{:<3}| {}", i, lines[i - 1]) << std::endl;
                }
            }
            else
            {
                std::cerr << std::format("{:<3}| {}", i, lines[i - 1]) << std::endl;
            }
        }

        std::cout << "\n"
                  << "(Error) " << fileName << ":" << errorLineNumber << "  " << errorMsg << std::endl;

        std::exit(1);
    }
}
