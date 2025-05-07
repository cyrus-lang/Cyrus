#include <iostream>
#include "util/util.hpp"

namespace util
{
    void isValidModuleName(const std::string &moduleName, const std::string &fileName)
    {
        if (moduleName.empty())
        {
            std::cerr << "(Error) File name cannot be empty: " << fileName << std::endl;
            exit(1);
        }

        if (std::isdigit(moduleName[0]))
        {
            std::cerr << "(Error) File name cannot start with a digit: " << fileName << std::endl;
            exit(1);
        }

        for (char c : moduleName)
        {
            if (!std::isalnum(c))
            {
                std::cerr << "(Error) File name contains invalid characters: " << fileName << std::endl;
                exit(1);
            }
        }
    }
} // namespace util
