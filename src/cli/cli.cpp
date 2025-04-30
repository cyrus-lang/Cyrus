#include <iostream>
#include <vector>
#include <map>
#include <string>

std::map<std::string, std::vector<std::string>> parseArguments(const std::vector<std::string>& args) {
    std::map<std::string, std::vector<std::string>> parsedArgs;
    std::string currentCommand;

    for (const auto& arg : args) {
        if (arg.substr(0, 2) == "--") {
            currentCommand = arg;
            parsedArgs[currentCommand] = {};
        } else if (!currentCommand.empty()) {
            parsedArgs[currentCommand].push_back(arg);
        } else {
            parsedArgs["command"].push_back(arg);
        }
    }
    return parsedArgs;
}