#ifndef CLI_HEADER_HPP
#define CLI_HEADER_HPP

std::map<std::string, std::vector<std::string>> parseArguments(const std::vector<std::string>& args);

void runCommand(const std::vector<std::string>& args);
void compileCommand(const std::vector<std::string>& args);
void compileDylibCommand(const std::vector<std::string>& args);
void compileObjCommand(const std::vector<std::string>& args);
void compileAsmCommand(const std::vector<std::string>& args);
void parseOnlyCommand(const std::vector<std::string>& args);
void lexOnlyCommand(const std::vector<std::string>& args);
void helpCommand();
void versionCommand();

#endif //CLI_HEADER_HPP
