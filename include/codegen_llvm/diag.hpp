#ifndef CODEGEN_LLVM_DIAG_HPP
#define CODEGEN_LLVM_DIAG_HPP

#include "util/util.hpp"

#define DISPLAY_DIAG(line, msg)                            \
    util::displayErrorPanel(filePath_, *fileContent_, line, msg); \
    exit(1)

#endif // CODEGEN_LLVM_DIAG_HPP
