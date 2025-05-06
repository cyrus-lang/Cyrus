cmake_minimum_required(VERSION 3.30) 

project(cyrus_language)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic") 

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
if(LLVM_FOUND)
    message(STATUS "Found LLVM: ${LLVM_PACKAGE_NAME} - version ${LLVM_VERSION}")
    include_directories(${LLVM_INCLUDE_DIRS})
    set(LLVM_LIBRARIES "")
else()
    message(FATAL_ERROR "LLVM is required to build this project.")
endif()

# Find Flex (Lex)
find_package(FLEX REQUIRED)
if(FLEX_FOUND)
  message(STATUS "Found Flex: ${FLEX_EXECUTABLE}")
endif()

# Find Bison (Yacc)
find_package(BISON REQUIRED)
if(BISON_FOUND)
  message(STATUS "Found Bison: ${BISON_EXECUTABLE}")
endif()

# Define source directories
set(SOURCE_DIR ${CMAKE_SOURCE_DIR}/src)
set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include)

include_directories(
    ${INCLUDE_DIR}
    ${SOURCE_DIR}  
)

macro(FLEX_LEXER lexer_file output_file)
    # Ensure the output directory exists before running Flex.
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src/lexer)
    add_custom_command(
        OUTPUT ${output_file}
        COMMAND ${FLEX_EXECUTABLE} -o${output_file} ${lexer_file}
        DEPENDS ${lexer_file}
        COMMENT "Generating lexer from ${lexer_file}"
    )
    set(LEXER_OUTPUT ${output_file})
endmacro()

macro(BISON_PARSER parser_file output_cpp_file output_hpp_file)
    # Ensure the output directory exists before running Bison.
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/src/parser)
    add_custom_command(
        OUTPUT ${output_cpp_file} ${output_hpp_file}
        COMMAND ${BISON_EXECUTABLE} -d -o${output_cpp_file} ${parser_file}
        DEPENDS ${parser_file}
        COMMENT "Generating parser from ${parser_file}"
    )
    set(PARSER_OUTPUT_CPP ${output_cpp_file})
    set(PARSER_OUTPUT_HPP ${output_hpp_file})
    set(PARSER_FILE ${parser_file}) # Store the parser file
endmacro()

# Generate Lexer Source
FLEX_LEXER(
    ${SOURCE_DIR}/lexer/cyrus.l
    ${CMAKE_CURRENT_BINARY_DIR}/src/lexer/cyrus.yy.cpp
)
# Add the generated lexer source file to the source list
set(set_source_files ${set_source_files} ${LEXER_OUTPUT})

# Generate Parser Source
BISON_PARSER(
    ${SOURCE_DIR}/parser/cyrus.y
    ${CMAKE_CURRENT_BINARY_DIR}/src/parser/cyrus.tab.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/src/parser/cyrus.tab.hpp
)
set(set_source_files ${set_source_files} ${PARSER_OUTPUT_CPP} ${PARSER_OUTPUT_HPP})

# Include lex & yacc generated files 
include_directories(${CMAKE_CURRENT_BINARY_DIR}/src)

# Collect all source files.
file(GLOB_RECURSE source_files
    ${SOURCE_DIR}/ast/*.cpp
    ${SOURCE_DIR}/semantic/*.cpp
    ${SOURCE_DIR}/codegen/*.cpp
    ${SOURCE_DIR}/runtime/*.cpp
    ${SOURCE_DIR}/util/*.cpp
    ${SOURCE_DIR}/cli/*.cpp
    ${SOURCE_DIR}/lexer/*.cpp
    ${SOURCE_DIR}/parser/*.cpp
)
set(source_files ${source_files} ${set_source_files})

# Create a Library
add_library(cyrus_lib ${source_files})

# Link LLVM Libraries
execute_process(
    COMMAND ${LLVM_CONFIG_PATH} --libs
    OUTPUT_VARIABLE LLVM_LIBRARIES_STRING
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(REPLACE " " ";" LLVM_LIBRARIES_LIST "${LLVM_LIBRARIES_STRING}")

target_link_libraries(cyrus_lib ${LLVM_LIBRARIES_LIST})

# Create an executable
add_executable(cyrus
    ${SOURCE_DIR}/cli/main.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/src/lexer/cyrus.yy.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/src/parser/cyrus.tab.cpp
) 
target_link_libraries(cyrus cyrus_lib)

add_subdirectory(test/parser)