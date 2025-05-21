cmake_minimum_required(VERSION 3.28) 

project(cyrus_language)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    message(STATUS "Configuring Debug build")
    add_compile_definitions(DEBUG_MODE)
    
    set(SANITIZER_FLAGS "-fsanitize=address,leak -fno-omit-frame-pointer")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic ${SANITIZER_FLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${SANITIZER_FLAGS}")
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    message(STATUS "Configuring Release build")
    add_compile_definitions(RELEASE_MODE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -DNDEBUG")
endif()

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
if(LLVM_FOUND)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
else()
    message(FATAL_ERROR "LLVM is required to build this project.")
endif()

# Find ZLIB
find_package(ZLIB REQUIRED) 
if(ZLIB)
  message(STATUS "Found ZLIB: ${ZLIB_EXECUTABLE}")
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
    # ${SOURCE_DIR}/codegen_llvm/*.cpp
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
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
llvm_map_components_to_libnames(llvm_libs
    core
    support 
    irreader
    nativecodegen
    mc
    X86
    target
    asmparser
    asmprinter
)

# Add third party libraries
include(FetchContent)
FetchContent_Declare(nlohmann_json URL https://github.com/nlohmann/json/releases/download/v3.12.0/json.tar.xz)
FetchContent_MakeAvailable(nlohmann_json)
target_link_libraries(cyrus_lib nlohmann_json::nlohmann_json)
include_directories(${nlohmann_json_SOURCE_DIR}/single_include/) 

# Create an executable
add_executable(cyrus
    ${SOURCE_DIR}/cli/main.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/src/lexer/cyrus.yy.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/src/parser/cyrus.tab.cpp
) 
target_link_libraries(cyrus cyrus_lib ${llvm_libs})

add_subdirectory(test/parser)