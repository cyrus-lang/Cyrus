cmake_minimum_required(VERSION 3.30)

project(ParserTests)

set(CMAKE_CXX_STANDARD ${CMAKE_CXX_STANDARD})
set(CMAKE_CXX_STANDARD_REQUIRED ON)

enable_testing()

include(FetchContent)

FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.17.0.zip
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

FetchContent_MakeAvailable(googletest)

set(gtest_force_shared_crt ON CACHE INTERNAL "" FORCE)

add_executable(parser_test parser_test.cpp)

target_link_libraries(parser_test cyrus_lib gtest_main)

target_include_directories(parser_test PRIVATE ${CMAKE_SOURCE_DIR}/include)

include(CTest)