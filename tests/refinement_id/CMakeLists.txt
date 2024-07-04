cmake_minimum_required(VERSION 3.11)

project(bool VERSION 0.1.0)

find_package(fmt REQUIRED)

set(target_name ${CMAKE_PROJECT_NAME})

set(sources bool.cpp)

add_executable(${target_name} ${sources})

target_compile_features(${target_name} PRIVATE cxx_std_20)

target_link_libraries(${target_name} fmt::fmt ${CMAKE_SOURCE_DIR}/out.o)
