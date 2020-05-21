cmake_minimum_required(VERSION 3.2)

project(rill)

# Base compiler
add_subdirectory(rillc)

# Libraries
add_subdirectory(corelib)
add_subdirectory(stdlib)
