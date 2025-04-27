cmake_minimum_required(VERSION 3.10)
project(SymLisp C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)

# Add the source directory which defines the core library
add_subdirectory(symlisplib)

# Add the REPL directory which defines the executable
add_subdirectory(repl)

# Add other subdirectories like tests if needed
# add_subdirectory(tests)