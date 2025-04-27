cmake_minimum_required(VERSION 3.21)  # Version 3.21+ has proper C23 support
project(SymLisp C)                    # Define project name and language

# Set C standard to C23 (latest released standard)
set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)           # Use standard C23 instead of GNU extensions

# Add executable
add_executable(symlisp repl/symlisp.c)