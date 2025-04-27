# filepath: /Users/dsc/Codeberg/SymLisp/symlisplib/CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

# Define the static library for the core components
add_library(symlisplib STATIC
    sl_core.c
    # Add sl_parse.c when it exists
    # sl_parse.c
)

# Make the header files in the current directory available
# to targets linking against this library.
target_include_directories(symlisplib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)