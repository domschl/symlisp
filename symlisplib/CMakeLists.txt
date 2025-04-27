# filepath: /Users/dsc/Codeberg/SymLisp/symlisplib/CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

# Define the static library for the core components
add_library(symlisplib STATIC
    sl_core.c
    sl_parse.c
    sl_env.c
)

# Make the header files in the current directory available
# to targets linking against this library.
# Also add GMP include directories publicly because sl_core.h includes gmp.h
target_include_directories(symlisplib PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${GMP_INCLUDE_DIRS} # Add GMP include path
)

# Link the library against GMP.
# Use PUBLIC because the interface (sl_core.h) exposes GMP types.
target_link_libraries(symlisplib PUBLIC
    ${GMP_LIBRARIES} # Link against GMP
)