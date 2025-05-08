# filepath: /Users/dsc/Codeberg/SymLisp/symlisplib/CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

set(SYMLISPLIB_SOURCES
    sl_core.c
    sl_parse.c
    sl_env.c
    sl_eval.c
    sl_builtins.c
    sl_predicates.c
    sl_strings.c
    sl_higher_order.c
    sl_unicode_case.c
)

# Define the static library for the core components
add_library(symlisplib_static STATIC ${SYMLISPLIB_SOURCES})
set_target_properties(symlisplib_static PROPERTIES OUTPUT_NAME symlisp) # Keep original name for static lib

# Define the shared library for the core components
add_library(symlisplib_shared SHARED ${SYMLISPLIB_SOURCES})
set_target_properties(symlisplib_shared PROPERTIES OUTPUT_NAME symlisp) # Keep original name for shared lib
set_target_properties(symlisplib_shared PROPERTIES POSITION_INDEPENDENT_CODE ON)


# Make the header files in the current directory available
# to targets linking against this library.
# Also add GMP include directories publicly because sl_core.h includes gmp.h
target_include_directories(symlisplib_static PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${GMP_INCLUDE_DIRS} # Add GMP include path
)
target_include_directories(symlisplib_shared PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${GMP_INCLUDE_DIRS} # Add GMP include path
)

# Link the libraries against GMP.
# Use PUBLIC because the interface (sl_core.h) exposes GMP types.
target_link_libraries(symlisplib_static PUBLIC
    ${GMP_LIBRARIES} # Link against GMP
)
target_link_libraries(symlisplib_shared PUBLIC
    ${GMP_LIBRARIES} # Link against GMP
)

# Alias target for convenience if users expect to link against 'symlisplib'
# This will point to the static library by default.
# If you prefer the shared library by default, change symlisplib_static to symlisplib_shared.
add_library(symlisplib ALIAS symlisplib_static)