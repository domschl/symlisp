cmake_minimum_required(VERSION 3.10)
project(SymLisp C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # Add this line

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)

# Find GMP package
find_package(GMP QUIET) # Try finding it automatically first, but don't fail yet

if(NOT GMP_FOUND)
    message(STATUS "GMP not found automatically, searching manually (macOS Homebrew paths)...")
    # macOS often needs help finding GMP when installed via Homebrew
    find_path(GMP_INCLUDE_DIR_MANUAL gmp.h
        PATHS /usr/local/opt/gmp/include  # Homebrew path (Intel)
              /opt/homebrew/opt/gmp/include  # Homebrew path (Apple Silicon)
              /usr/include
              /usr/local/include
    )
    find_library(GMP_LIBRARY_MANUAL NAMES gmp
        PATHS /usr/local/opt/gmp/lib  # Homebrew path (Intel)
              /opt/homebrew/opt/gmp/lib  # Homebrew path (Apple Silicon)
              /usr/lib
              /usr/local/lib
    )

    if(GMP_INCLUDE_DIR_MANUAL AND GMP_LIBRARY_MANUAL)
        set(GMP_FOUND TRUE)
        set(GMP_INCLUDE_DIRS ${GMP_INCLUDE_DIR_MANUAL})
        set(GMP_LIBRARIES ${GMP_LIBRARY_MANUAL})
        message(STATUS "Found GMP manually: ${GMP_LIBRARIES}")
    else()
        message(FATAL_ERROR "GMP library not found automatically or manually. Please install libgmp-dev or equivalent (e.g., 'brew install gmp').")
    endif()
endif()

# Add the source directory which defines the core library
add_subdirectory(symlisplib)

# Add the REPL directory which defines the executable
add_subdirectory(repl)

# Add other subdirectories like tests if needed
# add_subdirectory(tests)

# In your CMakeLists.txt (likely in the root or a 'tests' subdirectory)

# Add the tester executable
add_executable(symlisp_tester tests/symlisp_tester.c) # Adjust path if needed

# Link against the SymLisp library
target_link_libraries(symlisp_tester PRIVATE symlisplib) # Assuming your library target is symlisplib

# Optional: Add a test using CTest
# add_test(NAME SimpleMathTest COMMAND symlisp_tester ${CMAKE_CURRENT_SOURCE_DIR}/tests/testcases/simple_math.scm)

# --- Testing Support ---
enable_testing()

# Find leak checking tools
find_program(VALGRIND_COMMAND valgrind)
find_program(LEAKS_COMMAND leaks)

# Define the directory containing test case files
set(TESTCASE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tests/testcases)

# Find all Scheme test files
file(GLOB TEST_FILES "${TESTCASE_DIR}/*.scm")

# Loop through each test file and create CTest entries
foreach(TEST_FILE ${TEST_FILES})
    # Extract a base name for the test (e.g., "simple_math" from ".../simple_math.scm")
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)

    # --- Standard Test Run ---
    add_test(
        NAME ${TEST_NAME}
        COMMAND $<TARGET_FILE:symlisp_tester> ${TEST_FILE}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR} # Or wherever symlisp_tester needs to run from
    )
    set_tests_properties(${TEST_NAME} PROPERTIES WILL_FAIL OFF) # Expect success (exit code 0)

    # --- Leak Check Run (macOS) ---
    if(CMAKE_SYSTEM_NAME STREQUAL "Darwin" AND LEAKS_COMMAND)
        add_test(
            NAME ${TEST_NAME}_Leaks
            COMMAND ${LEAKS_COMMAND} --atExit -- $<TARGET_FILE:symlisp_tester> ${TEST_FILE}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        )
        # leaks --atExit returns non-zero if leaks are found OR if the program returns non-zero
        set_tests_properties(${TEST_NAME}_Leaks PROPERTIES WILL_FAIL OFF) # Expect success (exit code 0)
    endif()

    # --- Leak Check Run (Linux with Valgrind) ---
    if(CMAKE_SYSTEM_NAME STREQUAL "Linux" AND VALGRIND_COMMAND)
        add_test(
            NAME ${TEST_NAME}_Valgrind
            COMMAND ${VALGRIND_COMMAND} --leak-check=full --error-exitcode=1 -- $<TARGET_FILE:symlisp_tester> ${TEST_FILE}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        )
        # valgrind --error-exitcode=1 returns 1 if leaks found, otherwise returns program's exit code.
        # CTest fails on non-zero exit code.
        set_tests_properties(${TEST_NAME}_Valgrind PROPERTIES WILL_FAIL OFF) # Expect success (exit code 0)
    endif()

endforeach()