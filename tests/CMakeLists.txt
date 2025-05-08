cmake_minimum_required(VERSION 3.10)
project(SymLispTests C)

# Add the tester executable
# Note: CMAKE_SOURCE_DIR here refers to the top-level project directory
add_executable(symlisp_tester symlisp_tester.c)

# Link against the SymLisp library (which is built in the parent scope)
target_link_libraries(symlisp_tester PRIVATE symlisplib)

# Include directories for the tester if it needs headers from the library
target_include_directories(symlisp_tester PRIVATE ${CMAKE_SOURCE_DIR}/symlisplib) # Adjust if your library headers are elsewhere

# --- Testing Support ---
enable_testing()

# Find leak checking tools
find_program(VALGRIND_COMMAND valgrind)
find_program(LEAKS_COMMAND leaks)

# Define the directory containing test case files
# CMAKE_CURRENT_SOURCE_DIR in this file is the "tests" directory
set(TESTCASE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/testcases)

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
        set_tests_properties(${TEST_NAME}_Leaks PROPERTIES WILL_FAIL OFF)
    endif()

    # --- Leak Check Run (Linux with Valgrind) ---
    if(CMAKE_SYSTEM_NAME STREQUAL "Linux" AND VALGRIND_COMMAND)
        add_test(
            NAME ${TEST_NAME}_Valgrind
            COMMAND ${VALGRIND_COMMAND} --leak-check=full --error-exitcode=1 -- $<TARGET_FILE:symlisp_tester> ${TEST_FILE}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        )
        set_tests_properties(${TEST_NAME}_Valgrind PROPERTIES WILL_FAIL OFF)
    endif()
endforeach()