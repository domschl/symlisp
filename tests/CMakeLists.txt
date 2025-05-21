cmake_minimum_required(VERSION 3.10)
project(SymLispTests C)

# Option to include long-running tests
option(RUN_LONG_TESTS "Enable execution of long-running tests" OFF)

# Add the tester executable
# Note: CMAKE_SOURCE_DIR here refers to the top-level project directory
add_executable(symlisp_tester symlisp_tester.c)

# Link against the SymLisp library (which is built in the parent scope)
target_link_libraries(symlisp_tester PRIVATE symlisplib)

# Include directories for the tester if it needs headers from the library
target_include_directories(symlisp_tester PRIVATE ${CMAKE_SOURCE_DIR}/symlisplib) # Adjust if your library headers are elsewhere

# Find leak checking tools
find_program(VALGRIND_COMMAND valgrind)
find_program(LEAKS_COMMAND leaks)

# --- Regular Fast Tests ---
# Define the directory containing regular test case files
set(REGULAR_TESTCASE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/testcases)

# Find all Scheme test files for regular tests
file(GLOB REGULAR_TEST_FILES "${REGULAR_TESTCASE_DIR}/*.scm")

# Loop through each regular test file and create CTest entries
foreach(TEST_FILE ${REGULAR_TEST_FILES})
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

# --- Long Running Tests (Conditional) ---
if(RUN_LONG_TESTS)
    message(STATUS "Including long-running tests.")
    # Define the directory containing long-running test case files
    set(LONG_TESTCASE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/long_running_testcases)

    # Find all Scheme test files for long-running tests
    file(GLOB LONG_TEST_FILES "${LONG_TESTCASE_DIR}/*.scm")

    foreach(LONG_TEST_FILE ${LONG_TEST_FILES})
        get_filename_component(BASE_TEST_NAME ${LONG_TEST_FILE} NAME_WE)
        set(LONG_TEST_NAME "Long_${BASE_TEST_NAME}") # Prefix to distinguish from regular tests

        # --- Standard Long Test Run ---
        add_test(
            NAME ${LONG_TEST_NAME}
            COMMAND $<TARGET_FILE:symlisp_tester> ${LONG_TEST_FILE}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        )
        set_tests_properties(${LONG_TEST_NAME} PROPERTIES WILL_FAIL OFF)

        # --- Leak Check Long Test Run (macOS) ---
        if(CMAKE_SYSTEM_NAME STREQUAL "Darwin" AND LEAKS_COMMAND)
            add_test(
                NAME ${LONG_TEST_NAME}_Leaks
                COMMAND ${LEAKS_COMMAND} --atExit -- $<TARGET_FILE:symlisp_tester> ${LONG_TEST_FILE}
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            )
            set_tests_properties(${LONG_TEST_NAME}_Leaks PROPERTIES WILL_FAIL OFF)
        endif()

        # --- Leak Check Long Test Run (Linux with Valgrind) ---
        if(CMAKE_SYSTEM_NAME STREQUAL "Linux" AND VALGRIND_COMMAND)
            add_test(
                NAME ${LONG_TEST_NAME}_Valgrind
                COMMAND ${VALGRIND_COMMAND} --leak-check=full --error-exitcode=1 -- $<TARGET_FILE:symlisp_tester> ${LONG_TEST_FILE}
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            )
            set_tests_properties(${LONG_TEST_NAME}_Valgrind PROPERTIES WILL_FAIL OFF)
        endif()
    endforeach()
else()
    message(STATUS "Skipping long-running tests. Set RUN_LONG_TESTS=ON to include them.")
endif()