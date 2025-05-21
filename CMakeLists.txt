cmake_minimum_required(VERSION 3.10)
project(SymLisp C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # Add this line

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)

# ASAN option (address sanitizer). Note: NOT compatible with leak-checkers like valgrind! Do not use at the same time.
option(USE_ASAN "Enable AddressSanitizer" OFF)

if(USE_ASAN)
    message(STATUS "AddressSanitizer enabled")
    # Add ASan flags to compiler and linker
    # Using add_compile_options/add_link_options is generally preferred over modifying CMAKE_*_FLAGS directly
    # but modifying the global flags is often simpler for sanitizers applied project-wide.
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address -g")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=address")
    # If you build shared libraries, uncomment the next line:
    # set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -fsanitize=address")
endif()
# --- End ASan Option ---

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

# Add the MCP server directory
add_subdirectory(mcp_symlisp)

# Add the Jupyter kernel directory
add_subdirectory(jupyter_kernel)

# --- Testing Support ---
enable_testing()

# Add other subdirectories like tests if needed
add_subdirectory(tests)

