cmake_minimum_required(VERSION 3.21)  # Version 3.21+ has proper C23 support
project(SymLispREPL C)

# Set C standard to C23 (latest released standard)
set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

# Find readline package
find_package(Readline QUIET)

if(NOT Readline_FOUND)
    message(STATUS "Readline not found automatically, searching manually (macOS Homebrew paths)...")
    # macOS often needs help finding readline when installed via Homebrew
    find_path(Readline_INCLUDE_DIR readline/readline.h
        PATHS /usr/local/opt/readline/include  # Homebrew path (Intel)
              /opt/homebrew/opt/readline/include  # Homebrew on Apple Silicon
              /usr/include
              /usr/local/include
    )
    find_library(Readline_LIBRARY NAMES readline
        PATHS /usr/local/opt/readline/lib  # Homebrew path (Intel)
              /opt/homebrew/opt/readline/lib  # Homebrew on Apple Silicon
              /usr/lib
              /usr/local/lib
    )

    if(Readline_INCLUDE_DIR AND Readline_LIBRARY)
        set(Readline_FOUND TRUE)
        message(STATUS "Found readline manually: ${Readline_LIBRARY}")
    else()
        message(FATAL_ERROR "Readline library not found automatically or manually. Please install libreadline-dev or equivalent (e.g., 'brew install readline').")
    endif()
endif()

# Add executable
add_executable(symlisp symlisp.c)

# Include directories and link libraries
target_include_directories(symlisp PRIVATE
    ${Readline_INCLUDE_DIR}
    ${GMP_INCLUDE_DIRS}  # Use the variable set by find_package or manual search
)

target_link_libraries(symlisp PRIVATE
    symlisplib          # Link against our core library
    ${Readline_LIBRARY}
    ${GMP_LIBRARIES}     # Use the variable set by find_package or manual search
)


# On some systems, readline requires additional libraries
find_library(NCURSES_LIBRARY NAMES ncurses)
if(NCURSES_LIBRARY)
    target_link_libraries(symlisp PRIVATE ${NCURSES_LIBRARY})
endif()