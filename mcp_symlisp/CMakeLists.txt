cmake_minimum_required(VERSION 3.21)  # Version 3.21+ has proper C23 support
project(MCP_SymLisp C)

# Set C standard to C23 (latest released standard)
set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

# Add source files for cJSON
set(CJSON_SOURCES
    deps/cJSON.c
)

# Add source files for MCP JSONRPC
set(MCP_SOURCES
    mcp_symlisp.c
    mcp_jsonrpc.c
)

# Add executable
add_executable(mcp_symlisp ${MCP_SOURCES} ${CJSON_SOURCES})

# Include directories and link libraries
target_include_directories(mcp_symlisp PRIVATE
    ${CMAKE_SOURCE_DIR}/symlisplib  # Path to symlisplib headers
    ${CMAKE_CURRENT_SOURCE_DIR}     # For local includes
    ${GMP_INCLUDE_DIRS}
)

target_link_libraries(mcp_symlisp PRIVATE
    symlisplib          # Link against our core library
    ${GMP_LIBRARIES}
)
