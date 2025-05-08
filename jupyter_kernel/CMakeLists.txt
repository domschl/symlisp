cmake_minimum_required(VERSION 3.10)
project(SymLispJupyterPythonKernel NONE) # No C/C++ compilation needed here directly

# This CMakeLists.txt is primarily for organization and potential installation steps.
# The actual kernel is Python-based and will use the symlisplib shared library.

# Define a variable for the kernel name
set(SYMLISP_KERNEL_NAME "symlisp")
set(SYMLISP_KERNEL_DISPLAY_NAME "SymLisp")

# Path to the system Python interpreter (used as base for venv)
find_package(Python3 REQUIRED COMPONENTS Interpreter Development)

# Determine the installation directory for the Jupyter kernel FIRST
set(JUPYTER_USER_SPECIFIC_PATH "")
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin") # macOS
    if(DEFINED ENV{HOME})
        set(JUPYTER_USER_SPECIFIC_PATH "$ENV{HOME}/Library/Jupyter/kernels")
    endif()
elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    if(DEFINED ENV{APPDATA})
        set(JUPYTER_USER_SPECIFIC_PATH "$ENV{APPDATA}/jupyter/kernels")
    endif()
else() # Primarily for Linux, but as a general fallback for Unix-like
    if(DEFINED ENV{HOME})
        set(JUPYTER_USER_SPECIFIC_PATH "$ENV{HOME}/.local/share/jupyter/kernels")
    endif()
endif()

if(JUPYTER_USER_SPECIFIC_PATH AND IS_ABSOLUTE "${JUPYTER_USER_SPECIFIC_PATH}")
    set(KERNEL_INSTALL_DESTINATION "${JUPYTER_USER_SPECIFIC_PATH}/${SYMLISP_KERNEL_NAME}")
    message(STATUS "Jupyter kernel will be installed to user directory: ${KERNEL_INSTALL_DESTINATION}")
else()
    # Fallback to CMAKE_INSTALL_PREFIX based path if user dir couldn't be determined
    set(KERNEL_INSTALL_DESTINATION "share/jupyter/kernels/${SYMLISP_KERNEL_NAME}")
    message(STATUS "Jupyter kernel install path will be relative to CMAKE_INSTALL_PREFIX: ${KERNEL_INSTALL_DESTINATION}")
    if(NOT (DEFINED ENV{HOME} OR DEFINED ENV{APPDATA}))
        message(STATUS " (Reason: ENV{HOME} (Unix-like) or ENV{APPDATA} (Windows) is not defined)")
    elseif(JUPYTER_USER_SPECIFIC_PATH)
        message(STATUS " (Reason: Determined user path '${JUPYTER_USER_SPECIFIC_PATH}' is not absolute)")
    else()
        message(STATUS " (Reason: Could not determine a platform-specific user directory)")
    endif()
endif()

# --- Define paths for kernel.json ---
# 1. Absolute path to Python executable within the .venv (in the source/build tree)
#    This assumes .venv is created in CMAKE_CURRENT_SOURCE_DIR (jupyter_kernel/)
if(WIN32)
    set(VENV_PYTHON_REL_PATH ".venv/Scripts/python.exe")
else()
    set(VENV_PYTHON_REL_PATH ".venv/bin/python")
endif()
get_filename_component(VENV_PYTHON_ABS_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${VENV_PYTHON_REL_PATH}" ABSOLUTE)
message(STATUS "Kernel.json will point to venv Python (expected in build tree): ${VENV_PYTHON_ABS_PATH}")

# 2. Absolute path to the symlisp_kernel.py script in its installed location
set(INSTALLED_KERNEL_SCRIPT_ABS_PATH "${KERNEL_INSTALL_DESTINATION}/symlisp_kernel.py")
message(STATUS "Kernel.json will point to installed script: ${INSTALLED_KERNEL_SCRIPT_ABS_PATH}")


# Create kernel.json content
set(KERNEL_JSON_CONTENT
"{
 \"argv\": [
  \"${VENV_PYTHON_ABS_PATH}\",
  \"${INSTALLED_KERNEL_SCRIPT_ABS_PATH}\",
  \"-f\", \"{connection_file}\"
 ],
 \"display_name\": \"${SYMLISP_KERNEL_DISPLAY_NAME}\",
 \"language\": \"scheme\"
}
")

# Write the kernel.json file in the build directory
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/kernel.json" "${KERNEL_JSON_CONTENT}")

# Installation rules
# Install the generated kernel.json from the build directory
install(
    FILES "${CMAKE_CURRENT_BINARY_DIR}/kernel.json"
    DESTINATION "${KERNEL_INSTALL_DESTINATION}"
    RENAME kernel.json # Ensure the installed file is named kernel.json
)

# Install the Python kernel script from the source directory
install(
    FILES symlisp_kernel.py
        # Add other Python files if your kernel is split into multiple files
    DESTINATION "${KERNEL_INSTALL_DESTINATION}"
)

# Install the symlisplib shared library to the same directory
install(TARGETS symlisplib_shared
    LIBRARY DESTINATION "${KERNEL_INSTALL_DESTINATION}" # For .so, .dylib
    RUNTIME DESTINATION "${KERNEL_INSTALL_DESTINATION}" # For .dll on Windows
    ARCHIVE DESTINATION "${KERNEL_INSTALL_DESTINATION}" # For .lib on Windows if applicable
)

# Optional: Custom target to set up Python environment using uv
find_program(UV_EXECUTABLE uv)
if(UV_EXECUTABLE AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/pyproject.toml")
    add_custom_target(setup_jupyter_env ALL
        COMMAND ${UV_EXECUTABLE} venv .venv --python ${Python3_EXECUTABLE} # Base venv on system Python
        COMMAND ${UV_EXECUTABLE} sync # Assuming pyproject.toml is used by uv sync
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Setting up Python environment for Jupyter kernel using uv (creates .venv in jupyter_kernel/)"
        VERBATIM
    )
    # add_dependencies(setup_jupyter_env symlisplib_shared)
endif()

message(STATUS "Jupyter Kernel (Python) configured. Install target will place files in: ${KERNEL_INSTALL_DESTINATION}")
message(WARNING "The installed kernel.json now points to a Python executable within the project's build-time .venv (${VENV_PYTHON_ABS_PATH}). This .venv directory must remain at this exact location for the installed kernel to work.")
message(STATUS "To make the kernel available to Jupyter during development (without full install), you might run: jupyter kernelspec install --user --name=${SYMLISP_KERNEL_NAME} path/to/your/jupyter_kernel_source_or_build_dir")
