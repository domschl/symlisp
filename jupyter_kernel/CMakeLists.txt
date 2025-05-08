# SymLisp Jupyter kernel build configuration

# Create build directory for the kernel
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/symlisp_kernel)

# Copy Python package files to build directory
file(GLOB KERNEL_PY_FILES "${CMAKE_CURRENT_SOURCE_DIR}/symlisp_kernel/*.py")
foreach(PY_FILE ${KERNEL_PY_FILES})
    get_filename_component(FILE_NAME ${PY_FILE} NAME)
    configure_file(
        ${PY_FILE}
        ${CMAKE_CURRENT_BINARY_DIR}/symlisp_kernel/${FILE_NAME}
        COPYONLY
    )
endforeach()

# Copy pyproject.toml
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/pyproject.toml
    ${CMAKE_CURRENT_BINARY_DIR}/pyproject.toml
    COPYONLY
)

# Add custom target for creating Python virtual environment and installing dependencies
add_custom_target(setup_kernel_env
    # Check if uv is available
    COMMAND ${CMAKE_COMMAND} -E echo "Checking for uv..."
    COMMAND which uv || (echo "ERROR: 'uv' command not found. Please install uv first: https://github.com/astral-sh/uv" && exit 1)
    
    # Create venv with uv
    COMMAND ${CMAKE_COMMAND} -E echo "Creating virtual environment..."
    COMMAND cd ${CMAKE_CURRENT_BINARY_DIR} && uv venv
    
    # Install dependencies from pyproject.toml
    COMMAND ${CMAKE_COMMAND} -E echo "Installing dependencies..."
    COMMAND cd ${CMAKE_CURRENT_BINARY_DIR} && uv sync
    
    # Install the kernel package in development mode
    COMMAND ${CMAKE_COMMAND} -E echo "Installing SymLisp kernel package..."
    COMMAND cd ${CMAKE_CURRENT_BINARY_DIR} && uv pip install -e .
    
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Setting up Python environment for SymLisp kernel"
)

# Create a modified kernel.json that points to our virtual environment Python
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/kernel_modified.json.in
"{
  \"argv\": [
    \"${CMAKE_CURRENT_BINARY_DIR}/.venv/bin/python\",
    \"-m\", \"symlisp_kernel\",
    \"-f\", \"{connection_file}\"
  ],
  \"display_name\": \"SymLisp\",
  \"language\": \"scheme\",
  \"env\": {
    \"SYMLISP_LIB_PATH\": \"${CMAKE_BINARY_DIR}/symlisplib\",
    \"SYMLISP_STDLIB_PATH\": \"${CMAKE_SOURCE_DIR}/stdsymlisp\"
  }
}")

# Configure the modified kernel.json
configure_file(
    ${CMAKE_CURRENT_BINARY_DIR}/kernel_modified.json.in
    ${CMAKE_CURRENT_BINARY_DIR}/kernel_modified.json
    @ONLY
)

# Create a verification script
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/verify_kernel.py
"import jupyter_client.kernelspec
print('SymLisp kernel specs:', jupyter_client.kernelspec.find_kernel_specs())
")

# Add custom target to install the kernel
add_custom_target(install_jupyter_kernel
    # Create a temporary kernel directory for installation
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/kernel_install
    
    # Copy the modified kernel.json that references our specific venv Python
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/kernel_modified.json 
            ${CMAKE_CURRENT_BINARY_DIR}/kernel_install/kernel.json
    
    # Install the kernel using jupyter kernelspec install
    COMMAND ${CMAKE_COMMAND} -E env SYMLISP_LIB_PATH=${CMAKE_BINARY_DIR}/symlisplib
            ${CMAKE_CURRENT_BINARY_DIR}/.venv/bin/python -m jupyter kernelspec install 
            --user --name=symlisp ${CMAKE_CURRENT_BINARY_DIR}/kernel_install
            
    # Ensure the module is properly installed and accessible
    COMMAND ${CMAKE_COMMAND} -E echo "Verifying kernel installation..."
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/.venv/bin/python ${CMAKE_CURRENT_BINARY_DIR}/verify_kernel.py
    
    DEPENDS symlisplib setup_kernel_env
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Installing SymLisp Jupyter kernel"
)

# Add convenient target to launch Jupyter notebook
add_custom_target(jupyter_notebook
    COMMAND ${CMAKE_CURRENT_BINARY_DIR}/.venv/bin/jupyter notebook
    DEPENDS install_jupyter_kernel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Starting Jupyter Notebook with SymLisp kernel"
)
