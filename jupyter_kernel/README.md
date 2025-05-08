# SymLisp Jupyter Kernel

This is a Jupyter kernel for the SymLisp language, a Scheme-like Lisp implementation.

## Installation

The kernel is automatically installed when building the project with CMake:

```bash
mkdir build && cd build
cmake ..
make install_jupyter_kernel
```

## Manual Installation

If you prefer to install manually:

1. Install the Python package:
   ```
   cd jupyter_kernel
   pip install -e .
   ```

2. Install the kernel:
   ```
   python -m symlisp_kernel.install --user
   ```

## Usage

After installation, the SymLisp kernel will be available in Jupyter:

```bash
jupyter notebook
# or
jupyter lab
```

Select "SymLisp" from the kernel dropdown menu when creating a new notebook.

## Features

- Standard Scheme/Lisp syntax
- Special display functions:
  - `(display obj)` - standard output
  - `(display-html html-content)` - rich HTML output
  - `(display-markdown md-content)` - rich Markdown output