import argparse
import json
import os
import sys

from jupyter_client.kernelspec import KernelSpecManager
from IPython.utils.tempdir import TemporaryDirectory

def install_kernel_spec(kernel_json, user=True, prefix=None):
    """Install the kernel spec directory into the user's kernel directory."""
    with TemporaryDirectory() as td:
        # Copy the kernel.json file
        os.chmod(td, 0o755)  # Jupyter requires this permission
        with open(os.path.join(td, 'kernel.json'), 'w') as f:
            json.dump(kernel_json, f, indent=2)
        
        # Install the kernel spec
        print('Installing SymLisp kernel spec')
        KernelSpecManager().install_kernel_spec(td, 'symlisp', user=user, prefix=prefix)
        print(f"Installed kernelspec symlisp in {'user' if user else 'system'} directory")

def main(argv=None):
    parser = argparse.ArgumentParser(description='Install the SymLisp Jupyter kernel')
    parser.add_argument('--user', action='store_true', help='Install to the per-user kernel registry')
    parser.add_argument('--prefix', help='Specify a prefix to install to')
    parser.add_argument('--kernel-json', help='Path to kernel.json file')
    parser.add_argument('--lib-path', help='Path to SymLisp library')
    
    args = parser.parse_args(argv)
    
    # Load the kernel.json file
    if args.kernel_json:
        with open(args.kernel_json, 'r') as f:
            kernel_json = json.load(f)
            
        # Update the environment variables if a library path is provided
        if args.lib_path:
            if 'env' not in kernel_json:
                kernel_json['env'] = {}
            kernel_json['env']['SYMLISP_LIB_PATH'] = args.lib_path
    else:
        # Default kernel.json if no file provided
        kernel_json = {
            "argv": [sys.executable, "-m", "symlisp_kernel", "-f", "{connection_file}"],
            "display_name": "SymLisp",
            "language": "scheme",
            "env": {}
        }
        if args.lib_path:
            kernel_json['env']['SYMLISP_LIB_PATH'] = args.lib_path
    
    install_kernel_spec(kernel_json, user=args.user, prefix=args.prefix)

if __name__ == '__main__':
    main()