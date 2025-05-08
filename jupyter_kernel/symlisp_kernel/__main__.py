"""Main entry point for the SymLisp kernel."""
from ipykernel.kernelapp import IPKernelApp
from .kernel import SymLispKernel

if __name__ == '__main__':
    IPKernelApp.launch_instance(kernel_class=SymLispKernel)