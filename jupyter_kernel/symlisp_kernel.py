import ctypes
import os
import platform
import sys
import traceback
from ipykernel.kernelbase import Kernel

# --- SymLisp C Library Interaction ---

# Define sl_object structure (opaque pointer for Python)
class SLObject(ctypes.Structure):
    pass

SLObjectPtr = ctypes.POINTER(SLObject)

# Define sl_object_type enum (mirror from sl_core.h)
SL_TYPE_NIL = 1
SL_TYPE_HTML = 11
SL_TYPE_MARKDOWN = 12
# Add other types if needed for more detailed inspection

class SymLispLib:
    def __init__(self, library_path=None):
        self.lib = None
        self._load_library(library_path)
        self._define_prototypes()
        self._initialize_symlisp()

    def _find_lib(self):
        """Tries to find the symlisplib shared library."""
        lib_name = "libsymlisplib"
        system = platform.system()
        if system == "Windows":
            lib_ext = ".dll"
        elif system == "Darwin": # macOS
            lib_ext = ".dylib"
        else: # Linux and other Unix-like
            lib_ext = ".so"
        
        full_lib_name = lib_name + lib_ext

        # Paths to check:
        # 1. Next to this script (common for development/bundling)
        # 2. Build directory (relative to project root, common for CMake builds)
        # 3. Standard library paths (via ctypes.util.find_library)
        
        script_dir = os.path.dirname(os.path.abspath(__file__))
        
        # Path relative to this script (e.g., if lib is in ../symlisplib/build)
        # This needs to be robust. For a proper install, lib might be in system path.
        # For development, it's often in a build folder.
        
        # Try ../build/symlisplib (typical CMake out-of-source build)
        # Assumes jupyter_kernel is a subdir of the project root
        project_root = os.path.dirname(script_dir) 
        build_path_guess = os.path.join(project_root, "build", "symlisplib", full_lib_name)
        
        paths_to_try = [
            os.path.join(script_dir, full_lib_name), # Next to script
            build_path_guess,
            ctypes.util.find_library("symlisplib") # System paths
        ]
        
        # Add path if running from SymLisp/build/jupyter_kernel and lib is in SymLisp/build/symlisplib
        if "build" in script_dir:
             build_root = script_dir
             while os.path.basename(build_root) != "build" and os.path.dirname(build_root) != build_root :
                 build_root = os.path.dirname(build_root)
             if os.path.basename(build_root) == "build":
                paths_to_try.append(os.path.join(build_root, "symlisplib", full_lib_name))


        for path_candidate in paths_to_try:
            if path_candidate and os.path.exists(path_candidate):
                return path_candidate
        return None


    def _load_library(self, library_path):
        if library_path is None:
            library_path = self._find_lib()

        if library_path is None or not os.path.exists(library_path):
            # Fallback for macOS if find_library didn't work well with symlisplib
            if platform.system() == "Darwin" and ctypes.util.find_library("gmp"): # Check if gmp is findable
                 # Try common Homebrew paths if lib not found
                homebrew_intel_path = f"/usr/local/opt/symlisp/lib/libsymlisplib.dylib" # Placeholder if you install symlisp via brew
                homebrew_arm_path = f"/opt/homebrew/lib/libsymlisplib.dylib" # Placeholder
                if os.path.exists(homebrew_arm_path): library_path = homebrew_arm_path
                elif os.path.exists(homebrew_intel_path): library_path = homebrew_intel_path

        if library_path is None or not os.path.exists(library_path):
            raise ImportError(f"SymLisp shared library (libsymlisplib) not found. Tried paths: {self._find_lib() if not library_path else library_path}")
        
        self.lib = ctypes.CDLL(library_path)
        print(f"SymLisp Kernel: Loaded shared library from {library_path}", file=sys.__stderr__)


    def _define_prototypes(self):
        # Memory and Environment
        self.lib.sl_mem_init.argtypes = [ctypes.c_size_t]
        self.lib.sl_mem_init.restype = None
        self.lib.sl_mem_shutdown.argtypes = []
        self.lib.sl_mem_shutdown.restype = None
        self.lib.sl_env_create.argtypes = [SLObjectPtr]
        self.lib.sl_env_create.restype = SLObjectPtr
        self.lib.sl_builtins_init.argtypes = [SLObjectPtr]
        self.lib.sl_builtins_init.restype = None
        self.lib.sl_gc_add_root.argtypes = [ctypes.POINTER(SLObjectPtr)]
        self.lib.sl_gc_add_root.restype = None

        # Evaluation and String Conversion
        self.lib.sl_eval_string.argtypes = [ctypes.c_char_p, SLObjectPtr]
        self.lib.sl_eval_string.restype = SLObjectPtr
        self.lib.sl_object_to_string.argtypes = [SLObjectPtr]
        self.lib.sl_object_to_string.restype = ctypes.c_char_p # Returns malloc'd string
        
        # Error and Type Handling (assuming you add these helpers to sl_core.c/h)
        self.lib.sl_is_error.argtypes = [SLObjectPtr]
        self.lib.sl_is_error.restype = ctypes.c_bool
        self.lib.sl_error_message.argtypes = [SLObjectPtr]
        self.lib.sl_error_message.restype = ctypes.c_char_p # Points to internal string

        self.lib.sl_get_object_type.argtypes = [SLObjectPtr]
        self.lib.sl_get_object_type.restype = ctypes.c_int # sl_object_type enum

        self.lib.sl_get_rich_content_html.argtypes = [SLObjectPtr]
        self.lib.sl_get_rich_content_html.restype = ctypes.c_char_p # Points to internal string
        self.lib.sl_get_rich_content_markdown.argtypes = [SLObjectPtr]
        self.lib.sl_get_rich_content_markdown.restype = ctypes.c_char_p # Points to internal string
        
        # Function to free strings returned by sl_object_to_string
        self.lib.sl_free_c_string.argtypes = [ctypes.c_char_p]
        self.lib.sl_free_c_string.restype = None
        
        # Globals (accessing them directly as pointers)
        # sl_global_env_ptr = SLObjectPtr.in_dll(self.lib, "sl_global_env")
        # For this to work, sl_global_env must be exported correctly.
        # It's safer to pass it around or have C functions that operate on it.
        # For now, we'll create and pass the env.

    def _initialize_symlisp(self):
        self.lib.sl_mem_init(0) # 0 for default initial chunk size
        self.global_env = self.lib.sl_env_create(None) # No parent for global env
        if not self.global_env:
            raise RuntimeError("Failed to create SymLisp global environment.")
        
        # IMPORTANT: Root the global environment
        # We need to pass a pointer to our Python variable holding the SLObjectPtr
        self.global_env_ptr_storage = SLObjectPtr(self.global_env) # Store it so pointer remains valid
        self.lib.sl_gc_add_root(ctypes.byref(self.global_env_ptr_storage))
        
        self.lib.sl_builtins_init(self.global_env)
        print("SymLisp Kernel: SymLisp environment initialized.", file=sys.__stderr__)

    def eval_code(self, code_str):
        code_bytes = code_str.encode('utf-8')
        result_obj = self.lib.sl_eval_string(code_bytes, self.global_env)
        return result_obj

    def object_to_string(self, obj_ptr):
        if not obj_ptr: return ""
        c_str = self.lib.sl_object_to_string(obj_ptr)
        py_str = ""
        if c_str:
            try:
                py_str = c_str.decode('utf-8')
            except UnicodeDecodeError:
                py_str = str(c_str) # Fallback
            self.lib.sl_free_c_string(c_str) # Free the C string
        return py_str

    def get_object_type(self, obj_ptr):
        if not obj_ptr: return SL_TYPE_NIL # Or an error/unknown type
        return self.lib.sl_get_object_type(obj_ptr)

    def is_error(self, obj_ptr):
        if not obj_ptr: return False
        return self.lib.sl_is_error(obj_ptr)

    def get_error_message(self, obj_ptr):
        if not obj_ptr: return ""
        # This returns a pointer to an internal C string, do not free from Python
        # unless the C API changes to return a copy.
        msg_ptr = self.lib.sl_error_message(obj_ptr)
        return msg_ptr.decode('utf-8') if msg_ptr else "Unknown error"

    def get_html_content(self, obj_ptr):
        if not obj_ptr: return None
        content_ptr = self.lib.sl_get_rich_content_html(obj_ptr)
        return content_ptr.decode('utf-8') if content_ptr else None

    def get_markdown_content(self, obj_ptr):
        if not obj_ptr: return None
        content_ptr = self.lib.sl_get_rich_content_markdown(obj_ptr)
        return content_ptr.decode('utf-8') if content_ptr else None

    def shutdown(self):
        # In a real scenario, you'd need to ensure GC roots are removed if not handled by shutdown
        self.lib.sl_mem_shutdown()
        print("SymLisp Kernel: SymLisp environment shut down.", file=sys.__stderr__)


class SymLispKernel(Kernel):
    implementation = 'symlisp_kernel'
    implementation_version = '0.1'
    language = 'symlisp' # Will be used by CodeMirror etc.
    language_version = '0.1' # Version of SymLisp
    language_info = {
        'name': 'SymLisp',
        'mimetype': 'text/x-scheme', # Common Lisp/Scheme mimetype
        'file_extension': '.sl',
    }
    banner = "SymLisp Kernel - Your Lisp Adventure Starts Here!"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.symlisp_lib = SymLispLib()

    def do_shutdown(self, restart):
        self.symlisp_lib.shutdown()
        return super().do_shutdown(restart)

    def do_execute(self, code, silent, store_history=True, user_expressions=None, allow_stdin=False):
        if not code.strip():
            return {'status': 'ok', 'execution_count': self.execution_count,
                    'payload': [], 'user_expressions': {}}

        try:
            result_obj = self.symlisp_lib.eval_code(code)
            
            # Check for errors first
            if self.symlisp_lib.is_error(result_obj):
                error_message = self.symlisp_lib.get_error_message(result_obj)
                error_content = {
                    'ename': 'SymLispError',
                    'evalue': error_message,
                    'traceback': [] # Can be populated if SymLisp provides tracebacks
                }
                self.send_response(self.iopub_socket, 'error', error_content)
                return {'status': 'error', 'ename': 'SymLispError', 'evalue': error_message,
                        'traceback': [], 'execution_count': self.execution_count}

            obj_type = self.symlisp_lib.get_object_type(result_obj)

            # Handle rich output types
            if obj_type == SL_TYPE_HTML:
                html_content = self.symlisp_lib.get_html_content(result_obj)
                if html_content and not silent:
                    display_data = {
                        'data': {'text/html': html_content},
                        'metadata': {}
                    }
                    self.send_response(self.iopub_socket, 'display_data', display_data)
            elif obj_type == SL_TYPE_MARKDOWN:
                md_content = self.symlisp_lib.get_markdown_content(result_obj)
                if md_content and not silent:
                    display_data = {
                        'data': {'text/markdown': md_content},
                        'metadata': {}
                    }
                    self.send_response(self.iopub_socket, 'display_data', display_data)
            
            # For other types, or if the rich display was just a side effect of (display-html)
            # and the final result is something else (e.g. (begin (display-html ...) (+ 1 2)))
            # we might want to display the final result as text/plain if it's not NIL.
            # The current (display-html) returns an HTML object, so this branch might not be hit
            # if the last expression was (display-html).
            # If the result is NIL (e.g. from define, or if display-html returned NIL), don't send execute_result.
            elif obj_type != SL_TYPE_NIL and not silent : # Don't display NIL results explicitly
                plain_result_str = self.symlisp_lib.object_to_string(result_obj)
                if plain_result_str: # Ensure there's something to display
                    stream_content = {'name': 'stdout', 'text': plain_result_str}
                    self.send_response(self.iopub_socket, 'stream', stream_content)
            
            # Note: (display "text") in SymLisp currently prints to C's stdout.
            # To capture that for Jupyter, you'd need to redirect C's stdout
            # within the C library when called from the kernel, or have (display)
            # return a special object that the kernel can interpret as a stream output.
            # The current setup sends the *result* of evaluation.

            return {'status': 'ok', 'execution_count': self.execution_count,
                    'payload': [], 'user_expressions': {}}

        except Exception as e:
            self.log.error(traceback.format_exc())
            error_content = {
                'ename': e.__class__.__name__,
                'evalue': str(e),
                'traceback': traceback.format_exc().splitlines()
            }
            self.send_response(self.iopub_socket, 'error', error_content)
            return {'status': 'error', 'ename': e.__class__.__name__, 'evalue': str(e),
                    'traceback': traceback.format_exc().splitlines(), 'execution_count': self.execution_count}

if __name__ == '__main__':
    # This allows running the kernel directly using: python -m symlisp_kernel
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=SymLispKernel)
