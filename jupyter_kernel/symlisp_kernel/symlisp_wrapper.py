import os
import sys
import traceback
from cffi import FFI

class EvaluationResult:
    def __init__(self):
        self.has_error = False
        self.error_message = None
        self.standard_output = None
        self.html_content = None
        self.markdown_content = None
        self.return_value = None

class SymLispError(Exception):
    pass

class SymLispWrapper:
    def __init__(self, lib_path=''):
        self.ffi = FFI()
        
        # Define the C function signatures that we'll use
        self.ffi.cdef("""
            typedef struct SLObject SLObject;
            
            void sl_mem_init(size_t initial_heap_size);
            void sl_mem_shutdown();
            
            SLObject* sl_env_create(SLObject* outer_env);
            void sl_builtins_init(SLObject* global_env);
            
            SLObject* sl_eval_string(const char* code, SLObject* env);
            int sl_is_error(SLObject* obj);
            const char* sl_error_message(SLObject* obj);
            char* sl_object_to_string(SLObject* obj);
            void sl_free_c_string(const char* str);
            
            // Optional rich content functions
            const char* sl_get_rich_content_html(SLObject* obj);
            const char* sl_get_rich_content_markdown(SLObject* obj);
            
            // GC functions
            void sl_gc_add_root(SLObject** root_ptr);
            void sl_gc_remove_root(SLObject** root_ptr);
            void sl_gc();
            
            // Common objects
            extern SLObject* SL_NIL;
            extern SLObject* SL_TRUE;
            extern SLObject* SL_FALSE;
            extern SLObject* SL_OUT_OF_MEMORY_ERROR;
        """)
        
        # Load the SymLisp library
        try:
            if not lib_path:
                raise SymLispError("No library path provided")
            
            if not os.path.exists(lib_path):
                raise SymLispError(f"Library path not found: {lib_path}")
            
            # Try to find the library file
            lib_file = None
            if os.path.isdir(lib_path):
                for file in os.listdir(lib_path):
                    if file.startswith("libsymlisp") and (file.endswith(".so") or file.endswith(".dylib") or file.endswith(".dll")):
                        lib_file = os.path.join(lib_path, file)
                        break
            
            if not lib_file:
                raise SymLispError(f"Could not find SymLisp library in {lib_path}")
            
            print(f"Loading library: {lib_file}", file=sys.stderr)
            self.lib = self.ffi.dlopen(lib_file)
            
            # Initialize SymLisp memory
            self.lib.sl_mem_init(0)  # Use default heap size
            
            # Create the global environment
            self.global_env = self.lib.sl_env_create(self.ffi.NULL)
            if not self.global_env:
                raise SymLispError("Failed to create global environment")
            
            # Add to GC roots for protection
            self.global_env_ptr = self.ffi.new("SLObject**")
            self.global_env_ptr[0] = self.global_env
            self.lib.sl_gc_add_root(self.global_env_ptr)
            
            # Initialize built-in functions
            self.lib.sl_builtins_init(self.global_env)
            
        except Exception as e:
            raise SymLispError(f"Error initializing SymLisp: {str(e)}")
    
    def __del__(self):
        try:
            # Remove global env from roots before shutdown
            if hasattr(self, 'global_env_ptr') and self.global_env_ptr:
                self.lib.sl_gc_remove_root(self.global_env_ptr)
            
            # Shutdown SymLisp memory
            if hasattr(self, 'lib'):
                self.lib.sl_mem_shutdown()
        except:
            pass
    
    def eval_string(self, code):
        """Evaluate SymLisp code string and return the result."""
        result = EvaluationResult()
        
        try:
            # Evaluate the code
            obj = self.lib.sl_eval_string(code.encode('utf-8'), self.global_env)
            
            # Check for errors
            if self.lib.sl_is_error(obj):
                result.has_error = True
                result.error_message = self.ffi.string(self.lib.sl_error_message(obj)).decode('utf-8')
            else:
                # Get string representation of the result
                str_result = self.lib.sl_object_to_string(obj)
                if str_result != self.ffi.NULL:
                    result.return_value = self.ffi.string(str_result).decode('utf-8')
                    self.lib.sl_free_c_string(str_result)
                
                # Try to get rich content if those functions exist
                if hasattr(self.lib, 'sl_get_rich_content_html'):
                    html_content = self.lib.sl_get_rich_content_html(obj)
                    if html_content != self.ffi.NULL:
                        result.html_content = self.ffi.string(html_content).decode('utf-8')
                
                if hasattr(self.lib, 'sl_get_rich_content_markdown'):
                    md_content = self.lib.sl_get_rich_content_markdown(obj)
                    if md_content != self.ffi.NULL:
                        result.markdown_content = self.ffi.string(md_content).decode('utf-8')
            
            # Run garbage collection after evaluation
            self.lib.sl_gc()
        
        except Exception as e:
            result.has_error = True
            result.error_message = str(e)
            print(f"Exception in eval_string: {e}", file=sys.stderr)
            print(traceback.format_exc(), file=sys.stderr)
        
        return result