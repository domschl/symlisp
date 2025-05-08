import json
import os
import re
import sys
import traceback
from ipykernel.kernelbase import Kernel
from .symlisp_wrapper import SymLispWrapper, SymLispError

class SymLispKernel(Kernel):
    implementation = 'symlisp'
    implementation_version = '0.1'
    language = 'scheme'
    language_version = '0.1'
    language_info = {
        'name': 'symlisp',
        'mimetype': 'text/x-scheme',
        'file_extension': '.scm',
        'pygments_lexer': 'scheme',
    }
    banner = "SymLisp - A Symbolic Lisp/Scheme Interpreter"
    
    def __init__(self, **kwargs):
        try:
            super(SymLispKernel, self).__init__(**kwargs)
            
            # Use the logger from the parent class
            self.log.info("Starting SymLisp kernel initialization...")
            
            # Initialize the SymLisp interpreter
            lib_path = os.environ.get('SYMLISP_LIB_PATH', '')
            self.log.info(f"Looking for SymLisp library in: {lib_path}")
            
            if lib_path and os.path.exists(lib_path):
                self.log.info(f"Directory exists. Content: {os.listdir(lib_path)}")
            
            # Initialize the wrapper
            self.symlisp = SymLispWrapper(lib_path)
            self.log.info("SymLisp wrapper initialized successfully")
            
            # Register message handlers explicitly
            self.shell_handlers.update({
                'comm_open': self.handle_comm_open,
                'comm_msg': self.handle_comm_msg,
                'comm_close': self.handle_comm_close
            })
            
            # Initialize comms dictionary
            self.comms = {}
            self.log.info("SymLisp kernel initialization complete")
            
        except Exception as e:
            self.log.error(f"ERROR during kernel initialization: {e}")
            self.log.error(traceback.format_exc())
            raise
        
    def do_execute(self, code, silent, store_history=True, user_expressions=None, allow_stdin=False):
        try:
            if not code.strip():
                return {'status': 'ok', 'execution_count': self.execution_count,
                        'payload': [], 'user_expressions': {}}
            
            # Execute the code with the SymLisp interpreter
            result = self.symlisp.eval_string(code)
            
            # Handle the different types of results
            if not silent:
                if result.has_error:
                    error_message = result.error_message
                    # Send the error as stderr output
                    self.send_response(self.iopub_socket, 'stream', {
                        'name': 'stderr',
                        'text': f"Error: {error_message}"
                    })
                    return {'status': 'error',
                            'execution_count': self.execution_count,
                            'ename': 'EvaluationError',
                            'evalue': error_message,
                            'traceback': [error_message]}
                
                # Track if we've displayed any content
                content_displayed = False
                
                if result.standard_output:
                    self.log.info(f"Sending stdout: {result.standard_output!r}")
                    self.send_response(self.iopub_socket, 'stream', {
                        'name': 'stdout',
                        'text': result.standard_output
                    })
                    content_displayed = True
                
                if result.html_content:
                    self.log.info(f"Sending HTML content")
                    self.send_response(self.iopub_socket, 'display_data', {
                        'data': {'text/html': result.html_content},
                        'metadata': {}
                    })
                    content_displayed = True
                    # Skip showing the return value since we've displayed HTML content
                    result.return_value = None
                
                if result.markdown_content:
                    self.log.info(f"Sending Markdown content")
                    self.send_response(self.iopub_socket, 'display_data', {
                        'data': {'text/markdown': result.markdown_content},
                        'metadata': {}
                    })
                    content_displayed = True
                    # Skip showing the return value since we've displayed Markdown content
                    result.return_value = None
                
                # Send the return value if it exists and it's not just "()" after a display function
                if result.return_value and not (content_displayed and result.return_value.strip() in ["()", "#<unspecified>"]):
                    self.log.info(f"Sending return value: {result.return_value!r}")
                    data = {'text/plain': result.return_value}
                    self.send_response(self.iopub_socket, 'execute_result', {
                        'execution_count': self.execution_count,
                        'data': data,
                        'metadata': {}
                    })
        
            return {'status': 'ok',
                    'execution_count': self.execution_count,
                    'payload': [],
                    'user_expressions': {}}
                    
        except SymLispError as e:
            self.log.error(f"Runtime error: {str(e)}")
            return {'status': 'error',
                    'execution_count': self.execution_count,
                    'ename': 'RuntimeError',
                    'evalue': str(e),
                    'traceback': [str(e)]}
        except Exception as e:
            self.log.error(f"Unexpected error: {e}")
            self.log.error(traceback.format_exc())
            return {'status': 'error',
                    'execution_count': self.execution_count,
                    'ename': 'UnknownError',
                    'evalue': str(e),
                    'traceback': [str(e)]}
    
    # Handle comm_open messages
    def handle_comm_open(self, stream, ident, msg):
        """Handle comm_open messages from the frontend."""
        content = msg['content']
        comm_id = content['comm_id']
        target_name = content['target_name']
        
        # Store the comm in our dictionary
        self.comms[comm_id] = {'target_name': target_name}
        
        # Send a status message back
        self.session.send(stream, 'comm_open', 
                          {'comm_id': comm_id, 
                           'target_name': target_name,
                           'data': {'status': 'ok'}},
                          ident=ident)
    
    # Handle comm_msg messages
    def handle_comm_msg(self, stream, ident, msg):
        """Handle comm_msg messages from the frontend."""
        content = msg['content']
        comm_id = content['comm_id']
        data = content['data']
        
        # Process the message (in a real implementation, you'd do something with it)
        # For now, just acknowledge receipt
        if comm_id in self.comms:
            self.session.send(stream, 'comm_msg',
                             {'comm_id': comm_id, 
                              'data': {'status': 'received', 'original_msg': data}},
                             ident=ident)
    
    # Handle comm_close messages
    def handle_comm_close(self, stream, ident, msg):
        """Handle comm_close messages from the frontend."""
        content = msg['content']
        comm_id = content['comm_id']
        
        # Remove the comm from our dictionary
        if comm_id in self.comms:
            del self.comms[comm_id]

if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=SymLispKernel)