## todos

- [x] Environment structure and basic operations (sl_env_create, sl_env_define, sl_env_lookup).
- [x] Basic eval handling self-evaluating types and symbol lookup.
- [x] Implement quote special form in eval.
- [x] Implement a few simple builtins (car, cdr, cons) and the apply logic for builtins.
- [x] Populate the global environment with these builtins. Test simple calls like (car (quote (1 2))).
- [x] Add math for +, -, *, /
- [x] Implement lambda special form (creating closures) and apply logic for closures. (Basic TCO included)
- [x] Implement define special form. Test defining and calling simple functions.
- [x] Implement if special form.
- [x] Implement set! special form.
- [x] Add API functions for environment get/set/define by C string name.
- [x] Remove reliance on fixed allocations for buffers
- [x] Implement `begin` special form (for multi-expression bodies in lambda/define).
- [x] Implement basic I/O: `display`, `newline`.
- [x] Implement file I/O: `load` function.
- [ ] Implement library loading mechanism (e.g., `load-library` or auto-load from dir).
- [ ] Test framwork
- [ ] CI with valgrind
- [ ] Implement symbol interning in sl_make_symbol so that only one symbol object exists per unique name. Then you can revert the environment functions sl_env_define/lookup/set
- [ ] Refine lambda/define syntax validation (variadics?), variable arity support (lambda)
- [ ] let and named let
- [ ] Refine parameter binding error handling (GC).
- [ ] Gradually add more builtins.
- [ ] Macros: define-syntax, let-syntax
- [x] Refine error handling and GC integration for environments. (Initial error object mechanism added)
- [ ] Check against r7rs
- [ ] Standard library
- [ ] Symbolics libs
- [ ] Kernel
- [ ] MCP/agnt

## Details

Environment Representation (sl_env.c/.h):

Purpose: To store bindings (mapping symbols to sl_object values). Crucial for variables and lexical scoping.
Structure: Define struct sl_env. A common approach is:
sl_object *bindings: A list of pairs ((symbol . value) (symbol . value) ...). Simple to start, can be optimized to a hash table later.
struct sl_env *outer: A pointer to the enclosing (parent) environment. This enables lexical scoping. The global environment's outer is NULL.
Operations:
sl_env_create(sl_env *outer): Create a new, empty environment enclosed by outer.
sl_env_define(sl_env *env, sl_object *symbol, sl_object *value): Define/redefine a variable in the current environment env.
sl_env_set(sl_env *env, sl_object *symbol, sl_object *value): Find the first environment (starting from env and going outwards) where symbol is defined and update its value. Error if not found.
sl_env_lookup(sl_env *env, sl_object *symbol): Find the value bound to symbol, searching env and then outwards. Return the value or NULL/error if not found.
GC: Environments need to be managed. The bindings list and the outer environment pointer need to be marked during GC if the environment itself is reachable.
Evaluation Function (sl_eval.c/.h):

Purpose: The heart of the interpreter. Takes an expression (sl_object *expr) and an environment (sl_env *env) and returns the evaluated result (sl_object *).
Core Logic (eval function):
Self-Evaluating: If expr is a number, string, boolean, or NIL, return expr itself.
Symbol: If expr is a symbol, look it up in env using sl_env_lookup. Return the value or signal an "unbound variable" error.
List: If expr is a list (op arg1 arg2 ...):
Empty List: Error (or return SL_NIL depending on desired semantics).
Special Forms: Check if op is a symbol bound to a special form (like quote, if, define, lambda, set!). Handle these directly within eval as their evaluation rules differ (e.g., arguments might not all be evaluated, or evaluated conditionally).
quote: Return the argument unevaluated.
if: Evaluate the condition. If true, evaluate the 'then' branch; otherwise, evaluate the 'else' branch.
define: Evaluate the value expression. Call sl_env_define in the current env. Return a special "undefined" value or the symbol.
lambda: Create and return a closure object (see below). The closure captures the current env.
set!: Evaluate the value expression. Call sl_env_set starting from the current env.
Procedure Call: If op is not a special form:
Evaluate op in env to get a procedure object (proc).
Evaluate all arguments (arg1, arg2, ...) in env to get a list of argument values (arg_values).
Call an apply function: apply(proc, arg_values).
Function Representation (Closures & Builtins):

You already have the structure in sl_core.h.
Closures (lambda): When eval encounters (lambda (params...) body...), it creates an sl_object of type SL_TYPE_FUNCTION where:
is_builtin = false.
def.closure.params points to the list of parameter symbols.
def.closure.body points to the list of body expressions.
def.closure.env points to the environment where the lambda was defined. This captured environment is key to lexical scope.
Builtins: Predefined C functions wrapped in sl_objects.
Apply Function (sl_eval.c):

Purpose: Handles the actual function invocation. Takes a procedure object (proc) and a list of evaluated arguments (args).
Logic (apply function):
Builtin: If proc->is_builtin is true:
Call the C function pointer: proc->data.function.def.builtin.func_ptr(args).
The C function performs type/arity checks and returns the result sl_object.
Closure: If proc->is_builtin is false:
Create a new environment (call_env) whose outer environment is the closure's captured environment (proc->data.function.def.closure.env).
Bind the closure's parameter symbols (proc->data.function.def.closure.params) to the provided argument values (args) within call_env. Check arity (number of args vs params).
Evaluate the expressions in the closure's body (proc->data.function.def.closure.body) sequentially within this new call_env.
Return the result of the last expression evaluated in the body.
Builtin Procedures (sl_builtins.c/.h):

Implement C functions for core Scheme procedures (e.g., sl_builtin_add, sl_builtin_car, sl_builtin_cons, sl_builtin_eq, sl_builtin_display).
Each function takes sl_object *args (the list of evaluated arguments).
Inside each function:
Validate argument count (arity).
Validate argument types (e.g., + expects numbers).
Perform the operation (using GMP functions for numbers).
Create and return the result sl_object (e.g., sl_make_number_q, SL_TRUE, SL_FALSE).
Handle errors (e.g., print to stderr, return SL_NIL or a dedicated error object).
Integration & Initialization:

In sl_mem_init or a new sl_core_init function:
Create the initial global environment (sl_global_env = sl_env_create(NULL);).
In a new sl_builtins_init function (called after sl_mem_init):
Create builtin function objects using sl_make_builtin.
Define these builtins in the global environment: sl_env_define(sl_global_env, sl_make_symbol("+"), sl_make_builtin("+", sl_builtin_add)); etc.
Modify the REPL (symlisp.c):
Call sl_mem_init() and sl_builtins_init().
After parsing (result = sl_parse_string(...)), call eval_result = sl_eval(result, sl_global_env);.
Print eval_result using sl_write_to_stream.
Call sl_gc().
