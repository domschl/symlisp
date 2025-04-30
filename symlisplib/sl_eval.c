#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <dirent.h>    // <<< For directory listing (POSIX)
#include <errno.h>     // <<< For strerror
#include <sys/stat.h>  // <<< For stat to check if it's a file

#include "sl_eval.h"
#include "sl_core.h"
#include "sl_env.h"
#include "sl_parse.h"  // Need for sl_parse_string
#include "sl_builtins.h"

// --- Forward Declarations ---
sl_object *sl_eval_list(sl_object *list, sl_object *env);

// static sl_object *eval_sequence(sl_object *seq, sl_object *env);  // Helper for begin/body logic
//  Renamed eval_sequence to be more specific
static sl_object *eval_sequence_with_defines(sl_object *seq, sl_object *env, sl_object **obj_ptr, sl_object **env_ptr);

// Helper function to get symbol name safely
static const char *safe_symbol_name(sl_object *obj) {
    if (sl_is_symbol(obj)) {
        return sl_symbol_name(obj);
    }
    return "invalid-symbol";
}

// --- Sequence Evaluation Helper ---
// Evaluates a sequence of expressions, handling internal defines (letrec* style).
// Returns the value of the last expression, or an error object.
// If the last expression is in tail position relative to the *caller*,
// it modifies *obj_ptr and *env_ptr and returns SL_CONTINUE_EVAL
// to signal the caller to jump to top_of_eval.
static sl_object *eval_sequence_with_defines(sl_object *seq, sl_object *env, sl_object **obj_ptr, sl_object **env_ptr) {
    sl_object *current_node = seq;
    sl_object *defines_list = SL_NIL;
    sl_object **defines_tail_ptr = &defines_list;
    sl_object *body_start_node = seq;  // Where non-define expressions start
    sl_object *result = SL_NIL;        // Default result for empty sequence

    // --- Root parameters and locals ---
    sl_gc_add_root(&seq);
    sl_gc_add_root(&env);
    sl_gc_add_root(&defines_list);
    sl_gc_add_root(&body_start_node);
    sl_gc_add_root(&result);

    // --- Pass 1: Scan for defines and create placeholders ---
    bool defines_found = false;
    while (sl_is_pair(current_node)) {
        sl_object *expr = sl_car(current_node);
        if (sl_is_pair(expr) && sl_is_symbol(sl_car(expr)) && strcmp(sl_symbol_name(sl_car(expr)), "define") == 0) {
            defines_found = true;
            sl_object *define_args = sl_cdr(expr);
            if (define_args == SL_NIL) {
                result = sl_make_errorf("Eval: Malformed define in sequence (no target)");
                goto cleanup_sequence;
            }
            sl_object *target = sl_car(define_args);
            sl_object *var_sym = SL_NIL;

            if (sl_is_pair(target)) {  // Function define: (define (f x) ...)
                var_sym = sl_car(target);
            } else if (sl_is_symbol(target)) {  // Variable define: (define x ...)
                var_sym = target;
            } else {
                result = sl_make_errorf("Eval: Invalid define target in sequence");
                goto cleanup_sequence;
            }

            if (!sl_is_symbol(var_sym)) {
                result = sl_make_errorf("Eval: Define target must be or start with a symbol");
                goto cleanup_sequence;
            }

            // Add define expression to defines_list
            sl_object *define_pair = sl_make_pair(expr, SL_NIL);
            CHECK_ALLOC_GOTO(define_pair, cleanup_sequence, result);
            *defines_tail_ptr = define_pair;
            defines_tail_ptr = &define_pair->data.pair.cdr;

            // Define placeholder in the environment
            sl_env_define(env, var_sym, SL_UNDEFINED);
            // Check OOM?

            body_start_node = sl_cdr(current_node);  // Move body start past this define
        } else {
            // First non-define expression found
            break;
        }
        current_node = sl_cdr(current_node);
    }
    // Check for improper list during define scan
    if (!defines_found && current_node != seq) {  // Only error if we advanced
        if (current_node != SL_NIL) {
            result = sl_make_errorf("Eval: Improper list while scanning for defines");
            goto cleanup_sequence;
        }
    }

    // --- Pass 2: Evaluate define expressions ---
    if (defines_found) {
        current_node = defines_list;
        sl_gc_add_root(&current_node);  // Root traversal pointer for defines
        while (current_node != SL_NIL) {
            // We know current_node is a pair containing a define expression
            sl_object *define_expr = sl_car(current_node);
            sl_object *define_args = sl_cdr(define_expr);  // Args of define: (sym val) or ((f p) b)
            sl_object *target = sl_car(define_args);
            sl_object *value_expr_or_body = sl_cdr(define_args);
            sl_object *var_sym = SL_NIL;
            sl_object *value_to_set = SL_NIL;

            sl_gc_add_root(&value_to_set);  // Root value slot

            if (sl_is_pair(target)) {  // Function define
                var_sym = sl_car(target);
                sl_object *params = sl_cdr(target);
                sl_object *body = value_expr_or_body;

                value_to_set = sl_make_closure(params, body, env);
                CHECK_ALLOC_GOTO(value_to_set, cleanup_sequence_eval, result);
            } else {  // Variable define
                var_sym = target;
                if (!sl_is_pair(value_expr_or_body) || sl_cdr(value_expr_or_body) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed variable define in sequence");
                    sl_gc_remove_root(&value_to_set);
                    goto cleanup_sequence_eval;
                }
                sl_object *value_expr = sl_car(value_expr_or_body);
                value_to_set = sl_eval(value_expr, env);  // Eval value - MIGHT GC
                // value_to_set is already rooted
                if (value_to_set == SL_OUT_OF_MEMORY_ERROR || sl_is_error(value_to_set)) {
                    result = value_to_set;  // Propagate error
                    sl_gc_remove_root(&value_to_set);
                    goto cleanup_sequence_eval;
                }
            }

            // Update the placeholder using set! semantics
            if (!sl_env_set(env, var_sym, value_to_set)) {
                result = sl_make_errorf("Eval: Internal error - failed to set! defined variable '%s'", sl_symbol_name(var_sym));
                sl_gc_remove_root(&value_to_set);
                goto cleanup_sequence_eval;
            }
            sl_gc_remove_root(&value_to_set);  // Unroot value after set
            current_node = sl_cdr(current_node);
        }  // End while defines_list
    cleanup_sequence_eval:
        sl_gc_remove_root(&current_node);  // Unroot traversal pointer
        if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
            goto cleanup_sequence;  // Propagate error from define eval
        }
    }  // End if defines_found

    // --- Pass 3: Evaluate body expressions ---
    current_node = body_start_node;
    sl_gc_add_root(&current_node);
    result = SL_NIL;  // Default result if body is empty

    if (current_node == SL_NIL) {  // Handle empty body case
        goto cleanup_sequence_body;
    }

    while (sl_is_pair(current_node)) {
        sl_object *expr_to_eval = sl_car(current_node);
        sl_object *next_node = sl_cdr(current_node);

        // Check for tail call position
        if (next_node == SL_NIL) {
            // --- ADDED CHECK for valid TCO pointers ---
            if (obj_ptr && env_ptr) {
                // Caller can handle TCO, modify pointers and signal
                *obj_ptr = expr_to_eval;
                *env_ptr = env;             // Use current env for tail call
                result = SL_CONTINUE_EVAL;  // Signal TCO jump
            } else {
                // Caller cannot handle TCO (e.g., called from apply builtin),
                // so evaluate the tail expression directly here.
                // Need to root expr_to_eval before calling sl_eval
                sl_gc_add_root(&expr_to_eval);
                result = sl_eval(expr_to_eval, env);  // Evaluate final expression
                sl_gc_remove_root(&expr_to_eval);
                // result now holds the final value or an error
            }
            // --- END ADDED CHECK ---
            goto cleanup_sequence_body;  // Don't evaluate here
        } else {
            // Not the last expression, evaluate normally
            sl_gc_remove_root(&result);
            result = sl_eval(expr_to_eval, env);  // Eval - MIGHT GC
            sl_gc_add_root(&result);

            if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                goto cleanup_sequence_body;  // Error occurred, propagate
            }
        }
        current_node = sl_cdr(current_node);
    }

    if (current_node != SL_NIL) {  // Check for improper list
        result = sl_make_errorf("Eval: Improper list in sequence body");
    }
    // If loop finished normally, result holds the value of the second-to-last expression.
    // The TCO path handles the final evaluation.

cleanup_sequence_body:
    sl_gc_remove_root(&current_node);
cleanup_sequence:
    sl_gc_remove_root(&result);
    sl_gc_remove_root(&body_start_node);
    sl_gc_remove_root(&defines_list);
    sl_gc_remove_root(&env);
    sl_gc_remove_root(&seq);
    return result;
}

sl_object *sl_eval(sl_object *obj_in, sl_object *env_in) {
    sl_object *obj = obj_in;
    sl_object *env = env_in;
    sl_object *result = SL_NIL;

    // --- Root key local variables ---
    sl_gc_add_root(&obj);
    sl_gc_add_root(&env);
    sl_gc_add_root(&result);

top_of_eval:;
    if (!obj) {  // Handle NULL input gracefully
        result = sl_make_errorf("Eval: NULL object");
        goto cleanup;
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
    case SL_TYPE_BOOLEAN:
    case SL_TYPE_NUMBER:
    case SL_TYPE_STRING:
    case SL_TYPE_CHAR:
    case SL_TYPE_ERROR:  // Errors evaluate to themselves
        result = obj;    // Self-evaluating
        break;

    case SL_TYPE_SYMBOL: {
        // Look up the binding pair for the symbol
        sl_object *binding_pair = sl_env_lookup(env, obj);

        if (binding_pair == SL_NIL) {
            // Symbol not found (lookup returned NIL)
            const char *sym_name = sl_symbol_name(obj);
            result = sl_make_errorf("Eval: Unbound symbol '%s'", sym_name);
        } else if (!sl_is_pair(binding_pair)) {
            // Should not happen if lookup is correct, but check defensively
            result = sl_make_errorf("Eval: Internal error - lookup returned non-pair for found symbol '%s'", sl_symbol_name(obj));
        } else {
            // Symbol found, the value is the cdr of the binding pair
            result = sl_cdr(binding_pair);  // <<< CHANGED: Get value from cdr
        }
        break;  // Break from switch case SL_TYPE_SYMBOL
    }

    case SL_TYPE_PAIR: {
        sl_object *op_obj = sl_car(obj);
        sl_object *args = sl_cdr(obj);

        if (sl_is_symbol(op_obj)) {
            const char *op_name = sl_symbol_name(op_obj);

            // --- QUOTE ---
            if (strcmp(op_name, "quote") == 0) {
                sl_gc_add_root(&args);  // Root args for safety
                if (args == SL_NIL || sl_cdr(args) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed quote");
                } else {
                    result = sl_car(args);  // Don't eval args of quote
                }
                sl_gc_remove_root(&args);
                break;  // Break from switch (handled special form)
            }
            // --- IF ---
            else if (strcmp(op_name, "if") == 0) {
                // Root args temporarily
                sl_gc_add_root(&args);
                // (if test conseq alt)
                sl_object *arg1 = sl_car(args);
                sl_object *arg2_pair = sl_cdr(args);
                sl_object *arg3_pair = sl_is_pair(arg2_pair) ? sl_cdr(arg2_pair) : SL_NIL;

                sl_object *test_expr = arg1;
                sl_object *conseq_expr = sl_is_pair(arg2_pair) ? sl_car(arg2_pair) : SL_NIL;
                sl_object *alt_expr = sl_is_pair(arg3_pair) ? sl_car(arg3_pair) : SL_NIL;

                // Check structure
                if (test_expr == SL_NIL || conseq_expr == SL_NIL || !sl_is_pair(arg2_pair) || (sl_is_pair(arg3_pair) && sl_cdr(arg3_pair) != SL_NIL)) {
                    result = sl_make_errorf("Eval: Malformed if structure");
                    sl_gc_remove_root(&args);
                    break;  // Break from switch
                }
                if (!sl_is_pair(arg3_pair)) { alt_expr = SL_NIL; }

                sl_gc_remove_root(&args);  // Unroot args before eval

                // Root expressions needed after test_result is evaluated
                sl_gc_add_root(&conseq_expr);
                sl_gc_add_root(&alt_expr);

                sl_object *test_result = sl_eval(test_expr, env);  // Eval test - MIGHT GC
                sl_gc_add_root(&test_result);

                if (test_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(test_result)) {
                    result = test_result;  // Propagate error
                } else if (test_result != SL_FALSE) {
                    obj = conseq_expr;  // Tail call conseq
                } else {
                    obj = alt_expr;  // Tail call alt
                }
                // Unroot temporaries before tail call
                sl_gc_remove_root(&test_result);
                sl_gc_remove_root(&alt_expr);
                sl_gc_remove_root(&conseq_expr);
                goto top_of_eval;  // Tail call optimization
            }
            // --- DEFINE ---
            else if (strcmp(op_name, "define") == 0) {
                sl_gc_add_root(&args);
                // (define symbol value) or (define (fn params...) body...)
                if (args == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed define (no symbol/value)");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *target = sl_car(args);
                sl_object *value_expr_pair = sl_cdr(args);  // This is the body list for functions

                if (sl_is_pair(target)) {  // Function definition
                    sl_object *fn_name_sym = sl_car(target);
                    sl_object *params = sl_cdr(target);
                    sl_object *body_list = value_expr_pair;  // <<< Use the whole list

                    if (!sl_is_symbol(fn_name_sym)) {
                        result = sl_make_errorf("Eval: Function name in define must be a symbol");
                        sl_gc_remove_root(&args);
                        break;
                    }
                    // TODO: Add validation for params list structure
                    // TODO: Handle multiple body expressions (implicit begin)

                    // Store the *list* of body expressions in the closure
                    sl_object *lambda = sl_make_closure(params, body_list, env);  // <<< Pass body_list
                    CHECK_ALLOC(lambda);
                    sl_env_define(env, fn_name_sym, lambda);
                    result = fn_name_sym;

                } else if (sl_is_symbol(target)) {  // Variable definition
                    if (value_expr_pair == SL_NIL || sl_cdr(value_expr_pair) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed define (wrong number of args for variable)");
                        sl_gc_remove_root(&args);
                        break;
                    }
                    sl_object *value_expr = sl_car(value_expr_pair);

                    // Unroot op_obj and args before eval
                    sl_gc_remove_root(&op_obj);
                    // sl_gc_remove_root(&args);

                    sl_object *value = sl_eval(value_expr, env);
                    sl_gc_add_root(&value);

                    if (value == SL_OUT_OF_MEMORY_ERROR) {
                        result = value;
                    } else {
                        // sl_env_define returns void
                        sl_env_define(env, target, value);
                        // Check for OOM? See comment above.
                        result = target;  // Return the symbol
                    }
                    sl_gc_remove_root(&value);

                } else {
                    result = sl_make_errorf("Eval: Invalid target for define");
                }
                sl_gc_remove_root(&args);
                break;
            }
            // --- SET! --- <<< ADDED BLOCK
            else if (strcmp(op_name, "set!") == 0) {
                sl_gc_add_root(&args);  // Root args
                // (set! symbol value)
                if (args == SL_NIL || !sl_is_pair(args) || sl_cdr(args) == SL_NIL || !sl_is_pair(sl_cdr(args)) || sl_cdr(sl_cdr(args)) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed set!");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *target_sym = sl_car(args);
                sl_object *value_expr = sl_cadr(args);  // Use helper or sl_car(sl_cdr(args))

                if (!sl_is_symbol(target_sym)) {
                    result = sl_make_errorf("Eval: Target for set! must be a symbol");
                    sl_gc_remove_root(&args);
                    break;
                }

                sl_gc_remove_root(&args);  // Unroot args before eval

                sl_object *value = sl_eval(value_expr, env);  // Eval value - MIGHT GC
                sl_gc_add_root(&value);                       // Root result

                if (value == SL_OUT_OF_MEMORY_ERROR) {
                    result = value;  // Propagate error
                } else {
                    // sl_env_set returns bool indicating if symbol was found and set
                    if (!sl_env_set(env, target_sym, value)) {
                        result = sl_make_errorf("Eval: Cannot set! unbound symbol '%s'", sl_symbol_name(target_sym));
                    } else {
                        result = value;  // set! usually returns the value
                    }
                }
                sl_gc_remove_root(&value);  // Unroot value
                break;                      // Break from switch
            }
            // --- LAMBDA ---
            else if (strcmp(op_name, "lambda") == 0) {
                sl_gc_add_root(&args);  // Root args
                // (lambda (params...) body...)
                if (args == SL_NIL || sl_cdr(args) == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed lambda (missing params or body)");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *params = sl_car(args);
                sl_object *body_list = sl_cdr(args);  // Body is the rest of the list

                // TODO: Validate params is a proper list of symbols (or NIL)
                // TODO: Validate body_list is a proper list

                // Create closure with the *list* of body expressions
                result = sl_make_closure(params, body_list, env);
                CHECK_ALLOC(result);  // Checks if result is SL_OUT_OF_MEMORY_ERROR
                sl_gc_remove_root(&args);
                break;  // Break from switch
            }
            // --- BEGIN --- <<< ADDED BLOCK
            else if (strcmp(op_name, "begin") == 0) {
                // (begin expr1 expr2 ...)
                // Delegate to the sequence evaluator
                result = eval_sequence_with_defines(args, env, &obj, &env);
                if (result == SL_CONTINUE_EVAL) {  // Check if TCO jump is requested
                    goto top_of_eval;
                }
                // Otherwise, result holds the final value or error
                break;  // Break from switch
            }
            // --- LET / NAMED LET ---
            else if (strcmp(op_name, "let") == 0) {
                // (let bindings body...) or (let name bindings body...)
                sl_gc_add_root(&args);  // Root the rest of the let form

                if (args == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed let (missing bindings/body)");
                    sl_gc_remove_root(&args);
                    break;
                }

                sl_object *first_arg = sl_car(args);
                sl_object *body_list = sl_cdr(args);
                sl_object *name_sym = SL_NIL;  // For named let
                sl_object *bindings = SL_NIL;

                // Check for named let variant
                if (sl_is_symbol(first_arg)) {
                    name_sym = first_arg;
                    sl_gc_add_root(&name_sym);  // Root name symbol
                    if (body_list == SL_NIL || !sl_is_pair(body_list)) {
                        result = sl_make_errorf("Eval: Malformed named let (missing bindings/body)");
                        sl_gc_remove_root(&name_sym);
                        sl_gc_remove_root(&args);
                        break;
                    }
                    bindings = sl_car(body_list);
                    body_list = sl_cdr(body_list);
                } else {
                    // Standard let
                    bindings = first_arg;
                }

                // Validate bindings structure and body
                if (!sl_is_list(bindings)) {  // sl_is_list checks for proper list or NIL
                    result = sl_make_errorf("Eval: Malformed let bindings (not a list)");
                    if (name_sym != SL_NIL) sl_gc_remove_root(&name_sym);
                    sl_gc_remove_root(&args);
                    break;
                }
                if (body_list == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed let (missing body)");
                    if (name_sym != SL_NIL) sl_gc_remove_root(&name_sym);
                    sl_gc_remove_root(&args);
                    break;
                }

                // Root bindings and body before evaluation loops
                sl_gc_add_root(&bindings);
                sl_gc_add_root(&body_list);

                // --- Evaluate initializers in the *current* environment ---
                sl_object *vars_list = SL_NIL;
                sl_object *vals_list = SL_NIL;
                sl_object **vars_tail_ptr = &vars_list;
                sl_object **vals_tail_ptr = &vals_list;
                sl_object *current_binding = bindings;
                bool init_eval_ok = true;

                sl_gc_add_root(&vars_list);  // Root lists being built
                sl_gc_add_root(&vals_list);
                sl_gc_add_root(&current_binding);

                while (current_binding != SL_NIL) {
                    if (!sl_is_pair(current_binding)) {  // Should be caught by sl_is_list, but double check
                        result = sl_make_errorf("Eval: Malformed let bindings (improper list)");
                        init_eval_ok = false;
                        break;
                    }
                    sl_object *binding_pair = sl_car(current_binding);
                    if (!sl_is_pair(binding_pair) || sl_cdr(binding_pair) == SL_NIL || !sl_is_pair(sl_cdr(binding_pair)) || sl_cdr(sl_cdr(binding_pair)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed let binding pair");
                        init_eval_ok = false;
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    sl_object *init_expr = sl_cadr(binding_pair);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in let binding must be a symbol");
                        init_eval_ok = false;
                        break;
                    }

                    // Evaluate init_expr in the *outer* env
                    sl_object *init_val = sl_eval(init_expr, env);  // MIGHT GC
                    sl_gc_add_root(&init_val);                      // Root the evaluated value

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        sl_gc_remove_root(&init_val);
                        init_eval_ok = false;
                        break;
                    }

                    // Append var_sym to vars_list
                    sl_object *var_pair = sl_make_pair(var_sym, SL_NIL);
                    CHECK_ALLOC_GOTO(var_pair, oom_let, result);
                    *vars_tail_ptr = var_pair;
                    vars_tail_ptr = &var_pair->data.pair.cdr;

                    // Append init_val to vals_list
                    sl_object *val_pair = sl_make_pair(init_val, SL_NIL);
                    CHECK_ALLOC_GOTO(val_pair, oom_let, result);
                    *vals_tail_ptr = val_pair;
                    vals_tail_ptr = &val_pair->data.pair.cdr;

                    sl_gc_remove_root(&init_val);  // Value is now safe in vals_list
                    current_binding = sl_cdr(current_binding);
                }  // End while bindings

                // Unroot temporary binding list traversal pointer
                sl_gc_remove_root(&current_binding);

                if (!init_eval_ok) {   // Error during init eval or binding list parsing
                    goto cleanup_let;  // Result already holds the error
                }

                // --- Create new environment and bind variables ---
                sl_object *let_env = sl_env_create(env);
                CHECK_ALLOC_GOTO(let_env, oom_let, result);
                sl_gc_add_root(&let_env);  // Root the new environment

                // --- Handle named let: define the recursive function ---
                if (name_sym != SL_NIL) {
                    // Create lambda: (lambda (vars...) body...)
                    sl_object *lambda = sl_make_closure(vars_list, body_list, let_env);
                    CHECK_ALLOC_GOTO(lambda, oom_let_env, result);
                    // Define the name *within* the let_env
                    sl_env_define(let_env, name_sym, lambda);
                    // Note: lambda is now potentially reachable via let_env
                }

                // --- Bind vars to evaluated vals in the new environment ---
                sl_object *v = vars_list;
                sl_object *val = vals_list;
                while (v != SL_NIL) {  // Assumes vars_list and vals_list have same length
                    sl_env_define(let_env, sl_car(v), sl_car(val));
                    // Check for OOM from define?
                    v = sl_cdr(v);
                    val = sl_cdr(val);
                }

                // --- Evaluate body in the new environment ---
                // Use helper function for sequence evaluation with TCO
                // Unroot lists before potential tail call in eval_sequence
                sl_gc_remove_root(&vals_list);
                sl_gc_remove_root(&vars_list);
                // sl_gc_remove_root(&body_list);
                sl_gc_remove_root(&bindings);
                sl_gc_remove_root(&args);
                if (name_sym != SL_NIL) sl_gc_remove_root(&name_sym);

                result = eval_sequence_with_defines(body_list, let_env, &obj, &env);
                sl_gc_remove_root(&let_env);    // Unroot let_env after sequence eval
                sl_gc_remove_root(&body_list);  // <<< ADDED: Unroot body_list after use

                if (result == SL_CONTINUE_EVAL) {  // Check if TCO jump is requested
                    goto top_of_eval;
                }
                // Otherwise, result holds the final value or error from the body

            oom_let_env:                        // Label for OOM after let_env creation (if needed)
                                                // sl_gc_remove_root(&let_env); // Already handled
            oom_let:                            // Label for OOM before let_env creation
            cleanup_let:                        // General cleanup label for let
                sl_gc_remove_root(&vals_list);  // Ensure unrooted on error paths
                sl_gc_remove_root(&vars_list);
                sl_gc_remove_root(&body_list);
                sl_gc_remove_root(&bindings);
                sl_gc_remove_root(&args);
                if (name_sym != SL_NIL) sl_gc_remove_root(&name_sym);
                break;  // Break from switch case 'let'
            }  // End LET / NAMED LET block
            // --- LET* --- <<< NEW BLOCK
            else if (strcmp(op_name, "let*") == 0) {
                // (let* bindings body...)
                sl_gc_add_root(&args);  // Root the rest of the form

                if (args == SL_NIL || !sl_is_pair(args)) {
                    result = sl_make_errorf("Eval: Malformed let* (missing bindings/body)");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *bindings = sl_car(args);
                sl_object *body_list = sl_cdr(args);

                if (!sl_is_list(bindings)) {
                    result = sl_make_errorf("Eval: Malformed let* bindings (not a list)");
                    sl_gc_remove_root(&args);
                    break;
                }
                if (body_list == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed let* (missing body)");
                    sl_gc_remove_root(&args);
                    break;
                }

                sl_gc_add_root(&bindings);
                sl_gc_add_root(&body_list);

                // --- Sequentially evaluate and bind ---
                sl_object *current_binding_node = bindings;
                sl_object *let_star_env = env;  // Start with outer env
                sl_gc_add_root(&let_star_env);  // Root the evolving environment
                sl_gc_add_root(&current_binding_node);

                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) {
                        result = sl_make_errorf("Eval: Malformed let* bindings (improper list)");
                        goto cleanup_let_star;
                    }
                    sl_object *binding_pair = sl_car(current_binding_node);
                    if (!sl_is_pair(binding_pair) || sl_cdr(binding_pair) == SL_NIL || !sl_is_pair(sl_cdr(binding_pair)) || sl_cdr(sl_cdr(binding_pair)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed let* binding pair");
                        goto cleanup_let_star;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    sl_object *init_expr = sl_cadr(binding_pair);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in let* binding must be a symbol");
                        goto cleanup_let_star;
                    }

                    // Evaluate init_expr in the *current* let_star_env
                    sl_object *init_val = sl_eval(init_expr, let_star_env);  // MIGHT GC
                    sl_gc_add_root(&init_val);

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        sl_gc_remove_root(&init_val);
                        goto cleanup_let_star;
                    }

                    // Create a *new* environment extending the current one
                    sl_object *next_env = sl_env_create(let_star_env);
                    CHECK_ALLOC_GOTO(next_env, cleanup_let_star_val, result);

                    // Define var in the new environment
                    sl_env_define(next_env, var_sym, init_val);
                    // Check OOM?

                    // Update let_star_env for the next iteration
                    sl_gc_remove_root(&let_star_env);  // Unroot old env ptr
                    let_star_env = next_env;           // Point to the new env
                    sl_gc_add_root(&let_star_env);     // Root the new env ptr

                cleanup_let_star_val:
                    sl_gc_remove_root(&init_val);                                 // Unroot value after potential use/binding
                    if (result == SL_OUT_OF_MEMORY_ERROR) goto cleanup_let_star;  // Handle OOM from env_create

                    current_binding_node = sl_cdr(current_binding_node);
                }  // End while bindings

                // --- Evaluate body in the final let_star_env ---
                sl_gc_remove_root(&current_binding_node);
                sl_gc_remove_root(&bindings);
                sl_gc_remove_root(&args);

                result = eval_sequence_with_defines(body_list, let_star_env, &obj, &env);
                sl_gc_remove_root(&let_star_env);  // Unroot final env after sequence eval
                sl_gc_remove_root(&body_list);

                if (result == SL_CONTINUE_EVAL) {  // Check if TCO jump is requested
                    goto top_of_eval;
                }
                // Otherwise, result holds the final value or error

            cleanup_let_star:  // Error/cleanup path
                sl_gc_remove_root(&current_binding_node);
                sl_gc_remove_root(&let_star_env);
                sl_gc_remove_root(&body_list);
                sl_gc_remove_root(&bindings);
                sl_gc_remove_root(&args);
                break;  // Break from switch case 'let*'
            }  // End LET* block

            // --- LETREC* --- <<< NEW BLOCK
            else if (strcmp(op_name, "letrec*") == 0) {
                // (letrec* bindings body...)
                sl_gc_add_root(&args);  // Root the rest of the form

                if (args == SL_NIL || !sl_is_pair(args)) {
                    result = sl_make_errorf("Eval: Malformed letrec* (missing bindings/body)");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *bindings = sl_car(args);
                sl_object *body_list = sl_cdr(args);

                if (!sl_is_list(bindings)) {
                    result = sl_make_errorf("Eval: Malformed letrec* bindings (not a list)");
                    sl_gc_remove_root(&args);
                    break;
                }
                if (body_list == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed letrec* (missing body)");
                    sl_gc_remove_root(&args);
                    break;
                }

                sl_gc_add_root(&bindings);
                sl_gc_add_root(&body_list);

                // --- Create new environment ---
                sl_object *letrec_env = sl_env_create(env);
                CHECK_ALLOC_GOTO(letrec_env, cleanup_letrec, result);
                sl_gc_add_root(&letrec_env);

                // --- Pass 1: Create placeholders ---
                sl_object *current_binding_node = bindings;
                sl_gc_add_root(&current_binding_node);
                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) { /* error handled below */
                        break;
                    }
                    sl_object *binding_pair = sl_car(current_binding_node);
                    if (!sl_is_pair(binding_pair) || !sl_is_pair(sl_cdr(binding_pair))) { /* error handled below */
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    if (!sl_is_symbol(var_sym)) { /* error handled below */
                        break;
                    }

                    // Define with placeholder
                    sl_env_define(letrec_env, var_sym, SL_UNDEFINED);
                    // Check OOM?

                    current_binding_node = sl_cdr(current_binding_node);
                }
                sl_gc_remove_root(&current_binding_node);  // Unroot traversal pointer

                // --- Pass 2: Evaluate initializers sequentially and update bindings ---
                current_binding_node = bindings;
                sl_gc_add_root(&current_binding_node);
                bool init_ok = true;
                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) {
                        result = sl_make_errorf("Eval: Malformed letrec* bindings (improper list)");
                        init_ok = false;
                        break;
                    }
                    sl_object *binding_pair = sl_car(current_binding_node);
                    if (!sl_is_pair(binding_pair) || sl_cdr(binding_pair) == SL_NIL || !sl_is_pair(sl_cdr(binding_pair)) || sl_cdr(sl_cdr(binding_pair)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed letrec* binding pair");
                        init_ok = false;
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    sl_object *init_expr = sl_cadr(binding_pair);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in letrec* binding must be a symbol");
                        init_ok = false;
                        break;
                    }

                    // Evaluate init_expr in the letrec_env (placeholders are visible)
                    sl_object *init_val = sl_eval(init_expr, letrec_env);  // MIGHT GC
                    sl_gc_add_root(&init_val);

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        sl_gc_remove_root(&init_val);
                        init_ok = false;
                        break;
                    }

                    // Update the placeholder binding using set! semantics
                    if (!sl_env_set(letrec_env, var_sym, init_val)) {
                        // Should not happen if placeholder was created correctly
                        result = sl_make_errorf("Eval: Internal error - failed to set! letrec* variable '%s'", sl_symbol_name(var_sym));
                        sl_gc_remove_root(&init_val);
                        init_ok = false;
                        break;
                    }
                    sl_gc_remove_root(&init_val);  // Unroot value after set

                    current_binding_node = sl_cdr(current_binding_node);
                }  // End while bindings (Pass 2)
                sl_gc_remove_root(&current_binding_node);

                if (!init_ok) {  // Error during Pass 2?
                    goto cleanup_letrec_env;
                }

                // --- Evaluate body in the letrec_env ---
                sl_gc_remove_root(&bindings);
                sl_gc_remove_root(&args);

                result = eval_sequence_with_defines(body_list, letrec_env, &obj, &env);
                sl_gc_remove_root(&letrec_env);  // Unroot env after sequence eval
                sl_gc_remove_root(&body_list);

                if (result == SL_CONTINUE_EVAL) {  // Check if TCO jump is requested
                    goto top_of_eval;
                }
                // Otherwise, result holds the final value or error

            cleanup_letrec_env:
                sl_gc_remove_root(&letrec_env);
            cleanup_letrec:                                // Error/cleanup path
                sl_gc_remove_root(&current_binding_node);  // Ensure unrooted
                sl_gc_remove_root(&body_list);
                sl_gc_remove_root(&bindings);
                sl_gc_remove_root(&args);
                break;  // Break from switch case 'letrec*'
            }  // End LETREC* block

            // --- COND --- <<< ADDED BLOCK
            else if (strcmp(op_name, "cond") == 0) {
                // (cond (test1 body1...) (test2 body2...) ... (else else_body...))
                sl_gc_add_root(&args);  // Root the list of clauses

                sl_object *current_clause_node = args;
                result = SL_NIL;  // Default result if no clause matches (unspecified)

                while (current_clause_node != SL_NIL) {
                    if (!sl_is_pair(current_clause_node)) {
                        result = sl_make_errorf("Eval: Malformed cond (improper list of clauses)");
                        goto cleanup_cond;
                    }

                    sl_object *clause = sl_car(current_clause_node);
                    sl_object *next_clause_node = sl_cdr(current_clause_node);

                    if (!sl_is_pair(clause)) {
                        result = sl_make_errorf("Eval: Malformed cond clause (not a pair)");
                        goto cleanup_cond;
                    }

                    sl_object *test_expr = sl_car(clause);
                    sl_object *body_list = sl_cdr(clause);  // List of expressions in the body

                    bool is_else_clause = false;
                    if (sl_is_symbol(test_expr) && strcmp(sl_symbol_name(test_expr), "else") == 0) {
                        is_else_clause = true;
                        // 'else' must be the last clause
                        if (next_clause_node != SL_NIL) {
                            result = sl_make_errorf("Eval: 'else' clause must be the last clause in cond");
                            goto cleanup_cond;
                        }
                    }

                    sl_object *test_result = SL_TRUE;  // Assume true for 'else' clause
                    if (!is_else_clause) {
                        // Evaluate the test expression
                        test_result = sl_eval(test_expr, env);  // MIGHT GC
                        sl_gc_add_root(&test_result);           // Root the result

                        if (test_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(test_result)) {
                            result = test_result;  // Propagate error
                            sl_gc_remove_root(&test_result);
                            goto cleanup_cond;
                        }
                    }

                    // Check if test is true (anything but #f)
                    if (is_else_clause || test_result != SL_FALSE) {
                        if (!is_else_clause) {
                            sl_gc_remove_root(&test_result);  // Unroot test result if it was evaluated
                        }

                        // --- Evaluate the body sequence of the chosen clause ---
                        sl_gc_add_root(&body_list);  // Root the body list

                        if (body_list == SL_NIL) {
                            // Clause like (test) - result is the test result itself
                            // If it was the else clause, test_result is SL_TRUE, which is wrong.
                            // The result should be the value of the test expression.
                            // Re-evaluating test_expr if needed, or retrieve it.
                            // For simplicity now, let's return the test_result (or SL_TRUE for else)
                            // A clause like (else) is questionable, maybe error? R5RS says unspecified.
                            // Let's return the test result if not else, NIL if else.
                            result = is_else_clause ? SL_NIL : test_result;
                            sl_gc_remove_root(&body_list);
                            goto cleanup_cond;  // Found the clause, evaluation done.
                        }

                        sl_object *current_body_node = body_list;
                        result = SL_NIL;  // Default if body somehow becomes empty during eval

                        while (sl_is_pair(current_body_node)) {
                            sl_object *expr_to_eval = sl_car(current_body_node);
                            sl_object *next_body_node = sl_cdr(current_body_node);

                            // Check for tail call position (last expression in the body)
                            if (next_body_node == SL_NIL) {
                                sl_gc_remove_root(&body_list);  // Unroot body list
                                sl_gc_remove_root(&args);       // Unroot clause list
                                obj = expr_to_eval;             // Set up for tail call
                                goto top_of_eval;               // Jump!
                            } else {
                                // Not the last expression, evaluate normally
                                result = sl_eval(expr_to_eval, env);  // MIGHT GC
                                sl_gc_add_root(&result);              // Root intermediate result

                                if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                                    sl_gc_remove_root(&body_list);  // Unroot before cleanup
                                    goto cleanup_cond;              // Error occurred
                                }
                                sl_gc_remove_root(&result);  // Unroot intermediate result
                            }
                            current_body_node = next_body_node;
                        }  // end while body expressions

                        // Check for improper body list
                        if (current_body_node != SL_NIL) {
                            result = sl_make_errorf("Eval: Malformed cond clause body (improper list)");
                        }
                        // If loop finished normally (e.g. empty body list initially), result is NIL.
                        // If error occurred, result holds the error.

                        sl_gc_remove_root(&body_list);  // Unroot body list
                        goto cleanup_cond;              // Clause handled, exit cond evaluation.

                    }  // end if test true or else

                    // Test was false, unroot test result and continue to next clause
                    if (!is_else_clause) {
                        sl_gc_remove_root(&test_result);
                    }
                    current_clause_node = next_clause_node;

                }  // end while clauses

            cleanup_cond:
                sl_gc_remove_root(&args);  // Final unroot of clause list
                break;                     // Break from switch case 'cond'
            }  // End COND block
            // --- AND --- <<< ADDED BLOCK
            else if (strcmp(op_name, "and") == 0) {
                // (and expr1 expr2 ...)
                // Short-circuits on #f. Returns last value if all true. Returns #t if no args.
                sl_gc_add_root(&args);  // Root the list of expressions
                result = SL_TRUE;       // Default for (and)

                sl_object *current_node = args;
                while (sl_is_pair(current_node)) {
                    sl_object *expr_to_eval = sl_car(current_node);
                    sl_object *next_node = sl_cdr(current_node);

                    // Check for tail call position (last expression)
                    if (next_node == SL_NIL) {
                        sl_gc_remove_root(&args);  // Unroot args list
                        obj = expr_to_eval;        // Set up for tail call
                        goto top_of_eval;          // Jump!
                    } else {
                        // Not the last expression, evaluate normally
                        result = sl_eval(expr_to_eval, env);  // MIGHT GC
                        sl_gc_add_root(&result);              // Root intermediate result

                        if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                            sl_gc_remove_root(&args);  // Unroot before cleanup
                            goto cleanup;              // Error occurred
                        }

                        // Check for short-circuit condition (#f)
                        if (result == SL_FALSE) {
                            sl_gc_remove_root(&args);  // Unroot args list
                            goto cleanup;              // Result is already #f
                        }
                        sl_gc_remove_root(&result);  // Unroot intermediate result
                    }
                    current_node = next_node;
                }

                // Check for improper list of arguments
                if (current_node != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed 'and' (improper argument list)");
                }
                // If loop finished normally (e.g. empty args), result is SL_TRUE.
                // If error occurred, result holds the error.
                // If short-circuited, result is SL_FALSE.

                sl_gc_remove_root(&args);  // Final unroot of args list
                break;                     // Break from switch case 'and'
            }  // End AND block
            // --- OR --- <<< ADDED BLOCK
            else if (strcmp(op_name, "or") == 0) {
                // (or expr1 expr2 ...)
                // Short-circuits on first non-#f value. Returns #f if all #f. Returns #f if no args.
                sl_gc_add_root(&args);  // Root the list of expressions
                result = SL_FALSE;      // Default for (or)

                sl_object *current_node = args;
                while (sl_is_pair(current_node)) {
                    sl_object *expr_to_eval = sl_car(current_node);
                    sl_object *next_node = sl_cdr(current_node);

                    // Check for tail call position (last expression)
                    if (next_node == SL_NIL) {
                        sl_gc_remove_root(&args);  // Unroot args list
                        obj = expr_to_eval;        // Set up for tail call
                        goto top_of_eval;          // Jump!
                    } else {
                        // Not the last expression, evaluate normally
                        result = sl_eval(expr_to_eval, env);  // MIGHT GC
                        sl_gc_add_root(&result);              // Root intermediate result

                        if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                            sl_gc_remove_root(&args);  // Unroot before cleanup
                            goto cleanup;              // Error occurred
                        }

                        // Check for short-circuit condition (non-#f)
                        if (result != SL_FALSE) {
                            sl_gc_remove_root(&args);  // Unroot args list
                            goto cleanup;              // Result is the first truthy value
                        }
                        sl_gc_remove_root(&result);  // Unroot intermediate result (it was #f)
                    }
                    current_node = next_node;
                }

                // Check for improper list of arguments
                if (current_node != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed 'or' (improper argument list)");
                }
                // If loop finished normally (e.g. empty args), result is SL_FALSE.
                // If error occurred, result holds the error.
                // If short-circuited, result holds the first truthy value.

                sl_gc_remove_root(&args);  // Final unroot of args list
                break;                     // Break from switch case 'or'
            }  // End OR block

            // --- END OF SPECIAL FORMS ---
            // If none of the above matched, it's not a special form we handle here.
            // Fall through to the function call logic below.

        }  // end if (sl_is_symbol(op_obj))

        // --- Generic Function Call ---
        // If op_obj wasn't a symbol or wasn't a recognized special form symbol.
        sl_gc_add_root(&op_obj);  // Root operator object itself
        sl_gc_add_root(&args);    // Root arguments list

        sl_object *fn = sl_eval(op_obj, env);  // Evaluate the operator - MIGHT GC
        sl_gc_add_root(&fn);                   // Root the resulting function object

        if (fn == SL_OUT_OF_MEMORY_ERROR) {
            result = fn;
            goto cleanup_fn_call;
        }
        if (!sl_is_function(fn)) {
            char *op_str = sl_object_to_string(op_obj);
            result = sl_make_errorf("Eval: Not a function: %s", op_str ? op_str : "<?>");
            free(op_str);
            goto cleanup_fn_call;
        }

        // Evaluate arguments
        sl_object *evaled_args = sl_eval_list(args, env);  // Eval list - MIGHT GC
        sl_gc_add_root(&evaled_args);                      // Root the evaluated argument list

        // --- ADDED CHECK for errors from sl_eval_list ---
        if (evaled_args == SL_OUT_OF_MEMORY_ERROR || sl_is_error(evaled_args)) {
            result = evaled_args;  // Propagate the error from argument evaluation
            // Skip sl_apply, go directly to cleanup for evaled_args
            goto cleanup_args_call;
        }
        // --- END ADDED CHECK ---

        // Apply the function
        // Pass the main loop's state pointers (&obj, &env) to sl_apply
        result = sl_apply(fn, evaled_args, &obj, &env);  // <<< MODIFIED CALL

        // --- ADDED CHECK: Handle TCO signal from sl_apply ---
        if (result == SL_CONTINUE_EVAL) {
            // sl_apply signaled that a tail call is needed.
            // eval_sequence_with_defines (called by sl_apply) already updated obj and env.
            // Unroot locals specific to this path before jumping.
            sl_gc_remove_root(&evaled_args);
            sl_gc_remove_root(&fn);
            sl_gc_remove_root(&op_obj);
            sl_gc_remove_root(&args);
            goto top_of_eval;  // Jump back to the main loop start
        }
        // --- END ADDED CHECK ---
        // Otherwise, result holds the final value or an error from sl_apply

    cleanup_args_call:
        sl_gc_remove_root(&evaled_args);
    cleanup_fn_call:
        sl_gc_remove_root(&fn);
        // Also unroot op_obj and args used in this path
        sl_gc_remove_root(&op_obj);
        sl_gc_remove_root(&args);
        break;  // Break from switch case SL_TYPE_PAIR

    }  // end case SL_TYPE_PAIR

    default:
        result = sl_make_errorf("Eval: Unknown object type %d", obj->type);
        break;

    }  // end switch

cleanup:
    // --- Unroot locals before returning ---
    sl_gc_remove_root(&result);
    sl_gc_remove_root(&env);
    sl_gc_remove_root(&obj);
    return result;
}

// Helper to evaluate a sequence of expressions (like in begin or function body)
// Returns the result of the *last* expression, or an error.
// Handles TCO by jumping to top_of_eval in the caller (sl_eval).
// NOTE: This helper is NOT used in the current 'let' implementation above,
// the logic was integrated directly for TCO via goto top_of_eval.
// Keeping the signature here for potential future refactoring.
// static sl_object *eval_sequence(sl_object *seq, sl_object *env) { ... }

// Helper to evaluate a list of arguments
sl_object *sl_eval_list(sl_object *list, sl_object *env) {
    if (list == SL_NIL) {
        return SL_NIL;
    }
    if (!sl_is_pair(list)) {
        // Allow evaluating dotted lists? For now, require proper list.
        return sl_make_errorf("Eval: Invalid argument list structure (not a pair)");
    }

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    sl_object *current_expr = list;
    sl_object *return_value = SL_NIL;  // Variable to hold final return value (list head or error)

    // --- Root key variables ---
    sl_gc_add_root(&head);
    sl_gc_add_root(&env);
    sl_gc_add_root(&current_expr);
    // Do NOT root return_value itself, it just holds the final pointer

    while (current_expr != SL_NIL) {
        if (!sl_is_pair(current_expr)) {
            // Set error object to be returned
            return_value = sl_make_errorf("Eval: Improper argument list (dotted list?)");
            goto cleanup_eval_list;
        }
        sl_object *arg_expr = sl_car(current_expr);
        sl_object *evaled_arg = sl_eval(arg_expr, env);
        sl_gc_add_root(&evaled_arg);
        /*
        if (evaled_arg == SL_OUT_OF_MEMORY_ERROR) {
            return_value = evaled_arg;  // Set error object
            sl_gc_remove_root(&evaled_arg);
            goto cleanup_eval_list;
        }
        */

        // --- Check for ANY error from sl_eval --- <<< MODIFIED CHECK
        if (evaled_arg == SL_OUT_OF_MEMORY_ERROR || sl_is_error(evaled_arg)) {
            return_value = evaled_arg;  // Set error object
            sl_gc_remove_root(&evaled_arg);
            goto cleanup_eval_list;  // Propagate error immediately
        }
        // --- End Modified Check ---

        sl_object *new_pair = sl_make_pair(evaled_arg, SL_NIL);

        if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
            return_value = new_pair;  // Set error object
            sl_gc_remove_root(&evaled_arg);
            goto cleanup_eval_list;
        }

        *tail_ptr = new_pair;
        tail_ptr = &new_pair->data.pair.cdr;

        sl_gc_remove_root(&evaled_arg);
        current_expr = sl_cdr(current_expr);
    }

    return_value = head;  // Success: return the head of the built list

cleanup_eval_list:
    sl_gc_remove_root(&current_expr);
    sl_gc_remove_root(&env);
    sl_gc_remove_root(&head);
    return return_value;  // Return either the list head or an error object
}

// sl_object *sl_apply(sl_object *fn, sl_object *args) {
sl_object *sl_apply(sl_object *fn, sl_object *args, sl_object **obj_ptr, sl_object **env_ptr) {
    // --- Check for error during argument evaluation ---
    if (args == SL_OUT_OF_MEMORY_ERROR || sl_is_error(args)) {
        // fprintf(stderr, "[DEBUG sl_apply] Error detected in evaluated arguments, propagating directly.\n"); // Optional debug
        return args;  // Propagate the error returned by sl_eval_list
    }

    if (!sl_is_function(fn)) {
        char *fn_str = sl_object_to_string(fn);
        sl_object *err = sl_make_errorf("Apply: Not a function: %s", fn_str ? fn_str : "<?>");
        free(fn_str);
        return err;
    }
    if (!sl_is_list(args)) {
        return sl_make_errorf("Apply: Internal error - args is not a list (type: %s)", sl_type_name(args ? args->type : -1));
    }

    sl_object *result = SL_NIL;
    // --- Root key variables ---
    sl_gc_add_root(&fn);
    sl_gc_add_root(&args);
    sl_gc_add_root(&result);

    if (fn->data.function.is_builtin) {
        // Builtins cannot be tail called in this scheme. They execute and return.
        result = fn->data.function.def.builtin.func_ptr(args);
    } else {
        // Apply a user-defined closure
        sl_object *params = fn->data.function.def.closure.params;
        sl_object *body_list = fn->data.function.def.closure.body;
        sl_object *closure_env = fn->data.function.def.closure.env;

        // Root closure parts temporarily
        sl_gc_add_root(&params);
        sl_gc_add_root(&body_list);
        sl_gc_add_root(&closure_env);

        // Create a new environment for the call
        sl_object *call_env = sl_env_create(closure_env);
        CHECK_ALLOC_GOTO(call_env, cleanup_apply_closure, result);
        sl_gc_add_root(&call_env);

        // --- REVISED Binding Logic for Variadics ---
        sl_object *p = params;  // Current parameter specifier
        sl_object *a = args;    // Current argument list node
        bool bind_ok = true;
        sl_gc_add_root(&p);  // Root traversal pointers
        sl_gc_add_root(&a);

        if (sl_is_pair(p)) {  // Case 1 & 2: Proper or dotted list parameters (e.g., (a b) or (a b . rest))
            while (sl_is_pair(p) && sl_is_pair(a)) {
                // Bind one required parameter
                sl_object *param_sym = sl_car(p);
                if (!sl_is_symbol(param_sym)) {
                    result = sl_make_errorf("Apply: Parameter list contains non-symbol");
                    bind_ok = false;
                    break;
                }
                sl_env_define(call_env, param_sym, sl_car(a));
                // Check OOM?
                p = sl_cdr(p);
                a = sl_cdr(a);
            }

            if (!bind_ok) goto binding_done;  // Error occurred in loop

            if (sl_is_symbol(p)) {  // Case 2: Dotted list (e.g., (a b . rest)) - p is now the 'rest' symbol
                sl_object *rest_sym = p;
                // 'a' now points to the list node containing the first rest argument, or NIL if none.
                // The rest arguments are already evaluated and form a proper list.
                sl_env_define(call_env, rest_sym, a);  // Bind rest symbol to the remaining args list
                // Check OOM?
                // Arity check: Already consumed required args. Any number of rest args (>=0) is fine.
            } else if (p == SL_NIL && a == SL_NIL) {  // Case 1: Proper list, exact match
                // Correct number of arguments provided. Binding complete.
            } else {  // Case 1: Proper list, mismatch
                result = sl_make_errorf("Apply: Mismatched argument count (expected %s, got %s)",
                                        sl_is_nil(p) ? "fewer" : "more",
                                        sl_is_nil(a) ? "fewer" : "more");
                bind_ok = false;
            }
        } else if (sl_is_symbol(p)) {  // Case 3: Single symbol parameter (e.g., (lambda rest ...))
            sl_object *rest_sym = p;
            // Bind the symbol to the entire list of arguments
            sl_env_define(call_env, rest_sym, args);  // 'args' is the complete list
            // Check OOM?
            // Any number of arguments (>=0) is fine.
        } else if (p == SL_NIL) {  // Case: (lambda () ...)
            if (a != SL_NIL) {     // Provided arguments but expected none
                result = sl_make_errorf("Apply: Mismatched argument count (expected 0)");
                bind_ok = false;
            }
            // Else: p is NIL and a is NIL, correct arity (0). Binding complete.
        } else {  // Invalid parameter specification (should ideally be caught at lambda creation)
            result = sl_make_errorf("Apply: Invalid parameter specification object");
            bind_ok = false;
        }

    binding_done:
        sl_gc_remove_root(&p);  // Unroot traversal pointers
        sl_gc_remove_root(&a);
        // --- End REVISED Binding Logic ---

        if (bind_ok) {
            // --- Evaluate the body sequence using the helper ---
            // Pass the main loop's obj_ptr and env_ptr down.
            // <<< CORRECTED CALL to eval_sequence_with_defines >>>
            result = eval_sequence_with_defines(body_list, call_env, obj_ptr, env_ptr);

            // --- REMOVED BLOCK that called sl_eval recursively ---
            // If eval_sequence_with_defines returns SL_CONTINUE_EVAL,
            // it has already updated *obj_ptr and *env_ptr for the main loop.
            // sl_apply simply needs to return SL_CONTINUE_EVAL to signal the jump.
            // --- END REMOVED BLOCK ---
        }
        // else: result already holds the binding error

    cleanup_apply_closure:  // Label for cleanup within closure apply path
        sl_gc_remove_root(&call_env);
        sl_gc_remove_root(&params);
        sl_gc_remove_root(&body_list);
        sl_gc_remove_root(&closure_env);
    }

cleanup_apply:
    sl_gc_remove_root(&result);
    sl_gc_remove_root(&args);
    sl_gc_remove_root(&fn);
    // Return the final value, an error, OR SL_CONTINUE_EVAL
    return result;
}

// Parses and evaluates all S-expressions from the given null-terminated string
sl_object *sl_eval_string(const char *input, sl_object *env) {
    sl_object *last_result = SL_NIL;  // Default result if string is empty/only whitespace
    sl_object *expr = NULL;
    const char *parse_ptr = input;
    const char *end_ptr = NULL;

    // Root environment and potentially changing last_result/expr
    sl_gc_add_root(&env);
    sl_gc_add_root(&last_result);
    sl_gc_add_root(&expr);  // Root expr slot once

    while (true) {
        // --- Skip whitespace AND comments before parsing the next expression ---
        // while (isspace(*parse_ptr)) { // <<< OLD CODE
        //     parse_ptr++;
        // }
        skip_whitespace_and_comments(&parse_ptr);  // <<< CORRECTED: Use the function that handles comments

        // Check if we reached the end of the string after skipping
        if (*parse_ptr == '\0') {
            break;  // Done processing the string
        }

        // --- Parse ---
        // Store the start position in case of error
        const char *current_expr_start = parse_ptr;
        expr = sl_parse_string(parse_ptr, &end_ptr);
        // Note: expr is already rooted, sl_parse_string result overwrites the rooted slot

        if (expr == SL_NIL) {
            // Parsing failed (sl_parse_string should print details)
            // Or end of input was reached unexpectedly within parse attempt
            // Return a generic error, assuming sl_parse_string reported specifics
            last_result = sl_make_errorf("Error parsing expression starting near: %.*s", 30, current_expr_start);
            break;  // Stop processing on parse error
        }
        if (expr == SL_OUT_OF_MEMORY_ERROR) {
            last_result = expr;  // Propagate OOM
            break;
        }

        // --- Evaluate ---
        // Unroot previous result before eval potentially overwrites it via last_result
        sl_gc_remove_root(&last_result);
        last_result = sl_eval(expr, env);
        sl_gc_add_root(&last_result);  // Re-root the new result

        // Check for evaluation errors
        if (last_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(last_result)) {
            // Evaluation error occurred, stop processing
            break;
        }

        // Update parse_ptr to continue after the parsed expression
        parse_ptr = end_ptr;

        // Optional: Trigger GC periodically?
        // sl_gc();
    }

    // Clean up roots
    sl_gc_remove_root(&expr);
    sl_gc_remove_root(&last_result);
    sl_gc_remove_root(&env);

    return last_result;  // Return the result of the last evaluation or an error
}

// Reads and evaluates all S-expressions from a stream in the given environment.
// Simple implementation: Read whole stream to buffer, then call sl_eval_string.
// WARNING: Inefficient for large files. Consider true stream parsing later.
sl_object *sl_eval_stream(FILE *stream, sl_object *env) {
    // Determine file size
    fseek(stream, 0, SEEK_END);
    long file_size = ftell(stream);
    if (file_size < 0) {
        return sl_make_errorf("Error getting stream size");
    }
    fseek(stream, 0, SEEK_SET);  // Rewind

    // Allocate buffer (+1 for null terminator)
    // Check for potential overflow if file_size is huge
    if (file_size >= SIZE_MAX) {
        return sl_make_errorf("Stream too large to read into memory");
    }
    size_t buffer_size = (size_t)file_size + 1;
    char *buffer = malloc(buffer_size);
    if (!buffer) {
        return SL_OUT_OF_MEMORY_ERROR;  // Use the standard OOM error object
    }

    // Read the whole file
    size_t bytes_read = fread(buffer, 1, file_size, stream);
    if (bytes_read != (size_t)file_size) {
        free(buffer);
        // Check ferror(stream) vs feof(stream) if needed
        return sl_make_errorf("Error reading from stream");
    }
    buffer[bytes_read] = '\0';  // Null-terminate

    // Evaluate the buffer content
    sl_object *result = sl_eval_string(buffer, env);

    // Free the buffer
    free(buffer);

    return result;
}

// Loads all files ending in ".scm" from the specified directory path.
// Returns SL_TRUE on success (all files loaded without error),
// or an error object if directory cannot be opened or a file fails to load.
sl_object *sl_load_directory(const char *dir_path, sl_object *env) {
    DIR *dir;
    struct dirent *entry;
    sl_object *last_result = SL_TRUE;  // Assume success initially

    dir = opendir(dir_path);
    if (!dir) {
        return sl_make_errorf("load-directory: Cannot open directory '%s' (%s)", dir_path, strerror(errno));
    }

    // Root env and last_result
    sl_gc_add_root(&env);
    sl_gc_add_root(&last_result);

    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        size_t name_len = strlen(name);
        const char *ext = ".scm";
        size_t ext_len = strlen(ext);

        // Check if filename ends with ".scm"
        if (name_len > ext_len && strcmp(name + name_len - ext_len, ext) == 0) {
            // Construct full path
            // Need space for dir_path + '/' + name + '\0'
            size_t full_path_len = strlen(dir_path) + 1 + name_len + 1;
            char *full_path = malloc(full_path_len);
            if (!full_path) {
                last_result = SL_OUT_OF_MEMORY_ERROR;
                break;  // OOM error
            }
            // Ensure directory path doesn't end with '/' before appending
            if (dir_path[strlen(dir_path) - 1] == '/') {
                snprintf(full_path, full_path_len, "%s%s", dir_path, name);
            } else {
                snprintf(full_path, full_path_len, "%s/%s", dir_path, name);
            }

            // Check if it's actually a file (and not a directory ending in .scm)
            struct stat path_stat;
            if (stat(full_path, &path_stat) == 0 && S_ISREG(path_stat.st_mode)) {
                FILE *file = fopen(full_path, "r");
                if (!file) {
                    fprintf(stderr, "Warning: Could not open library file '%s' (%s), skipping.\n", full_path, strerror(errno));
                    // Optionally make this a hard error:
                    // last_result = sl_make_errorf("load-directory: Cannot open file '%s' (%s)", full_path, strerror(errno));
                    // free(full_path);
                    // break;
                } else {
                    printf("Loading library file: %s\n", full_path);  // Debug message
                    // Evaluate the file
                    sl_object *eval_res = sl_eval_stream(file, env);
                    fclose(file);

                    // Check for evaluation errors
                    if (eval_res == SL_OUT_OF_MEMORY_ERROR || sl_is_error(eval_res)) {
                        // Report error from file loading
                        char *err_str = sl_object_to_string(eval_res);
                        fprintf(stderr, "Error loading library file '%s': %s\n", full_path, err_str ? err_str : "Unknown error");
                        free(err_str);
                        last_result = eval_res;  // Store the error
                        free(full_path);
                        break;  // Stop loading on error
                    }
                    // Discard successful result of individual file loading
                }
            } else {
                // It's not a regular file or stat failed, skip it silently or add warning
                // fprintf(stderr, "Warning: Skipping non-regular file '%s'.\n", full_path);
            }
            free(full_path);
        }  // end if .scm
    }  // end while readdir

    closedir(dir);

    // Unroot locals
    sl_gc_remove_root(&last_result);
    sl_gc_remove_root(&env);

    // Return SL_TRUE if loop completed without break, otherwise return the error.
    return last_result;
}
