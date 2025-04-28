#include "sl_eval.h"
#include "sl_core.h"
#include "sl_env.h"
#include "sl_parse.h"
#include <stdio.h>
#include <string.h>

// Forward declaration for apply
static sl_object *apply(sl_object *proc, sl_object *args, sl_object **expr_out, sl_object **env_out);

// Helper to get list length
static size_t list_length(sl_object *list) {
    size_t len = 0;
    while (sl_is_pair(list)) {
        len++;
        list = sl_cdr(list);
    }
    // Proper lists end in NIL, dotted lists don't count the last element here
    return (list == SL_NIL) ? len : (size_t)-1;  // Return -1 for improper lists
}

// Helper to evaluate a list of arguments
static sl_object *eval_list(sl_object *args, sl_object *env_obj) {
    if (args == SL_NIL) {
        return SL_NIL;  // No arguments evaluate to an empty list
    }
    if (!sl_is_pair(args)) {
        return sl_make_errorf("Internal Error: Invalid argument list structure in eval_list.");
    }

    sl_object *first_arg_val = sl_eval(sl_car(args), env_obj);
    if (sl_is_error(first_arg_val)) {
        return first_arg_val;  // Propagate error
    }

    sl_object *rest_args_val = eval_list(sl_cdr(args), env_obj);
    if (sl_is_error(rest_args_val)) {
        // TODO: Need GC protection for first_arg_val if rest_args_val fails and GC runs?
        return rest_args_val;  // Propagate error
    }

    return sl_make_pair(first_arg_val, rest_args_val);
}

// Helper to bind parameters to arguments in a new environment
static sl_object *bind_params(sl_object *params, sl_object *args, sl_object *outer_env) {
    sl_object *call_env = sl_env_create(outer_env);
    if (!call_env) return SL_NIL;  // Allocation failed

    sl_object *current_param = params;
    sl_object *current_arg = args;

    while (sl_is_pair(current_param)) {
        if (!sl_is_pair(current_arg)) {
            // Too few arguments provided
            // TODO: GC call_env? Relies on next GC cycle for now.
            return sl_make_errorf("Arity Mismatch: Too few arguments provided.");
        }
        sl_object *param_sym = sl_car(current_param);
        if (!sl_is_symbol(param_sym)) {
            // TODO: GC call_env?
            return sl_make_errorf("Internal Error (lambda): Non-symbol found in parameter list.");
        }
        sl_object *arg_val = sl_car(current_arg);

        // Define the parameter in the new environment
        sl_env_define(call_env, param_sym, arg_val);  // Uses name comparison currently

        current_param = sl_cdr(current_param);
        current_arg = sl_cdr(current_arg);
    }

    if (current_param != SL_NIL) {
        // TODO: Handle variadic functions (e.g., (lambda x body) or (lambda (a b . c) body))
        // For now, assume fixed arity, so if params remain, it's an internal error?
        return sl_make_errorf("Internal Error (lambda): Parameter list structure error.");
    }

    if (current_arg != SL_NIL) {
        // Too many arguments provided
        // TODO: GC call_env?
        return sl_make_errorf("Arity Mismatch: Too many arguments provided.");
    }

    return call_env;  // Return the newly created environment with bindings
}

sl_object *sl_eval(sl_object *expr, sl_object *env_obj) {
    sl_object *eval_result = SL_NIL;

tail_call:
    if (!expr) {
        return sl_make_errorf("Internal Error: sl_eval received NULL expression.");
    }
    if (!sl_is_env(env_obj)) {
        return sl_make_errorf("Internal Error: sl_eval received invalid environment object.");
    }

    switch (expr->type) {
    // --- Self-Evaluating Types ---
    case SL_TYPE_NIL:
    case SL_TYPE_BOOLEAN:
    case SL_TYPE_NUMBER:
    case SL_TYPE_STRING:
    case SL_TYPE_ERROR:
        return expr;

    // --- Symbol Lookup ---
    case SL_TYPE_SYMBOL: {
        sl_object *value = sl_env_lookup(env_obj, expr);
        if (value == NULL) {
            return sl_make_errorf("Unbound variable: %s", sl_symbol_name(expr));
        }
        return value;
    }

    // --- List Evaluation ---
    case SL_TYPE_PAIR: {
        sl_object *op = sl_car(expr);
        sl_object *args = sl_cdr(expr);

        // --- Check for Special Forms ---

        // (quote <datum>)
        if (sl_is_symbol(op) && strcmp(sl_symbol_name(op), "quote") == 0) {
            size_t arg_count = list_length(args);
            if (arg_count != 1) {
                return sl_make_errorf("Syntax Error (quote): Expected 1 argument, got %zu", arg_count);
            }
            // Return the datum unevaluated
            return sl_car(args);
        }

        // (lambda <params> <body>)
        if (sl_is_symbol(op) && strcmp(sl_symbol_name(op), "lambda") == 0) {
            // Validate syntax
            if (!sl_is_pair(args) || !sl_is_pair(sl_cdr(args)) || sl_cdr(sl_cdr(args)) != SL_NIL) {
                return sl_make_errorf("Syntax Error (lambda): Expected (lambda <params> <body>).");
            }
            sl_object *params = sl_car(args);
            sl_object *body = sl_car(sl_cdr(args));
            // Validate params
            sl_object *p = params;
            while (sl_is_pair(p)) {
                if (!sl_is_symbol(sl_car(p))) {
                    return sl_make_errorf("Syntax Error (lambda): Parameters must be symbols.");
                }
                p = sl_cdr(p);
            }
            if (p != SL_NIL) {  // Check for proper list termination
                // TODO: Allow single symbol for variadic later
                return sl_make_errorf("Syntax Error (lambda): Parameter list must be a proper list.");
            }
            // Create closure
            return sl_make_closure(params, body, env_obj);
        }

        // (define <var> <expr>) or (define (<var> <params...>) <body>)
        if (sl_is_symbol(op) && strcmp(sl_symbol_name(op), "define") == 0) {
            if (!sl_is_pair(args)) {  // Need at least a variable/formals and a value/body
                return sl_make_errorf("Syntax Error (define): Malformed definition.");
            }

            sl_object *var_or_formals = sl_car(args);
            sl_object *value_or_body = sl_cdr(args);  // This is a list of remaining args

            if (sl_is_symbol(var_or_formals)) {
                // --- Variable Definition: (define <var> <expr>) ---
                sl_object *var = var_or_formals;

                // Check syntax: exactly one expression after variable
                if (!sl_is_pair(value_or_body) || sl_cdr(value_or_body) != SL_NIL) {
                    return sl_make_errorf("Syntax Error (define): Expected exactly one expression after variable '%s'.", sl_symbol_name(var));
                }
                sl_object *expr_to_eval = sl_car(value_or_body);

                // Evaluate the expression (non-tail call)
                sl_object *value = sl_eval(expr_to_eval, env_obj);
                if (sl_is_error(value)) {
                    return value;  // Propagate error
                }

                // Define the variable in the current environment
                sl_env_define(env_obj, var, value);

                // Return the variable symbol (or some unspecified value like SL_NIL)
                return var;

            } else if (sl_is_pair(var_or_formals)) {
                // --- Function Definition: (define (<var> <params...>) <body>...) ---
                sl_object *formals = var_or_formals;
                sl_object *func_name_sym = sl_car(formals);
                sl_object *params = sl_cdr(formals);

                // TODO: Handle multi-expression body (implicit begin)
                if (!sl_is_pair(value_or_body) || sl_cdr(value_or_body) != SL_NIL) {
                    return sl_make_errorf("Syntax Error (define): Function definition requires exactly one body expression (for now).");
                }
                sl_object *body = sl_car(value_or_body);

                if (!sl_is_symbol(func_name_sym)) {
                    return sl_make_errorf("Syntax Error (define): Function name must be a symbol.");
                }

                // Validate params is a list of symbols (or NIL) - reuse lambda validation logic
                sl_object *p = params;
                while (sl_is_pair(p)) {
                    if (!sl_is_symbol(sl_car(p))) {
                        return sl_make_errorf("Syntax Error (define): Function parameters must be symbols.");
                    }
                    p = sl_cdr(p);
                }
                if (p != SL_NIL) {  // Check for proper list termination
                    // TODO: Allow single symbol for variadic later
                    return sl_make_errorf("Syntax Error (define): Parameter list must be a proper list.");
                }

                // Create the closure object directly (equivalent to `(define func_name (lambda (params...) body))`)
                sl_object *closure = sl_make_closure(params, body, env_obj);
                if (!closure) {
                    return sl_make_errorf("Internal Error: Failed to create closure during define.");
                }

                // Define the function name in the current environment
                sl_env_define(env_obj, func_name_sym, closure);

                // Return the function name symbol
                return func_name_sym;

            } else {
                // Invalid syntax for define
                return sl_make_errorf("Syntax Error (define): Expected symbol or list after define keyword.");
            }
        }  // end define

        // (if <test> <consequent> [<alternate>])
        if (sl_is_symbol(op) && strcmp(sl_symbol_name(op), "if") == 0) {
            // Syntax check: Need at least test and consequent
            if (!sl_is_pair(args) || !sl_is_pair(sl_cdr(args))) {
                return sl_make_errorf("Syntax Error (if): Malformed if expression. Expected (if <test> <consequent> [<alternate>]).");
            }

            sl_object *test_expr = sl_car(args);
            sl_object *consequent_expr = sl_car(sl_cdr(args));
            sl_object *alternate_expr = SL_NIL;  // Default if not provided

            sl_object *rest = sl_cdr(sl_cdr(args));
            if (sl_is_pair(rest)) {
                // Three arguments: (if test consequent alternate)
                alternate_expr = sl_car(rest);
                // Check for too many arguments
                if (sl_cdr(rest) != SL_NIL) {
                    return sl_make_errorf("Syntax Error (if): Too many arguments. Expected (if <test> <consequent> [<alternate>]).");
                }
            } else if (rest != SL_NIL) {
                // Improper list after consequent
                return sl_make_errorf("Syntax Error (if): Malformed if expression after consequent.");
            }
            // If rest is SL_NIL, it's the two-argument form, alternate_expr remains SL_NIL

            // Evaluate the test expression (Non-tail call)
            sl_object *test_result = sl_eval(test_expr, env_obj);
            if (sl_is_error(test_result)) {
                return test_result;  // Propagate error
            }

            // Determine which branch to take for the *next* evaluation cycle (TCO)
            if (test_result == SL_FALSE) {
                // Test is false, choose alternate (or SL_NIL if no alternate)
                expr = alternate_expr;  // Update expr for the next loop iteration
            } else {
                // Test is true (anything other than #f), choose consequent
                expr = consequent_expr;  // Update expr for the next loop iteration
            }

            // If the chosen branch is NIL (e.g., no alternate provided and test was false),
            // the next loop iteration will evaluate NIL, which returns NIL.
            // This handles the unspecified return value for the missing alternate case.
            if (expr == SL_NIL) {
                // Scheme often returns an unspecified value here. Let's return NIL.
                // We can return directly as evaluating NIL is trivial.
                return SL_NIL;
            }

            // The environment remains the same for the tail call
            goto tail_call;  // Jump to evaluate the chosen branch

        }  // end if

        // (set! <variable> <expression>)
        if (sl_is_symbol(op) && strcmp(sl_symbol_name(op), "set!") == 0) {
            // Syntax check: (set! <symbol> <expr>)
            if (!sl_is_pair(args) || !sl_is_pair(sl_cdr(args)) || sl_cdr(sl_cdr(args)) != SL_NIL) {
                return sl_make_errorf("Syntax Error (set!): Expected (set! <variable> <expression>).");
            }

            sl_object *var = sl_car(args);
            sl_object *value_expr = sl_car(sl_cdr(args));

            if (!sl_is_symbol(var)) {
                return sl_make_errorf("Syntax Error (set!): Variable name must be a symbol.");
            }

            // Evaluate the value expression (Non-tail call)
            sl_object *new_value = sl_eval(value_expr, env_obj);
            if (sl_is_error(new_value)) {
                return new_value;  // Propagate error
            }

            // Attempt to set the variable in the environment chain
            bool success = sl_env_set(env_obj, var, new_value);

            if (!success) {
                return sl_make_errorf("Error (set!): Variable '%s' is not defined.", sl_symbol_name(var));
            }

            // Return value is unspecified, return SL_NIL
            return SL_NIL;
        }  // end set!

        // TODO: begin

        // --- If not a special form, it's a procedure call ---

        // 1. Evaluate the operator (Non-tail call)
        sl_object *proc = sl_eval(op, env_obj);
        if (sl_is_error(proc)) {
            return proc;
        }
        if (!sl_is_function(proc)) {
            char op_str[100];
            sl_object_to_string_buf(op, op_str, sizeof(op_str));
            return sl_make_errorf("Not a procedure: %s", op_str);
        }

        // 2. Evaluate the arguments (Non-tail call)
        sl_object *evaled_args = eval_list(args, env_obj);
        if (sl_is_error(evaled_args)) {
            return evaled_args;
        }

        // 3. Apply the procedure
        // Pass pointers to expr and env_obj for TCO modification
        eval_result = apply(proc, evaled_args, &expr, &env_obj);
        if (eval_result == SL_NIL) {  // Check for tail call
            goto tail_call;
        } else {
            return eval_result;
        }

    }  // end case SL_TYPE_PAIR

    // --- Other Types ---
    case SL_TYPE_FUNCTION:
        return sl_make_errorf("Cannot evaluate a raw function object directly.");
    case SL_TYPE_ENV:
        return sl_make_errorf("Cannot evaluate an environment object directly.");
    case SL_TYPE_FREE:
        return sl_make_errorf("Internal Error: Attempted to evaluate a FREE object.");
    default:
        return sl_make_errorf("Internal Error: Unknown object type %d encountered during evaluation.", expr->type);
    }  // end switch

    // Should not be reached if switch is exhaustive
    return sl_make_errorf("Internal Error: Fell through sl_eval switch.");
}

// Apply a procedure (function object) to a list of evaluated arguments
// Modifies expr_out and env_out and returns SL_NIL for tail calls.
// Returns the result directly for builtins or errors.
static sl_object *apply(sl_object *proc, sl_object *args, sl_object **expr_out, sl_object **env_out) {
    if (!sl_is_function(proc)) {
        return sl_make_errorf("Internal Error (apply): proc is not a function.");
    }

    if (sl_is_builtin(proc)) {
        // Call the C function pointer for the builtin
        // Builtins never perform tail calls in this model
        return sl_builtin_ptr(proc)(args);
    } else {
        // --- Closure Application ---
        sl_object *params = sl_closure_params(proc);
        sl_object *body = sl_closure_body(proc);
        sl_object *closure_env = sl_closure_env(proc);

        // 1. Create new environment extending the closure's captured env
        //    and bind parameters to arguments.
        sl_object *call_env = bind_params(params, args, closure_env);
        if (sl_is_error(call_env)) {
            return call_env;  // Return binding error (e.g., arity mismatch)
        }
        if (call_env == SL_NIL) {
            return sl_make_errorf("Internal Error: Failed to create call environment.");  // Allocation failed in bind_params
        }

        // 2. Prepare for tail call: Update expr and env pointers for sl_eval loop
        *expr_out = body;
        *env_out = call_env;

        // 3. Signal tail call by returning SL_NIL
        return SL_NIL;
    }
}