#include "sl_eval.h"
#include "sl_core.h"
#include "sl_env.h"
#include "sl_parse.h"  // Include sl_parse.h for sl_object_to_string_buf
#include <stdio.h>
#include <string.h>  // For strcmp

// Forward declaration for apply
static sl_object *apply(sl_object *proc, sl_object *args);

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

sl_object *sl_eval(sl_object *expr, sl_object *env_obj) {
    if (!expr) {
        return sl_make_errorf("Internal Error: sl_eval received NULL expression.");
    }
    if (!sl_is_env(env_obj)) {
        return sl_make_errorf("Internal Error: sl_eval received invalid environment object.");
    }

    // --- Trampoline loop for Tail Call Optimization (future) ---
    // while (true) { // TCO loop

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
        // quote
        if (sl_is_symbol(op) && strcmp(sl_symbol_name(op), "quote") == 0) {
            size_t arg_count = list_length(args);
            if (arg_count != 1) {
                return sl_make_errorf("Syntax Error (quote): Expected 1 argument, got %zu", arg_count);
            }
            // Return the datum unevaluated
            return sl_car(args);
        }

        // TODO: if
        // TODO: define
        // TODO: lambda
        // TODO: set!

        // --- If not a special form, it's a procedure call ---

        // 1. Evaluate the operator
        sl_object *proc = sl_eval(op, env_obj);
        if (sl_is_error(proc)) {
            return proc;  // Propagate error from operator evaluation
        }
        if (!sl_is_function(proc)) {
            // Try to print the operator that failed
            char op_str[100];                                     // Limited buffer
            sl_object_to_string_buf(op, op_str, sizeof(op_str));  // Need this helper
            return sl_make_errorf("Not a procedure: %s", op_str);
        }

        // 2. Evaluate the arguments
        sl_object *evaled_args = eval_list(args, env_obj);
        if (sl_is_error(evaled_args)) {
            return evaled_args;  // Propagate error from argument evaluation
        }

        // 3. Apply the procedure to the evaluated arguments
        return apply(proc, evaled_args);

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

    // } // end while(true) for TCO
}

// Apply a procedure (function object) to a list of evaluated arguments
static sl_object *apply(sl_object *proc, sl_object *args) {
    if (!sl_is_function(proc)) {
        return sl_make_errorf("Internal Error (apply): proc is not a function.");
    }
    // Args should be a proper list of evaluated values, or NIL
    // We might rely on eval_list to ensure this, or add checks here.

    if (proc->data.function.is_builtin) {
        // Call the C function pointer for the builtin
        return proc->data.function.def.builtin.func_ptr(args);
    } else {
        // --- Closure Application ---
        // TODO: Implement closure application
        // 1. Create new environment extending the closure's captured env
        // 2. Bind parameters to arguments in the new env
        // 3. Evaluate the closure body in the new env (tail call?)
        return sl_make_errorf("Closure application not yet implemented.");
    }
}