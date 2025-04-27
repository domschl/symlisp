#include "sl_builtins.h"
#include "sl_core.h"
#include "sl_env.h"
#include <stdio.h>
#include <string.h>  // For strcmp in symbol creation (temp)

// Helper to check arity (can be expanded)
static bool check_arity(const char *func_name, sl_object *args, size_t expected) {
    size_t count = 0;
    sl_object *current = args;
    while (sl_is_pair(current)) {
        count++;
        current = sl_cdr(current);
    }
    if (current != SL_NIL) {  // Improper list
        sl_make_errorf("Error (%s): Improper argument list.", func_name);
        return false;
    }
    if (count != expected) {
        sl_make_errorf("Error (%s): Expected %zu arguments, got %zu.", func_name, expected, count);
        return false;
    }
    return true;
}

// --- Builtin Implementations ---

static sl_object *sl_builtin_car(sl_object *args) {
    if (!check_arity("car", args, 1)) {
        // Error object already created by check_arity
        // Need a way to return the last created error...
        // For now, let's create another one or assume check_arity returns it.
        // Let's modify check_arity slightly for now.
        // A better approach is needed (e.g., thread-local error state or return error obj).
        return sl_make_errorf("Error (car): Arity check failed.");  // Placeholder
    }

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (car): Argument must be a pair, got %d.", pair ? pair->type : -1);
    }
    return sl_car(pair);  // Return the car of the argument pair
}

static sl_object *sl_builtin_cdr(sl_object *args) {
    if (!check_arity("cdr", args, 1)) {
        return sl_make_errorf("Error (cdr): Arity check failed.");  // Placeholder
    }

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (cdr): Argument must be a pair, got %d.", pair ? pair->type : -1);
    }
    return sl_cdr(pair);  // Return the cdr of the argument pair
}

static sl_object *sl_builtin_cons(sl_object *args) {
    if (!check_arity("cons", args, 2)) {
        return sl_make_errorf("Error (cons): Arity check failed.");  // Placeholder
    }

    sl_object *car_val = sl_car(args);          // First argument
    sl_object *cdr_val = sl_car(sl_cdr(args));  // Second argument

    return sl_make_pair(car_val, cdr_val);
}

// --- Builtin Initialization ---

// Helper to define a builtin
static void define_builtin(sl_object *env, const char *name, sl_object *(*func_ptr)(sl_object *args)) {
    // TODO: Use interned symbols once available
    sl_object *sym = sl_make_symbol(name);
    sl_object *func = sl_make_builtin(name, func_ptr);
    if (sym && func) {  // Check allocation success
        sl_env_define(env, sym, func);
    } else {
        fprintf(stderr, "FATAL: Failed to allocate symbol or builtin for '%s'\n", name);
        // Consider exiting or more robust error handling
    }
}

void sl_builtins_init(sl_object *global_env) {
    if (!sl_is_env(global_env)) {
        fprintf(stderr, "FATAL: sl_builtins_init received invalid global environment.\n");
        return;
    }

    define_builtin(global_env, "car", sl_builtin_car);
    define_builtin(global_env, "cdr", sl_builtin_cdr);
    define_builtin(global_env, "cons", sl_builtin_cons);

    // Add more builtins here later...
    // define_builtin(global_env, "+", sl_builtin_add);
    // define_builtin(global_env, "-", sl_builtin_sub);
    // define_builtin(global_env, "*", sl_builtin_mul);
    // define_builtin(global_env, "/", sl_builtin_div);
    // define_builtin(global_env, "=", sl_builtin_num_eq);
    // define_builtin(global_env, "<", sl_builtin_num_lt);
    // define_builtin(global_env, ">", sl_builtin_num_gt);
    // define_builtin(global_env, "eq?", sl_builtin_eq_q);
    // define_builtin(global_env, "pair?", sl_builtin_pair_q);
    // define_builtin(global_env, "null?", sl_builtin_null_q);
    // define_builtin(global_env, "display", sl_builtin_display);
    // define_builtin(global_env, "newline", sl_builtin_newline);
}