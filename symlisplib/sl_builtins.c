#include "sl_builtins.h"
#include "sl_core.h"
#include "sl_env.h"
#include <stdio.h>
#include <string.h>  // For strcmp in symbol creation (temp)
#include <gmp.h>     // Include GMP

// Helper to check arity.
// Returns SL_TRUE if arity matches and list is proper.
// Returns an SL_TYPE_ERROR object otherwise.
static sl_object *check_arity(const char *func_name, sl_object *args, size_t expected) {
    size_t count = 0;
    sl_object *current = args;
    while (sl_is_pair(current)) {
        count++;
        current = sl_cdr(current);
    }
    if (current != SL_NIL) {  // Improper list
        return sl_make_errorf("Error (%s): Improper argument list provided.", func_name);
    }
    if (count != expected) {
        return sl_make_errorf("Error (%s): Expected %zu arguments, got %zu.", func_name, expected, count);
    }
    return SL_TRUE;  // Arity is correct
}

// Helper to check arity for functions taking at least N arguments.
// Returns SL_TRUE if arity is >= min_expected and list is proper.
// Returns an SL_TYPE_ERROR object otherwise.
static sl_object *check_arity_min(const char *func_name, sl_object *args, size_t min_expected) {
    size_t count = 0;
    sl_object *current = args;
    while (sl_is_pair(current)) {
        count++;
        current = sl_cdr(current);
    }
    if (current != SL_NIL) {  // Improper list
        return sl_make_errorf("Error (%s): Improper argument list provided.", func_name);
    }
    if (count < min_expected) {
        return sl_make_errorf("Error (%s): Expected at least %zu arguments, got %zu.", func_name, min_expected, count);
    }
    return SL_TRUE;  // Arity is correct
}

// Helper to get a number object's value as mpq_t
// Initializes 'out' - caller must clear 'out'
static bool get_number_as_mpq(sl_object *obj, mpq_t out, const char *func_name) {
    if (!sl_is_number(obj)) {
        // Error object creation needs refinement
        sl_make_errorf("Error (%s): Expected a number, got type %d.", func_name, obj ? obj->type : -1);
        return false;
    }
    mpq_init(out);  // Initialize the output mpq_t
    if (obj->data.number.is_bignum) {
        mpq_set(out, obj->data.number.value.big_num);
    } else {
        mpq_set_si(out, obj->data.number.value.small_num.num, obj->data.number.value.small_num.den);
        mpq_canonicalize(out);  // Ensure small numbers are also canonical
    }
    return true;
}

// Helper to create a number object from mpq_t, simplifying if possible
static sl_object *make_number_from_mpq(mpq_t val) {
    mpq_canonicalize(val);  // Ensure result is canonical

    mpz_t num_z, den_z;
    mpz_inits(num_z, den_z, NULL);
    mpq_get_num(num_z, val);
    mpq_get_den(den_z, val);

    sl_object *result = NULL;
    // Check if denominator is 1 and numerator fits int64_t
    if (mpz_cmp_si(den_z, 1) == 0 && fits_int64(num_z)) {
        result = sl_make_number_si(mpz_get_si(num_z), 1);
    }
    // Check if both numerator and denominator fit int64_t
    else if (fits_int64(num_z) && fits_int64(den_z)) {
        result = sl_make_number_si(mpz_get_si(num_z), mpz_get_si(den_z));
    } else {
        // Doesn't fit small int, use the bignum
        result = sl_make_number_q(val);  // This copies val
    }

    mpz_clears(num_z, den_z, NULL);
    return result;
}

// --- Builtin Implementations ---

static sl_object *sl_builtin_car(sl_object *args) {
    sl_object *arity_check = check_arity("car", args, 1);
    if (arity_check != SL_TRUE) return arity_check;  // Return error object directly

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        // Use sl_object_to_string_buf for better error message if available
        // char arg_str[64];
        // sl_object_to_string_buf(pair, arg_str, sizeof(arg_str));
        // return sl_make_errorf("Error (car): Argument must be a pair, got %s.", arg_str);
        return sl_make_errorf("Error (car): Argument must be a pair, got type %d.", pair ? pair->type : -1);
    }
    return sl_car(pair);
}

static sl_object *sl_builtin_cdr(sl_object *args) {
    sl_object *arity_check = check_arity("cdr", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (cdr): Argument must be a pair, got type %d.", pair ? pair->type : -1);
    }
    return sl_cdr(pair);
}

static sl_object *sl_builtin_cons(sl_object *args) {
    sl_object *arity_check = check_arity("cons", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *car_val = sl_car(args);          // First argument
    sl_object *cdr_val = sl_car(sl_cdr(args));  // Second argument

    return sl_make_pair(car_val, cdr_val);
}

// (+) or (+ x y ...)
static sl_object *sl_builtin_add(sl_object *args) {
    // No specific arity check needed for +, 0 args is valid (returns 0)
    mpq_t sum, temp;
    mpq_init(sum);  // Initialize sum to 0/1

    sl_object *current = args;
    while (sl_is_pair(current)) {
        sl_object *arg = sl_car(current);
        if (!get_number_as_mpq(arg, temp, "+")) {
            mpq_clear(sum);
            // TODO: Return the error from get_number_as_mpq if possible
            return sl_make_errorf("Error (+): Non-number argument found.");
        }
        mpq_add(sum, sum, temp);
        mpq_clear(temp);
        current = sl_cdr(current);
    }
    if (current != SL_NIL) {  // Improper list
        mpq_clear(sum);
        return sl_make_errorf("Error (+): Improper argument list.");
    }

    sl_object *result = make_number_from_mpq(sum);
    mpq_clear(sum);
    return result;
}

// (- x) or (- x y ...)
static sl_object *sl_builtin_sub(sl_object *args) {
    sl_object *arity_check = check_arity_min("-", args, 1);  // Need at least 1 arg
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t result_q, temp;

    sl_object *first_arg = sl_car(args);
    // get_number_as_mpq initializes result_q if it returns true
    if (!get_number_as_mpq(first_arg, result_q, "-")) {
        // mpq_init(result_q) was called above, but get_number_as_mpq failed
        // before re-initializing it. We still need to clear the initial one.
        mpq_clear(result_q);
        return sl_make_errorf("Error (-): Non-number argument found (first).");
    }

    sl_object *current = sl_cdr(args);
    if (current == SL_NIL) {
        // Unary negation
        mpq_neg(result_q, result_q);
    } else {
        // Binary/N-ary subtraction
        while (sl_is_pair(current)) {
            sl_object *arg = sl_car(current);
            // get_number_as_mpq initializes temp if it returns true
            bool got_num = get_number_as_mpq(arg, temp, "-");
            if (!got_num) {
                // Failed to get number, temp was NOT initialized by get_number_as_mpq.
                // Only clear result_q which was initialized earlier.
                mpq_clear(result_q);
                return sl_make_errorf("Error (-): Non-number argument found (subsequent).");
            }
            // If we got here, temp WAS initialized by get_number_as_mpq
            mpq_sub(result_q, result_q, temp);
            mpq_clear(temp);  // Clear temp now that we're done with it for this iteration
            current = sl_cdr(current);
        }
        // Improper list check already done by check_arity_min
    }

    sl_object *result_obj = make_number_from_mpq(result_q);
    mpq_clear(result_q);  // Clear result_q after potentially creating result_obj
    return result_obj;
}

// (*) or (* x y ...)
static sl_object *sl_builtin_mul(sl_object *args) {
    // No specific arity check needed for *, 0 args is valid (returns 1)
    mpq_t product, temp;
    mpq_init(product);
    mpq_set_si(product, 1, 1);  // Initialize product to 1/1

    sl_object *current = args;
    while (sl_is_pair(current)) {
        sl_object *arg = sl_car(current);
        if (!get_number_as_mpq(arg, temp, "*")) {
            mpq_clear(product);
            // TODO: Return error from get_number_as_mpq
            return sl_make_errorf("Error (*): Non-number argument found.");
        }
        mpq_mul(product, product, temp);
        mpq_clear(temp);
        current = sl_cdr(current);
    }
    if (current != SL_NIL) {  // Improper list
        mpq_clear(product);
        return sl_make_errorf("Error (*): Improper argument list.");
    }

    sl_object *result = make_number_from_mpq(product);
    mpq_clear(product);
    return result;
}

// (/ x) or (/ x y ...)
static sl_object *sl_builtin_div(sl_object *args) {
    sl_object *arity_check = check_arity_min("/", args, 1);  // Need at least 1 arg
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t result_q, temp;

    sl_object *first_arg = sl_car(args);
    // get_number_as_mpq initializes result_q if it returns true
    if (!get_number_as_mpq(first_arg, result_q, "/")) {
        // mpq_init(result_q) was called above, but get_number_as_mpq failed.
        // Clear the initial one.
        mpq_clear(result_q);
        return sl_make_errorf("Error (/): Non-number argument found (first).");
    }

    sl_object *current = sl_cdr(args);
    if (current == SL_NIL) {
        // Reciprocal
        mpq_t one;
        mpq_init(one);
        mpq_set_si(one, 1, 1);
        if (mpq_sgn(result_q) == 0) {  // Check division by zero
            // Clear both result_q and one before returning error
            mpq_clears(result_q, one, NULL);
            return sl_make_errorf("Error (/): Division by zero (reciprocal).");
        }
        mpq_div(result_q, one, result_q);
        mpq_clear(one);  // Clear one
    } else {
        // Binary/N-ary division
        while (sl_is_pair(current)) {
            sl_object *arg = sl_car(current);
            // get_number_as_mpq initializes temp if it returns true
            bool got_num = get_number_as_mpq(arg, temp, "/");
            if (!got_num) {
                // Failed to get number, temp was NOT initialized by get_number_as_mpq.
                // Only clear result_q.
                mpq_clear(result_q);
                return sl_make_errorf("Error (/): Non-number argument found (subsequent).");
            }
            // If we got here, temp WAS initialized by get_number_as_mpq
            if (mpq_sgn(temp) == 0) {  // Check division by zero
                // Clear both result_q and the initialized temp before returning error
                mpq_clears(result_q, temp, NULL);
                return sl_make_errorf("Error (/): Division by zero.");
            }
            mpq_div(result_q, result_q, temp);
            mpq_clear(temp);  // Clear temp now that we're done with it for this iteration
            current = sl_cdr(current);
        }
        // Improper list check already done by check_arity_min
    }

    sl_object *result_obj = make_number_from_mpq(result_q);
    mpq_clear(result_q);  // Clear result_q after potentially creating result_obj
    return result_obj;
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

    // Add math builtins
    define_builtin(global_env, "+", sl_builtin_add);
    define_builtin(global_env, "-", sl_builtin_sub);
    define_builtin(global_env, "*", sl_builtin_mul);
    define_builtin(global_env, "/", sl_builtin_div);

    // Add more builtins here later...
    // define_builtin(global_env, "=", sl_builtin_num_eq);
    // define_builtin(global_env, "<", sl_builtin_num_lt);
    // define_builtin(global_env, ">", sl_builtin_num_gt);
    // define_builtin(global_env, "eq?", sl_builtin_eq_q);
    // define_builtin(global_env, "pair?", sl_builtin_pair_q);
    // define_builtin(global_env, "null?", sl_builtin_null_q);
    // define_builtin(global_env, "display", sl_builtin_display);
    // define_builtin(global_env, "newline", sl_builtin_newline);
}