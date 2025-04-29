#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <gmp.h>
#include <errno.h>  // For errno used with fopen

#include "sl_builtins.h"
#include "sl_core.h"
#include "sl_env.h"
#include "sl_parse.h"
#include "sl_eval.h"  // <<< ADDED for sl_eval_stream

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

// Helper to check if a number object represents an integer and get its value as mpz_t
// Initializes 'out' - caller must clear 'out'
// Returns true on success, false on failure (non-number, non-integer, or error)
static bool get_number_as_mpz(sl_object *obj, mpz_t out, const char *func_name) {
    mpq_t temp_q;
    // get_number_as_mpq initializes temp_q if successful
    if (!get_number_as_mpq(obj, temp_q, func_name)) {
        // Error already printed by get_number_as_mpq or obj is not a number
        // temp_q is not initialized here.
        return false;
    }

    // Check if the rational is actually an integer (denominator is 1)
    mpz_t den_z;
    mpz_init(den_z);
    mpq_get_den(den_z, temp_q);

    if (mpz_cmp_si(den_z, 1) != 0) {
        // Not an integer
        mpz_clear(den_z);
        mpq_clear(temp_q);  // Clear the temporary rational
        // We need to return an error object from the calling builtin
        // This helper just signals failure.
        return false;
    }

    // It's an integer, get the numerator
    mpz_clear(den_z);          // Don't need denominator anymore
    mpz_init(out);             // Initialize the output mpz_t
    mpq_get_num(out, temp_q);  // Get the numerator into 'out'

    mpq_clear(temp_q);  // Clear the temporary rational
    return true;
}

// Helper to create a number object from mpz_t, simplifying if possible
static sl_object *make_number_from_mpz(mpz_t val) {
    // Check if it fits in int64_t
    if (fits_int64(val)) {  // Assuming fits_int64 checks mpz_t
        return sl_make_number_si(mpz_get_si(val), 1);
    } else {
        // Doesn't fit small int, use bignum integer
        return sl_make_number_z(val);  // Assumes this copies val
    }
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

// (modulo int1 int2)
static sl_object *sl_builtin_modulo(sl_object *args) {
    sl_object *arity_check = check_arity("modulo", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpz_t num1_z, num2_z, result_z;
    sl_object *arg1 = sl_car(args);
    sl_object *arg2 = sl_cadr(args);

    // Get arguments as integers, get_number_as_mpz initializes num1_z/num2_z
    if (!get_number_as_mpz(arg1, num1_z, "modulo")) {
        return sl_make_errorf("Error (modulo): First argument must be an integer.");
    }
    if (!get_number_as_mpz(arg2, num2_z, "modulo")) {
        mpz_clear(num1_z);  // Clear the first one which was initialized
        return sl_make_errorf("Error (modulo): Second argument must be an integer.");
    }

    // Check for division by zero
    if (mpz_sgn(num2_z) == 0) {
        mpz_clears(num1_z, num2_z, NULL);
        return sl_make_errorf("Error (modulo): Division by zero.");
    }

    // Calculate modulo (sign matches divisor) using Euclidean division remainder
    // Note: R7RS specifies floor division for modulo. mpz_fdiv_r gives remainder for floor division.
    // Let's re-read R7RS 6.2.6. It says `modulo` corresponds to `floor` division's remainder.
    // `remainder` corresponds to `truncate` division's remainder.
    // GMP: mpz_fdiv_r -> floor remainder. mpz_tdiv_r -> truncate remainder.
    // So, `modulo` should use `mpz_fdiv_r` and `remainder` should use `mpz_tdiv_r`. Let's swap.

    mpz_init(result_z);
    mpz_fdiv_r(result_z, num1_z, num2_z);  // Floor division remainder for modulo

    sl_object *result_obj = make_number_from_mpz(result_z);

    mpz_clears(num1_z, num2_z, result_z, NULL);  // Clear all mpz_t vars
    return result_obj;
}

// (remainder int1 int2) -> Truncate division remainder
static sl_object *sl_builtin_remainder(sl_object *args) {  // Overwrite previous definition
    sl_object *arity_check = check_arity("remainder", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpz_t num1_z, num2_z, result_z;
    sl_object *arg1 = sl_car(args);
    sl_object *arg2 = sl_cadr(args);

    if (!get_number_as_mpz(arg1, num1_z, "remainder")) {
        return sl_make_errorf("Error (remainder): First argument must be an integer.");
    }
    if (!get_number_as_mpz(arg2, num2_z, "remainder")) {
        mpz_clear(num1_z);
        return sl_make_errorf("Error (remainder): Second argument must be an integer.");
    }

    if (mpz_sgn(num2_z) == 0) {
        mpz_clears(num1_z, num2_z, NULL);
        return sl_make_errorf("Error (remainder): Division by zero.");
    }

    // Calculate remainder (sign matches dividend) using truncate division remainder
    mpz_init(result_z);
    mpz_tdiv_r(result_z, num1_z, num2_z);  // Truncate division remainder for remainder

    sl_object *result_obj = make_number_from_mpz(result_z);

    mpz_clears(num1_z, num2_z, result_z, NULL);
    return result_obj;
}

// (denominator num) -> Returns the denominator of a number (1 for integers).
static sl_object *sl_builtin_denominator(sl_object *args) {
    sl_object *arity_check = check_arity("denominator", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *num_obj = sl_car(args);
    if (!sl_is_number(num_obj)) {
        return sl_make_errorf("Error (denominator): Argument must be a number.");
    }

    sl_object *result = NULL;
    mpq_t num_q;
    mpz_t den_z;

    sl_gc_add_root(&num_obj);  // Protect input during potential error allocation

    if (!get_number_as_mpq(num_obj, num_q, "denominator")) {
        // Error already created by get_number_as_mpq
        result = sl_make_errorf("Error (denominator): Invalid number format.");  // Fallback
        mpq_clear(num_q);
    } else {
        mpz_init(den_z);
        mpq_get_den(den_z, num_q);                // Extract denominator
        result = sl_make_number_from_mpz(den_z);  // Create SymLisp number
        CHECK_ALLOC(result);
        mpq_clear(num_q);
        mpz_clear(den_z);
    }

    sl_gc_remove_root(&num_obj);
    return result;
}

// (numerator num) -> Returns the numerator of a number.
static sl_object *sl_builtin_numerator(sl_object *args) {
    sl_object *arity_check = check_arity("numerator", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *num_obj = sl_car(args);
    if (!sl_is_number(num_obj)) {
        return sl_make_errorf("Error (numerator): Argument must be a number.");
    }

    sl_object *result = NULL;
    mpq_t num_q;
    mpz_t num_z;

    sl_gc_add_root(&num_obj);  // Protect input

    if (!get_number_as_mpq(num_obj, num_q, "numerator")) {
        result = sl_make_errorf("Error (numerator): Invalid number format.");  // Fallback
        mpq_clear(num_q);
    } else {
        mpz_init(num_z);
        mpq_get_num(num_z, num_q);                // Extract numerator
        result = sl_make_number_from_mpz(num_z);  // Create SymLisp number
        CHECK_ALLOC(result);
        mpq_clear(num_q);
        mpz_clear(num_z);
    }

    sl_gc_remove_root(&num_obj);
    return result;
}

// (quotient n1 n2) -> Integer division n1 / n2 (truncating towards zero).
static sl_object *sl_builtin_quotient(sl_object *args) {
    sl_object *arity_check = check_arity("quotient", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *n1_obj = sl_car(args);
    sl_object *n2_obj = sl_cadr(args);

    if (!sl_is_number(n1_obj) || !sl_is_number(n2_obj)) {
        return sl_make_errorf("Error (quotient): Arguments must be numbers.");
    }
    if (!sl_number_is_integer(n1_obj)) {  // <<< UPDATED
        return sl_make_errorf("Error (quotient): First argument must be an integer.");
    }
    if (!sl_number_is_integer(n2_obj)) {  // <<< UPDATED
        return sl_make_errorf("Error (quotient): Second argument must be an integer.");
    }
    if (sl_number_is_zero(n2_obj)) {  // <<< UPDATED
        return sl_make_errorf("Error (quotient): Division by zero.");
    }

    sl_object *result = NULL;
    mpz_t n1_z, n2_z, quotient_z;
    mpz_inits(n1_z, n2_z, quotient_z, NULL);
    sl_gc_add_root(&n1_obj);  // Protect inputs
    sl_gc_add_root(&n2_obj);

    // Extract mpz values (we know they are integers)
    sl_number_get_z(n1_obj, n1_z);
    sl_number_get_z(n2_obj, n2_z);

    // Perform truncated division (tdiv)
    mpz_tdiv_q(quotient_z, n1_z, n2_z);

    result = sl_make_number_from_mpz(quotient_z);
    CHECK_ALLOC(result);

    sl_gc_remove_root(&n2_obj);
    sl_gc_remove_root(&n1_obj);
    mpz_clears(n1_z, n2_z, quotient_z, NULL);
    return result;
}

// (gcd n ...) -> Greatest Common Divisor of integers.
static sl_object *sl_builtin_gcd(sl_object *args) {
    // (gcd) -> 0
    if (args == SL_NIL) {
        return sl_make_number_si(0, 1);
    }

    sl_object *result_obj = NULL;
    mpz_t current_gcd, next_num_z;
    mpz_inits(current_gcd, next_num_z, NULL);

    sl_object *current_node = args;
    sl_gc_add_root(&current_node);  // Protect arg list traversal

    // Initialize with the absolute value of the first argument
    sl_object *first_arg = sl_car(current_node);
    if (!sl_is_number(first_arg) || !sl_number_is_integer(first_arg)) {
        mpz_clears(current_gcd, next_num_z, NULL);
        sl_gc_remove_root(&current_node);
        return sl_make_errorf("Error (gcd): Arguments must be integers.");
    }
    sl_number_get_z(first_arg, current_gcd);
    mpz_abs(current_gcd, current_gcd);  // gcd(a, b) = gcd(abs(a), abs(b))

    current_node = sl_cdr(current_node);  // Move to the second argument

    // Iterate through the rest of the arguments
    while (sl_is_pair(current_node)) {
        sl_object *next_arg = sl_car(current_node);
        if (!sl_is_number(next_arg) || !sl_number_is_integer(next_arg)) {
            mpz_clears(current_gcd, next_num_z, NULL);
            sl_gc_remove_root(&current_node);
            return sl_make_errorf("Error (gcd): Arguments must be integers.");
        }
        sl_number_get_z(next_arg, next_num_z);
        mpz_abs(next_num_z, next_num_z);  // Use absolute value

        mpz_gcd(current_gcd, current_gcd, next_num_z);  // Update gcd

        current_node = sl_cdr(current_node);
    }

    // Check for improper list
    if (current_node != SL_NIL) {
        mpz_clears(current_gcd, next_num_z, NULL);
        sl_gc_remove_root(&current_node);
        return sl_make_errorf("Error (gcd): Improper argument list.");
    }

    result_obj = sl_make_number_from_mpz(current_gcd);
    CHECK_ALLOC(result_obj);

    mpz_clears(current_gcd, next_num_z, NULL);
    sl_gc_remove_root(&current_node);
    return result_obj;
}

// (lcm n ...) -> Least Common Multiple of integers.
static sl_object *sl_builtin_lcm(sl_object *args) {
    // (lcm) -> 1
    if (args == SL_NIL) {
        return sl_make_number_si(1, 1);
    }

    sl_object *result_obj = NULL;
    mpz_t current_lcm, next_num_z;
    mpz_inits(current_lcm, next_num_z, NULL);

    sl_object *current_node = args;
    sl_gc_add_root(&current_node);  // Protect arg list traversal

    // Initialize with the absolute value of the first argument
    sl_object *first_arg = sl_car(current_node);
    if (!sl_is_number(first_arg) || !sl_number_is_integer(first_arg)) {
        mpz_clears(current_lcm, next_num_z, NULL);
        sl_gc_remove_root(&current_node);
        return sl_make_errorf("Error (lcm): Arguments must be integers.");
    }
    sl_number_get_z(first_arg, current_lcm);
    mpz_abs(current_lcm, current_lcm);  // lcm(a, b) = lcm(abs(a), abs(b))

    current_node = sl_cdr(current_node);  // Move to the second argument

    // Iterate through the rest of the arguments
    while (sl_is_pair(current_node)) {
        sl_object *next_arg = sl_car(current_node);
        if (!sl_is_number(next_arg) || !sl_number_is_integer(next_arg)) {
            mpz_clears(current_lcm, next_num_z, NULL);
            sl_gc_remove_root(&current_node);
            return sl_make_errorf("Error (lcm): Arguments must be integers.");
        }
        sl_number_get_z(next_arg, next_num_z);
        mpz_abs(next_num_z, next_num_z);  // Use absolute value

        // Handle lcm(a, 0) = 0
        if (mpz_sgn(current_lcm) == 0 || mpz_sgn(next_num_z) == 0) {
            mpz_set_ui(current_lcm, 0);
        } else {
            mpz_lcm(current_lcm, current_lcm, next_num_z);  // Update lcm
        }

        current_node = sl_cdr(current_node);
    }

    // Check for improper list
    if (current_node != SL_NIL) {
        mpz_clears(current_lcm, next_num_z, NULL);
        sl_gc_remove_root(&current_node);
        return sl_make_errorf("Error (lcm): Improper argument list.");
    }

    result_obj = sl_make_number_from_mpz(current_lcm);
    CHECK_ALLOC(result_obj);

    mpz_clears(current_lcm, next_num_z, NULL);
    sl_gc_remove_root(&current_node);
    return result_obj;
}

// (abs num) -> Returns the absolute value of a number.
static sl_object *sl_builtin_abs(sl_object *args) {
    sl_object *arity_check = check_arity("abs", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *num_obj = sl_car(args);
    if (!sl_is_number(num_obj)) {
        return sl_make_errorf("Error (abs): Argument must be a number.");
    }

    sl_object *result = NULL;
    mpq_t num_q, abs_q;
    // mpq_inits(num_q, abs_q, NULL);
    sl_gc_add_root(&num_obj);  // Protect input

    if (!get_number_as_mpq(num_obj, num_q, "abs")) {
        result = sl_make_errorf("Error (abs): Invalid number format.");  // Fallback
    } else {
        mpq_init(abs_q);                       // Initialize absolute value
        mpq_abs(abs_q, num_q);                 // Calculate absolute value
        result = make_number_from_mpq(abs_q);  // Create SymLisp number (uses static helper in sl_core.c)
        CHECK_ALLOC(result);
        mpq_clear(abs_q);  // Free the absolute value mpq_t
        mpq_clear(num_q);  // Free the original number mpq_t
    }

    sl_gc_remove_root(&num_obj);
    // mpq_clears(num_q, abs_q, NULL);
    return result;
}

// (max num ...) -> Returns the maximum of one or more numbers.
static sl_object *sl_builtin_max(sl_object *args) {
    if (args == SL_NIL) {
        return sl_make_errorf("Error (max): Requires at least one argument.");
    }

    sl_object *max_obj = sl_car(args);  // Initialize max with the first argument
    if (!sl_is_number(max_obj)) {
        return sl_make_errorf("Error (max): Arguments must be numbers.");
    }

    mpq_t max_q, current_q;
    // mpq_inits(max_q, current_q, NULL);
    sl_gc_add_root(&max_obj);  // Protect current max object

    // Get the first number as mpq
    if (!get_number_as_mpq(max_obj, max_q, "max")) {
        // mpq_clears(max_q, current_q, NULL);
        sl_gc_remove_root(&max_obj);
        return sl_make_errorf("Error (max): Invalid number format for first argument.");  // Fallback
    }

    sl_object *current_node = sl_cdr(args);
    sl_gc_add_root(&current_node);  // Protect arg list traversal

    while (sl_is_pair(current_node)) {
        sl_object *current_arg = sl_car(current_node);
        if (!sl_is_number(current_arg)) {
            // mpq_clears(max_q, current_q, NULL);
            mpq_clear(max_q);  // Clear max_q
            sl_gc_remove_root(&current_node);
            sl_gc_remove_root(&max_obj);
            return sl_make_errorf("Error (max): Arguments must be numbers.");
        }

        // Get current argument as mpq
        if (!get_number_as_mpq(current_arg, current_q, "max")) {
            // mpq_clears(max_q, current_q, NULL);
            mpq_clear(max_q);  // Clear max_q
            sl_gc_remove_root(&current_node);
            sl_gc_remove_root(&max_obj);
            return sl_make_errorf("Error (max): Invalid number format for argument.");  // Fallback
        }

        // Compare and update max if current is greater
        if (mpq_cmp(current_q, max_q) > 0) {
            // mpq_clear(max_q);             // Clear old max_q
            mpq_set(max_q, current_q);    // Update max_q
            sl_gc_remove_root(&max_obj);  // Unroot old max object
            max_obj = current_arg;        // Update max_obj pointer
            sl_gc_add_root(&max_obj);     // Root new max object
        }
        mpq_clear(current_q);  // Clear current_q

        current_node = sl_cdr(current_node);
    }

    // Check for improper list
    if (current_node != SL_NIL) {
        // mpq_clears(max_q, current_q, NULL);
        mpq_clear(max_q);  // Clear old max_q
        sl_gc_remove_root(&current_node);
        sl_gc_remove_root(&max_obj);
        return sl_make_errorf("Error (max): Improper argument list.");
    }

    // mpq_clears(max_q, current_q, NULL);
    mpq_clear(max_q);  // Clear old max_q
    sl_gc_remove_root(&current_node);
    sl_gc_remove_root(&max_obj);  // Unroot the final max object
    return max_obj;               // Return the object itself, not a copy
}

// (min num ...) -> Returns the minimum of one or more numbers.
static sl_object *sl_builtin_min(sl_object *args) {
    if (args == SL_NIL) {
        return sl_make_errorf("Error (min): Requires at least one argument.");
    }

    sl_object *min_obj = sl_car(args);  // Initialize min with the first argument
    if (!sl_is_number(min_obj)) {
        return sl_make_errorf("Error (min): Arguments must be numbers.");
    }

    mpq_t min_q, current_q;
    // mpq_inits(min_q, current_q, NULL);
    sl_gc_add_root(&min_obj);  // Protect current min object

    // Get the first number as mpq
    if (!get_number_as_mpq(min_obj, min_q, "min")) {
        // mpq_clears(min_q, current_q, NULL);
        sl_gc_remove_root(&min_obj);
        return sl_make_errorf("Error (min): Invalid number format for first argument.");  // Fallback
    }

    sl_object *current_node = sl_cdr(args);
    sl_gc_add_root(&current_node);  // Protect arg list traversal

    while (sl_is_pair(current_node)) {
        sl_object *current_arg = sl_car(current_node);
        if (!sl_is_number(current_arg)) {
            // mpq_clears(min_q, current_q, NULL);
            mpq_clear(min_q);  // Clear min_q
            sl_gc_remove_root(&current_node);
            sl_gc_remove_root(&min_obj);
            return sl_make_errorf("Error (min): Arguments must be numbers.");
        }

        // Get current argument as mpq
        if (!get_number_as_mpq(current_arg, current_q, "min")) {
            // mpq_clears(min_q, current_q, NULL);
            mpq_clear(min_q);  // Clear min_q
            sl_gc_remove_root(&current_node);
            sl_gc_remove_root(&min_obj);
            return sl_make_errorf("Error (min): Invalid number format for argument.");  // Fallback
        }

        // Compare and update min if current is smaller
        if (mpq_cmp(current_q, min_q) < 0) {
            mpq_set(min_q, current_q);    // Update min_q
            sl_gc_remove_root(&min_obj);  // Unroot old min object
            min_obj = current_arg;        // Update min_obj pointer
            sl_gc_add_root(&min_obj);     // Root new min object
        }
        mpq_clear(current_q);  // Clear current_q

        current_node = sl_cdr(current_node);
    }

    // Check for improper list
    if (current_node != SL_NIL) {
        mpq_clear(min_q);  // Clear min_q
        // mpq_clears(min_q, current_q, NULL);
        sl_gc_remove_root(&current_node);
        sl_gc_remove_root(&min_obj);
        return sl_make_errorf("Error (min): Improper argument list.");
    }

    mpq_clear(min_q);  // Clear min_q
    // mpq_clears(min_q, current_q, NULL);
    sl_gc_remove_root(&current_node);
    sl_gc_remove_root(&min_obj);  // Unroot the final min object
    return min_obj;               // Return the object itself, not a copy
}

// (expt base exponent) -> Calculates base^exponent. Handles integer exponents.
static sl_object *sl_builtin_expt(sl_object *args) {
    sl_object *arity_check = check_arity("expt", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *base_obj = sl_car(args);
    sl_object *exp_obj = sl_cadr(args);

    if (!sl_is_number(base_obj)) {
        return sl_make_errorf("Error (expt): Base must be a number.");
    }
    if (!sl_is_number(exp_obj)) {
        return sl_make_errorf("Error (expt): Exponent must be a number.");
    }

    // For now, only handle integer exponents
    if (!sl_number_is_integer(exp_obj)) {
        return sl_make_errorf("Error (expt): Exponent must be an integer (fractional exponents not implemented).");
    }

    sl_object *result_obj = NULL;
    mpq_t base_q;
    mpz_t exp_z, base_num_z, base_den_z, result_num_z, result_den_z;
    unsigned long exp_ui;
    bool exp_is_neg = false;

    // Initialize GMP variables
    mpz_init(exp_z);
    mpz_init(base_num_z);
    mpz_init(base_den_z);
    mpz_init(result_num_z);
    mpz_init(result_den_z);
    // mpq_init(base_q);  // Init in get_number_as_mpq

    sl_gc_add_root(&base_obj);  // Protect base_obj during potential allocations
    sl_gc_add_root(&exp_obj);   // Protect exp_obj

    // Get exponent as mpz_t
    sl_number_get_z(exp_obj, exp_z);  // We know it's an integer

    // Check exponent sign and magnitude for mpz_pow_ui
    if (mpz_sgn(exp_z) < 0) {
        exp_is_neg = true;
        mpz_neg(exp_z, exp_z);  // Use absolute value for pow_ui
    }

    // mpz_pow_ui requires unsigned long exponent
    if (!mpz_fits_ulong_p(exp_z)) {
        result_obj = sl_make_errorf("Error (expt): Exponent magnitude too large.");
        goto cleanup_expt;
    }
    exp_ui = mpz_get_ui(exp_z);
    // mpz_clear(exp_z); // Keep exp_z for 0^0 check if needed

    // Get base as mpq_t
    if (!get_number_as_mpq(base_obj, base_q, "expt")) {
        result_obj = sl_make_errorf("Error (expt): Invalid base number format.");  // Fallback
        goto cleanup_expt;
    }

    // Handle base^0 = 1 (including 0^0)
    if (exp_ui == 0 && !exp_is_neg) {
        result_obj = sl_make_number_si(1, 1);
        goto cleanup_expt;
    }
    // Handle 0^positive_exp = 0
    if (mpq_sgn(base_q) == 0 && exp_ui > 0 && !exp_is_neg) {
        result_obj = sl_make_number_si(0, 1);
        goto cleanup_expt;
    }
    // Handle 0^negative_exp -> Division by zero
    if (mpq_sgn(base_q) == 0 && exp_is_neg) {
        result_obj = sl_make_errorf("Error (expt): 0 cannot be raised to a negative power.");
        goto cleanup_expt;
    }

    // Get numerator and denominator of the base
    mpq_get_num(base_num_z, base_q);
    mpq_get_den(base_den_z, base_q);

    // Calculate numerator^|exponent|
    mpz_pow_ui(result_num_z, base_num_z, exp_ui);

    // Calculate denominator^|exponent|
    mpz_pow_ui(result_den_z, base_den_z, exp_ui);

    // Construct the result rational number object
    if (exp_is_neg) {
        // If exponent was negative, result is den^|exp| / num^|exp|
        result_obj = sl_make_number_zz(result_den_z, result_num_z);
    } else {
        // If exponent was positive, result is num^|exp| / den^|exp|
        result_obj = sl_make_number_zz(result_num_z, result_den_z);
    }
    CHECK_ALLOC(result_obj);  // Check allocation of the final number object

cleanup_expt:
    // Clear all GMP variables
    mpz_clear(exp_z);
    mpz_clear(base_num_z);
    mpz_clear(base_den_z);
    mpz_clear(result_num_z);
    mpz_clear(result_den_z);
    mpq_clear(base_q);

    // Unroot objects
    sl_gc_remove_root(&exp_obj);
    sl_gc_remove_root(&base_obj);
    return result_obj;
}

// (square num) -> Calculates num*num
static sl_object *sl_builtin_square(sl_object *args) {
    sl_object *arity_check = check_arity("square", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *num_obj = sl_car(args);
    if (!sl_is_number(num_obj)) {
        return sl_make_errorf("Error (square): Argument must be a number.");
    }

    sl_object *result_obj = NULL;
    mpq_t num_q, result_q;
    sl_gc_add_root(&num_obj);  // Protect input

    if (!get_number_as_mpq(num_obj, num_q, "square")) {
        result_obj = sl_make_errorf("Error (square): Invalid number format.");  // Fallback
    } else {
        mpq_init(result_q);
        mpq_mul(result_q, num_q, num_q);  // result = num * num
        result_obj = make_number_from_mpq(result_q);
        CHECK_ALLOC(result_obj);
        mpq_clear(result_q);
        mpq_clear(num_q);
    }

    sl_gc_remove_root(&num_obj);
    return result_obj;
}

// (exact-integer-sqrt n) -> Returns (list s r) where s*s + r = n
static sl_object *sl_builtin_exact_integer_sqrt(sl_object *args) {
    sl_object *arity_check = check_arity("exact-integer-sqrt", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *n_obj = sl_car(args);
    if (!sl_is_number(n_obj) || !sl_number_is_integer(n_obj)) {
        return sl_make_errorf("Error (exact-integer-sqrt): Argument must be an integer.");
    }

    sl_object *result_obj = NULL;
    mpz_t n_z, s_z, r_z;
    mpz_inits(n_z, s_z, r_z, NULL);
    sl_gc_add_root(&n_obj);  // Protect input

    sl_number_get_z(n_obj, n_z);

    if (mpz_sgn(n_z) < 0) {
        result_obj = sl_make_errorf("Error (exact-integer-sqrt): Argument must be non-negative.");
    } else {
        mpz_sqrtrem(s_z, r_z, n_z);  // Calculate s and r

        sl_object *s_obj = sl_make_number_from_mpz(s_z);
        CHECK_ALLOC(s_obj);
        sl_gc_add_root(&s_obj);  // Protect s_obj

        sl_object *r_obj = sl_make_number_from_mpz(r_z);
        CHECK_ALLOC(r_obj);
        sl_gc_add_root(&r_obj);  // Protect r_obj

        // Build the result list (s r)
        sl_object *r_list = sl_make_pair(r_obj, SL_NIL);
        CHECK_ALLOC(r_list);
        sl_gc_add_root(&r_list);  // Protect r_list

        result_obj = sl_make_pair(s_obj, r_list);
        CHECK_ALLOC(result_obj);

        // Cleanup roots
        sl_gc_remove_root(&r_list);
        sl_gc_remove_root(&r_obj);
        sl_gc_remove_root(&s_obj);
    }

    mpz_clears(n_z, s_z, r_z, NULL);
    sl_gc_remove_root(&n_obj);
    return result_obj;
}

// --- Comparison Builtins ---

// (= num1 num2) - Numeric equality
static sl_object *sl_builtin_num_eq(sl_object *args) {
    sl_object *arity_check = check_arity("=", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t num1_q, num2_q;
    // get_number_as_mpq initializes the mpq_t vars if successful
    if (!get_number_as_mpq(sl_car(args), num1_q, "=")) {
        // Error: first arg not number. num1_q not initialized.
        return sl_make_errorf("Error (=): First argument is not a number.");
    }
    if (!get_number_as_mpq(sl_car(sl_cdr(args)), num2_q, "=")) {
        // Error: second arg not number. num2_q not initialized.
        mpq_clear(num1_q);  // Clear the first one which was initialized.
        return sl_make_errorf("Error (=): Second argument is not a number.");
    }

    // Both numbers successfully retrieved and mpq_t initialized
    int cmp_result = mpq_cmp(num1_q, num2_q);

    mpq_clears(num1_q, num2_q, NULL);  // Clear both temporaries

    return (cmp_result == 0) ? SL_TRUE : SL_FALSE;
}

// (> num1 num2) - Greater than
static sl_object *sl_builtin_gt(sl_object *args) {
    sl_object *arity_check = check_arity(">", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t num1_q, num2_q;
    if (!get_number_as_mpq(sl_car(args), num1_q, ">")) {
        return sl_make_errorf("Error (>): First argument is not a number.");
    }
    if (!get_number_as_mpq(sl_car(sl_cdr(args)), num2_q, ">")) {
        mpq_clear(num1_q);
        return sl_make_errorf("Error (>): Second argument is not a number.");
    }

    int cmp_result = mpq_cmp(num1_q, num2_q);
    mpq_clears(num1_q, num2_q, NULL);

    return (cmp_result > 0) ? SL_TRUE : SL_FALSE;
}

// (< num1 num2) - Less than
static sl_object *sl_builtin_lt(sl_object *args) {
    sl_object *arity_check = check_arity("<", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t num1_q, num2_q;
    if (!get_number_as_mpq(sl_car(args), num1_q, "<")) {
        return sl_make_errorf("Error (<): First argument is not a number.");
    }
    if (!get_number_as_mpq(sl_car(sl_cdr(args)), num2_q, "<")) {
        mpq_clear(num1_q);
        return sl_make_errorf("Error (<): Second argument is not a number.");
    }

    int cmp_result = mpq_cmp(num1_q, num2_q);
    mpq_clears(num1_q, num2_q, NULL);

    return (cmp_result < 0) ? SL_TRUE : SL_FALSE;
}

// (>= num1 num2) - Greater than or equal to
static sl_object *sl_builtin_ge(sl_object *args) {
    sl_object *arity_check = check_arity(">=", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t num1_q, num2_q;
    if (!get_number_as_mpq(sl_car(args), num1_q, ">=")) {
        return sl_make_errorf("Error (>=): First argument is not a number.");
    }
    if (!get_number_as_mpq(sl_car(sl_cdr(args)), num2_q, ">=")) {
        mpq_clear(num1_q);
        return sl_make_errorf("Error (>=): Second argument is not a number.");
    }

    int cmp_result = mpq_cmp(num1_q, num2_q);
    mpq_clears(num1_q, num2_q, NULL);

    return (cmp_result >= 0) ? SL_TRUE : SL_FALSE;
}

// (<= num1 num2) - Less than or equal to
static sl_object *sl_builtin_le(sl_object *args) {
    sl_object *arity_check = check_arity("<=", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    mpq_t num1_q, num2_q;
    if (!get_number_as_mpq(sl_car(args), num1_q, "<=")) {
        return sl_make_errorf("Error (<=): First argument is not a number.");
    }
    if (!get_number_as_mpq(sl_car(sl_cdr(args)), num2_q, "<=")) {
        mpq_clear(num1_q);
        return sl_make_errorf("Error (<=): Second argument is not a number.");
    }

    int cmp_result = mpq_cmp(num1_q, num2_q);
    mpq_clears(num1_q, num2_q, NULL);

    return (cmp_result <= 0) ? SL_TRUE : SL_FALSE;
}

// (not obj) -> Returns #t if obj is #f, otherwise returns #f.
static sl_object *sl_builtin_not(sl_object *args) {
    sl_object *arity_check = check_arity("not", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *obj = sl_car(args);

    // In Scheme, only #f is false. Everything else is true.
    // So, 'not' returns #t only if the input is exactly #f.
    return (obj == SL_FALSE) ? SL_TRUE : SL_FALSE;
}

// (display obj)
static sl_object *sl_builtin_display(sl_object *args) {
    sl_object *arity_check = check_arity("display", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *obj_to_display = sl_car(args);  // Get the first argument

    char *str_repr = NULL;  // Initialize to NULL

    // Special handling for string objects to print contents without quotes
    if (sl_is_string(obj_to_display)) {
        // Use the accessor macro from sl_core.h
        fputs(sl_string_value(obj_to_display), stdout);
    } else {
        // For non-strings, get the standard representation using the function
        // declared in sl_core.h
        str_repr = sl_object_to_string(obj_to_display);
        if (!str_repr) {
            // sl_object_to_string returns NULL on failure according to sl_core.h comment
            return sl_make_errorf("display: Failed to convert object to string (allocation failed?)");
        }
        fputs(str_repr, stdout);
        // Free the string allocated by sl_object_to_string
        // Note: The comment in sl_core.h says caller must free().
        free(str_repr);
    }
    fflush(stdout);  // Ensure output is flushed

    // R7RS specifies display returns an unspecified value. Use SL_NIL.
    return SL_NIL;
}

// (list obj ...) -> Creates a new list containing the objects.
static sl_object *sl_builtin_list(sl_object *args) {
    // No arity check needed, accepts any number of arguments.
    // The input 'args' is already the list of arguments.
    // We just need to return it, as sl_eval_list already evaluated them.
    // However, the standard 'list' function *constructs* a new list
    // from its arguments, it doesn't just return the argument list structure.
    // Let's implement it correctly by copying the argument list structure.

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    sl_object *current_arg = args;

    sl_gc_add_root(&head);         // Protect the list being built
    sl_gc_add_root(&current_arg);  // Protect the argument list traversal

    while (sl_is_pair(current_arg)) {
        sl_object *arg_val = sl_car(current_arg);  // Get the already evaluated argument
        sl_object *new_pair = sl_make_pair(arg_val, SL_NIL);
        CHECK_ALLOC(new_pair);  // Check for OOM

        *tail_ptr = new_pair;
        tail_ptr = &new_pair->data.pair.cdr;

        current_arg = sl_cdr(current_arg);
    }

    // Check if the argument list itself was improper
    if (current_arg != SL_NIL) {
        sl_gc_remove_root(&current_arg);
        sl_gc_remove_root(&head);
        return sl_make_errorf("Error (list): Improper argument list provided to list constructor.");
    }

    sl_gc_remove_root(&current_arg);
    sl_gc_remove_root(&head);
    return head;  // Return the newly constructed list
}

// (length list) -> Returns the number of elements in a proper list.
static sl_object *sl_builtin_length(sl_object *args) {
    sl_object *arity_check = check_arity("length", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *list = sl_car(args);
    size_t count = 0;
    sl_object *current = list;

    // Need to protect 'list' in case GC runs during error creation below
    sl_gc_add_root(&list);

    while (sl_is_pair(current)) {
        count++;
        current = sl_cdr(current);
    }

    if (current != SL_NIL) {  // Check if it was a proper list
        sl_gc_remove_root(&list);
        return sl_make_errorf("Error (length): Argument must be a proper list.");
    }

    sl_gc_remove_root(&list);

    // Convert size_t count to an sl_object number
    // For simplicity, assume count fits in int64_t for now.
    // A robust implementation might use mpz_set_ui if count is large.
    if (count > INT64_MAX) {
        return sl_make_errorf("Error (length): List length exceeds maximum representable integer.");
    }

    return sl_make_number_si((int64_t)count, 1);
}

// (reverse list) -> Returns a new list with elements in reverse order.
static sl_object *sl_builtin_reverse(sl_object *args) {
    sl_object *arity_check = check_arity("reverse", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *input_list = sl_car(args);
    sl_object *reversed_list = SL_NIL;
    sl_object *current = input_list;

    // Protect input and result during iteration/allocation
    sl_gc_add_root(&input_list);
    sl_gc_add_root(&reversed_list);

    while (sl_is_pair(current)) {
        sl_object *element = sl_car(current);
        // cons the element onto the front of the reversed list
        reversed_list = sl_make_pair(element, reversed_list);
        CHECK_ALLOC(reversed_list);  // Check allocation, return OOM error if needed

        current = sl_cdr(current);
    }

    if (current != SL_NIL) {  // Check if input was a proper list
        sl_gc_remove_root(&reversed_list);
        sl_gc_remove_root(&input_list);
        return sl_make_errorf("Error (reverse): Argument must be a proper list.");
    }

    sl_gc_remove_root(&reversed_list);
    sl_gc_remove_root(&input_list);
    return reversed_list;
}

// (append list ...) -> Concatenates lists. Last arg becomes tail.
static sl_object *sl_builtin_append(sl_object *args) {
    // No specific arity check, 0 args is valid (returns NIL)

    if (args == SL_NIL) {
        return SL_NIL;  // (append) -> '()
    }

    sl_object *result_head = SL_NIL;
    sl_object **result_tail_ptr = &result_head;  // Pointer to where the next pair should be linked
    sl_object *current_arg_node = args;

    // Protect the list being built and the argument list traversal
    sl_gc_add_root(&result_head);
    sl_gc_add_root(&current_arg_node);

    while (sl_is_pair(current_arg_node)) {
        sl_object *current_list_arg = sl_car(current_arg_node);
        sl_object *next_arg_node = sl_cdr(current_arg_node);

        // If this is the LAST argument node, we handle it differently
        if (next_arg_node == SL_NIL) {
            // Append the last argument directly to the tail
            *result_tail_ptr = current_list_arg;
            break;  // Done processing arguments
        }

        // --- Process a list argument (not the last one) ---
        sl_object *current_element_node = current_list_arg;
        sl_gc_add_root(&current_element_node);  // Protect inner loop traversal

        while (sl_is_pair(current_element_node)) {
            sl_object *element = sl_car(current_element_node);
            sl_object *new_pair = sl_make_pair(element, SL_NIL);  // cdr will be overwritten or is last
            CHECK_ALLOC(new_pair);

            *result_tail_ptr = new_pair;                 // Link the new pair into the result list
            result_tail_ptr = &new_pair->data.pair.cdr;  // Advance the tail pointer

            current_element_node = sl_cdr(current_element_node);
        }

        // Check if the current list argument was proper
        if (current_element_node != SL_NIL) {
            sl_gc_remove_root(&current_element_node);
            sl_gc_remove_root(&current_arg_node);
            sl_gc_remove_root(&result_head);
            return sl_make_errorf("Error (append): Argument before last must be a proper list.");
        }
        sl_gc_remove_root(&current_element_node);
        // --- End processing list argument ---

        current_arg_node = next_arg_node;  // Move to the next argument in the main list
    }

    // Check if the main argument list itself was improper (shouldn't happen if called correctly)
    if (current_arg_node != SL_NIL && !sl_is_pair(current_arg_node)) {
        sl_gc_remove_root(&current_arg_node);
        sl_gc_remove_root(&result_head);
        return sl_make_errorf("Error (append): Internal error - improper argument list structure.");
    }

    sl_gc_remove_root(&current_arg_node);
    sl_gc_remove_root(&result_head);
    return result_head;
}

// (set-car! pair obj) -> Modifies pair, sets car to obj. Returns unspecified.
static sl_object *sl_builtin_set_car(sl_object *args) {
    sl_object *arity_check = check_arity("set-car!", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);
    sl_object *new_val = sl_cadr(args);  // Helper for sl_car(sl_cdr(args))

    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (set-car!): First argument must be a pair, got type %d.", pair ? pair->type : -1);
    }

    // Use the macro from sl_core.h to modify the pair
    sl_set_car(pair, new_val);

    // Return unspecified value (NIL is conventional)
    return SL_NIL;
}

// (set-cdr! pair obj) -> Modifies pair, sets cdr to obj. Returns unspecified.
static sl_object *sl_builtin_set_cdr(sl_object *args) {
    sl_object *arity_check = check_arity("set-cdr!", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);
    sl_object *new_val = sl_cadr(args);

    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (set-cdr!): First argument must be a pair, got type %d.", pair ? pair->type : -1);
    }

    // Use the macro from sl_core.h to modify the pair
    sl_set_cdr(pair, new_val);

    // Return unspecified value (NIL is conventional)
    return SL_NIL;
}

// (eq? obj1 obj2) -> Checks for pointer equality (identity).
static sl_object *sl_builtin_eq(sl_object *args) {
    sl_object *arity_check = check_arity("eq?", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *obj1 = sl_car(args);
    sl_object *obj2 = sl_cadr(args);  // Helper for sl_car(sl_cdr(args))

    // Direct pointer comparison
    return (obj1 == obj2) ? SL_TRUE : SL_FALSE;
}

// Forward declaration for recursive equal? helper
static bool sl_equal_recursive(sl_object *obj1, sl_object *obj2);

// (equal? obj1 obj2) -> Checks for structural equality.
static sl_object *sl_builtin_equal(sl_object *args) {
    sl_object *arity_check = check_arity("equal?", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *obj1 = sl_car(args);
    sl_object *obj2 = sl_cadr(args);

    // Call the recursive helper
    return sl_equal_recursive(obj1, obj2) ? SL_TRUE : SL_FALSE;
}

// Recursive helper for equal?
// NOTE: This implementation does NOT handle circular structures.
static bool sl_equal_recursive(sl_object *obj1, sl_object *obj2) {
    // 1. Check for identity (eq?) first - covers NIL, booleans, identical objects
    if (obj1 == obj2) {
        return true;
    }

    // 2. If not identical, check if types are different
    if (!obj1 || !obj2 || obj1->type != obj2->type) {
        return false;
    }

    // 3. Types are the same, compare based on type
    switch (obj1->type) {
    case SL_TYPE_NUMBER: {
        // Compare numerically
        mpq_t num1_q, num2_q;
        bool ok1 = get_number_as_mpq(obj1, num1_q, "equal?");  // Initializes num1_q
        bool ok2 = get_number_as_mpq(obj2, num2_q, "equal?");  // Initializes num2_q
        // If conversion fails (shouldn't happen if sl_is_number passed), treat as unequal
        if (!ok1 || !ok2) {
            if (ok1) mpq_clear(num1_q);
            if (ok2) mpq_clear(num2_q);
            return false;
        }
        int cmp_result = mpq_cmp(num1_q, num2_q);
        mpq_clears(num1_q, num2_q, NULL);
        return (cmp_result == 0);
    }
    case SL_TYPE_STRING:
        // Compare string contents
        // Assumes sl_string_value returns a null-terminated C string
        return (strcmp(sl_string_value(obj1), sl_string_value(obj2)) == 0);
    case SL_TYPE_SYMBOL:
        // Compare symbol names (since interning is not yet implemented)
        // Once interning is done, this case can just return true (because if they
        // weren't eq? they wouldn't be the same symbol).
        return (strcmp(sl_symbol_name(obj1), sl_symbol_name(obj2)) == 0);
    case SL_TYPE_PAIR:
        // Recursively compare car and cdr
        // Rooting is not strictly necessary here as we are only reading
        // and not allocating within the recursive calls themselves.
        return sl_equal_recursive(sl_car(obj1), sl_car(obj2)) &&
               sl_equal_recursive(sl_cdr(obj1), sl_cdr(obj2));

    // Other types are only equal if they are eq?, which was checked first.
    case SL_TYPE_NIL:       // Handled by eq? check
    case SL_TYPE_BOOLEAN:   // Handled by eq? check
    case SL_TYPE_FUNCTION:  // Not equal unless eq?
    case SL_TYPE_ENV:       // Not equal unless eq?
    case SL_TYPE_ERROR:     // Not equal unless eq?
    case SL_TYPE_FREE:      // Should not be encountered
    default:
        return false;
    }
}

// (newline)
static sl_object *sl_builtin_newline(sl_object *args) {
    sl_object *arity_check = check_arity("newline", args, 0);
    if (arity_check != SL_TRUE) return arity_check;

    putchar('\n');
    fflush(stdout);  // Ensure it appears immediately

    // R7RS specifies newline returns an unspecified value. Use SL_NIL.
    return SL_NIL;
}

// (load filename-string)
static sl_object *sl_builtin_load(sl_object *args) {
    sl_object *arity_check = check_arity("load", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *filename_obj = sl_car(args);
    if (!sl_is_string(filename_obj)) {
        return sl_make_errorf("Error (load): Argument must be a string filename.");
    }

    const char *filename = sl_string_value(filename_obj);
    FILE *file = fopen(filename, "r");

    if (!file) {
        // Could use strerror(errno) for a more specific error
        return sl_make_errorf("Error (load): Could not open file '%s' (%s).", filename, strerror(errno));
    }

    // Use sl_eval_stream to evaluate the file content in the *current* global environment
    // Note: sl_eval_stream handles reading and evaluating all expressions.
    // It uses the environment passed to it. Since builtins are called via sl_apply,
    // which gets the environment from the closure/builtin object, we should use
    // the global environment here, assuming 'load' operates at the top level.
    // If 'load' could be called from within a local scope and affect that scope,
    // sl_apply would need to pass the correct 'env' down here.
    // For now, assume it loads into the global environment.
    sl_object *result = sl_eval_stream(file, sl_global_env);

    fclose(file);

    // sl_eval_stream returns the result of the last expression or an error.
    return result;
}

// --- Builtin Initialization ---

// Helper to define a builtin function in an environment
static void define_builtin(sl_object *env, const char *name, sl_builtin_func_ptr func_ptr) {
    // --- FIX: Root env temporarily ---
    sl_gc_add_root(&env);  // Protect env during allocations below

    sl_object *sym = sl_make_symbol(name);
    if (!sym || sym == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "FATAL: Failed to create symbol for builtin '%s'\n", name);
        sl_gc_remove_root(&env);  // Unroot env before potentially exiting
        // Consider exiting or handling more gracefully
        exit(EXIT_FAILURE);
    }
    // --- FIX: Root sym ---
    sl_gc_add_root(&sym);

    sl_object *func = sl_make_builtin(name, func_ptr);
    if (!func || func == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "FATAL: Failed to create builtin object for '%s'\n", name);
        // --- FIX: Unroot sym and env ---
        sl_gc_remove_root(&sym);
        sl_gc_remove_root(&env);
        // Consider exiting
        exit(EXIT_FAILURE);
    }
    // --- FIX: Root func ---
    sl_gc_add_root(&func);

    // Define the symbol-function pair in the environment
    // sl_env_define now handles its internal rooting correctly,
    // but we needed to root sym and func before calling it.
    sl_env_define(env, sym, func);

    // --- FIX: Unroot temporary variables ---
    sl_gc_remove_root(&func);
    sl_gc_remove_root(&sym);
    sl_gc_remove_root(&env);  // Unroot env
}

void sl_builtins_init(sl_object *global_env) {
    // Core functions
    define_builtin(global_env, "car", sl_builtin_car);
    define_builtin(global_env, "cdr", sl_builtin_cdr);
    define_builtin(global_env, "cons", sl_builtin_cons);
    define_builtin(global_env, "list", sl_builtin_list);         // <<< ADDED
    define_builtin(global_env, "length", sl_builtin_length);     // <<< ADDED
    define_builtin(global_env, "reverse", sl_builtin_reverse);   // <<< ADDED
    define_builtin(global_env, "append", sl_builtin_append);     // <<< ADDED
    define_builtin(global_env, "set-car!", sl_builtin_set_car);  // <<< ADDED
    define_builtin(global_env, "set-cdr!", sl_builtin_set_cdr);  // <<< ADDED

    // Arithmetic
    define_builtin(global_env, "+", sl_builtin_add);
    define_builtin(global_env, "-", sl_builtin_sub);
    define_builtin(global_env, "*", sl_builtin_mul);
    define_builtin(global_env, "/", sl_builtin_div);
    define_builtin(global_env, "remainder", sl_builtin_remainder);                    // <<< ADDED
    define_builtin(global_env, "modulo", sl_builtin_modulo);                          // <<< ADDED
    define_builtin(global_env, "denominator", sl_builtin_denominator);                // <<< ADDED
    define_builtin(global_env, "numerator", sl_builtin_numerator);                    // <<< ADDED
    define_builtin(global_env, "quotient", sl_builtin_quotient);                      // <<< ADDED
    define_builtin(global_env, "gcd", sl_builtin_gcd);                                // <<< ADDED
    define_builtin(global_env, "lcm", sl_builtin_lcm);                                // <<< ADDED
    define_builtin(global_env, "abs", sl_builtin_abs);                                // <<< ADDED
    define_builtin(global_env, "max", sl_builtin_max);                                // <<< ADDED
    define_builtin(global_env, "min", sl_builtin_min);                                // <<< ADDED
    define_builtin(global_env, "expt", sl_builtin_expt);                              // <<< ADDED
    define_builtin(global_env, "square", sl_builtin_square);                          // <<< ADDED
    define_builtin(global_env, "exact-integer-sqrt", sl_builtin_exact_integer_sqrt);  // <<< ADDED

    // Comparison
    define_builtin(global_env, "=", sl_builtin_num_eq);
    define_builtin(global_env, ">", sl_builtin_gt);
    define_builtin(global_env, "<", sl_builtin_lt);
    define_builtin(global_env, ">=", sl_builtin_ge);
    define_builtin(global_env, "<=", sl_builtin_le);
    define_builtin(global_env, "eq?", sl_builtin_eq);        // <<< ADDED
    define_builtin(global_env, "equal?", sl_builtin_equal);  // <<< ADDED
    define_builtin(global_env, "not", sl_builtin_not);       // <<< ADDED

    // Basic I/O
    define_builtin(global_env, "display", sl_builtin_display);
    define_builtin(global_env, "newline", sl_builtin_newline);
    define_builtin(global_env, "load", sl_builtin_load);

    // Add other builtins here...
}