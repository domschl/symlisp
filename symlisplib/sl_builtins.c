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
#include "sl_predicates.h"
#include "sl_strings.h"
#include "sl_higher_order.h"

// Helper to check arity.
// Returns SL_TRUE if arity matches and list is proper.
// Returns an SL_TYPE_ERROR object otherwise.
sl_object *check_arity(const char *func_name, sl_object *args, size_t expected) {
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

sl_object *check_arity_range(const char *func_name, sl_object *args, size_t min_expected, size_t max_expected) {
    size_t count = 0;
    sl_object *current = args;
    while (sl_is_pair(current)) {
        count++;
        current = sl_cdr(current);
    }
    if (current != SL_NIL) {  // Improper list
        return sl_make_errorf("Error (%s): Improper argument list provided.", func_name);
    }
    if (count < min_expected || count > max_expected) {
        if (min_expected == max_expected) {
            return sl_make_errorf("Error (%s): Expected %zu arguments, got %zu.", func_name, min_expected, count);
        } else {
            return sl_make_errorf("Error (%s): Expected between %zu and %zu arguments, got %zu.", func_name, min_expected, max_expected, count);
        }
    }
    return SL_TRUE;  // Arity is correct
}

// Helper to get a number object's value as int64_t if it's an integer and fits.
// Returns true on success, false on failure (non-number, non-integer, out of range, or error).
// Prints an error message to stderr on failure.
bool get_number_as_int64(sl_object *obj, int64_t *out, const char *func_name) {
    if (!sl_is_number(obj)) {
        fprintf(stderr, "Error (%s): Expected a number, got %s.\n", func_name, sl_type_name(obj ? obj->type : -1));
        return false;
    }
    if (!sl_number_is_integer(obj)) {
        // TODO: Print the actual non-integer value?
        fprintf(stderr, "Error (%s): Expected an integer, but got a non-integer number.\n", func_name);
        return false;
    }

    // Use a temporary mpz_t to handle both small and big integers uniformly
    mpz_t temp_z;
    mpz_init(temp_z);

    // sl_number_get_z handles both small and big nums internally
    sl_number_get_z(obj, temp_z);

    // Check if the integer value fits within int64_t range
    // Using mpz_fits_slong_p assuming long is at least 64 bits,
    // or mpz_fits_sint_p if int is 64 bits. A more explicit check might be needed
    // depending on platform guarantees, but slong is often 64 bits on 64-bit systems.
    // Let's assume mpz_fits_slong_p is sufficient for int64_t for now.
    if (!mpz_fits_slong_p(temp_z)) {
        // Value is too large or too small for int64_t
        gmp_fprintf(stderr, "Error (%s): Integer value %Zd is out of range for a 64-bit signed integer.\n", func_name, temp_z);
        mpz_clear(temp_z);
        return false;
    }

    // It fits, get the value
    *out = mpz_get_si(temp_z);  // mpz_get_si returns long int
    mpz_clear(temp_z);
    return true;
}

// Helper to get a number object's value as mpq_t
// Initializes 'out' - caller must clear 'out'
bool get_number_as_mpq(sl_object *obj, mpq_t out, const char *func_name) {
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

static sl_object *sl_builtin_caar(sl_object *args) {
    sl_object *arity_check = check_arity("caar", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (caar): Argument must be a pair, got type %d.", pair ? pair->type : -1);
    }
    return sl_caar(pair);
}

static sl_object *sl_builtin_cadr(sl_object *args) {
    sl_object *arity_check = check_arity("cadr", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (cadr): Argument must be a pair, got type %d.", pair ? pair->type : -1);
    }
    return sl_cadr(pair);
}

static sl_object *sl_builtin_cddr(sl_object *args) {
    sl_object *arity_check = check_arity("cddr", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (cddr): Argument must be a pair, got type %d.", pair ? pair->type : -1);
    }
    return sl_cddr(pair);
}

static sl_object *sl_builtin_caddr(sl_object *args) {
    sl_object *arity_check = check_arity("caddr", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *pair = sl_car(args);  // Get the first argument
    if (!sl_is_pair(pair)) {
        return sl_make_errorf("Error (caddr): Argument must be a pair, got type %d.", pair ? pair->type : -1);
    }
    return sl_caddr(pair);
}

static sl_object *sl_builtin_cons(sl_object *args) {
    sl_object *arity_check = check_arity("cons", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *car_val = sl_car(args);          // First argument
    sl_object *cdr_val = sl_car(sl_cdr(args));  // Second argument

    return sl_make_pair(car_val, cdr_val);
}

// Helper function to reverse a list (used by cons*)
// Assumes input is already validated as a proper list if necessary
// Returns the reversed list or an error object
static sl_object *reverse_list_internal(sl_object *list) {
    sl_object *reversed = SL_NIL;
    sl_object *current = list;
    sl_object *temp_item = SL_NIL;

    SL_GC_ADD_ROOT(&list);  // Root input list
    SL_GC_ADD_ROOT(&reversed);
    SL_GC_ADD_ROOT(&current);
    SL_GC_ADD_ROOT(&temp_item);

    while (sl_is_pair(current)) {
        temp_item = sl_car(current);
        sl_object *new_pair = sl_make_pair(temp_item, reversed);
        if (!new_pair) {
            reversed = SL_OUT_OF_MEMORY_ERROR;  // OOM
            goto cleanup_reverse;
        }
        // No need to root new_pair explicitly if reversed is rooted
        reversed = new_pair;
        current = sl_cdr(current);
    }

    // Check if the original list was proper
    if (current != SL_NIL) {
        reversed = sl_make_errorf("Internal reverse: Input was not a proper list");
        // Fall through to cleanup
    }

cleanup_reverse:
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&current);
    SL_GC_REMOVE_ROOT(&reversed);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&list);
    return reversed;
}

// (cons* obj1 obj2 ...) -> (cons obj1 (cons obj2 (... (cons objN-1 objN) ...)))
// Requires at least one argument. If one arg, returns it.
static sl_object *sl_builtin_cons_star(sl_object *args) {
    sl_object *arity_check = check_arity_min("cons*", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *result = SL_NIL;
    sl_object *reversed_args = SL_NIL;
    sl_object *iter = SL_NIL;
    sl_object *item = SL_NIL;

    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&result);
    SL_GC_ADD_ROOT(&reversed_args);
    SL_GC_ADD_ROOT(&iter);
    SL_GC_ADD_ROOT(&item);

    // Handle 1-argument case
    if (sl_cdr(args) == SL_NIL) {
        result = sl_car(args);
        goto cleanup_cons_star;
    }

    // Reverse the argument list to process from last to second
    reversed_args = reverse_list_internal(args);
    if (reversed_args == SL_OUT_OF_MEMORY_ERROR || sl_is_error(reversed_args)) {
        result = reversed_args;  // Propagate error
        goto cleanup_cons_star;
    }
    // reversed_args is now rooted

    // Initialize result with the first element of reversed list (last original arg)
    if (!sl_is_pair(reversed_args)) {  // Should not happen if arity >= 2
        result = sl_make_errorf("cons*: Internal error after reversing arguments");
        goto cleanup_cons_star;
    }
    result = sl_car(reversed_args);
    iter = sl_cdr(reversed_args);

    // Iterate through the rest of the reversed args (original args N-1 down to 1)
    while (sl_is_pair(iter)) {
        item = sl_car(iter);
        sl_object *new_pair = sl_make_pair(item, result);
        if (!new_pair) {
            result = SL_OUT_OF_MEMORY_ERROR;
            goto cleanup_cons_star;
        }
        result = new_pair;  // Update result (implicitly rooted via result root)
        iter = sl_cdr(iter);
    }
    // No need to check iter != SL_NIL, reverse_list_internal ensures proper list

cleanup_cons_star:
    SL_GC_REMOVE_ROOT(&item);
    SL_GC_REMOVE_ROOT(&iter);
    SL_GC_REMOVE_ROOT(&reversed_args);
    SL_GC_REMOVE_ROOT(&result);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&args);
    return result;
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
        mpq_clear(temp);  // Clear temp after use
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
        // mpq_clear(result_q);
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
                // mpq_clear(result_q);
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

    SL_GC_ADD_ROOT(&num_obj);  // Protect input during potential error allocation

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

    SL_GC_REMOVE_ROOT(&num_obj);
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

    SL_GC_ADD_ROOT(&num_obj);  // Protect input

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

    SL_GC_REMOVE_ROOT(&num_obj);
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
    SL_GC_ADD_ROOT(&n1_obj);  // Protect inputs
    SL_GC_ADD_ROOT(&n2_obj);

    // Extract mpz values (we know they are integers)
    sl_number_get_z(n1_obj, n1_z);
    sl_number_get_z(n2_obj, n2_z);

    // Perform truncated division (tdiv)
    mpz_tdiv_q(quotient_z, n1_z, n2_z);

    result = sl_make_number_from_mpz(quotient_z);
    CHECK_ALLOC(result);

    SL_GC_REMOVE_ROOT(&n2_obj);
    SL_GC_REMOVE_ROOT(&n1_obj);
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
    SL_GC_ADD_ROOT(&current_node);  // Protect arg list traversal

    // Initialize with the absolute value of the first argument
    sl_object *first_arg = sl_car(current_node);
    if (!sl_is_number(first_arg) || !sl_number_is_integer(first_arg)) {
        mpz_clears(current_gcd, next_num_z, NULL);
        SL_GC_REMOVE_ROOT(&current_node);
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
            SL_GC_REMOVE_ROOT(&current_node);
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
        SL_GC_REMOVE_ROOT(&current_node);
        return sl_make_errorf("Error (gcd): Improper argument list.");
    }

    result_obj = sl_make_number_from_mpz(current_gcd);
    CHECK_ALLOC(result_obj);

    mpz_clears(current_gcd, next_num_z, NULL);
    SL_GC_REMOVE_ROOT(&current_node);
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
    SL_GC_ADD_ROOT(&current_node);  // Protect arg list traversal

    // Initialize with the absolute value of the first argument
    sl_object *first_arg = sl_car(current_node);
    if (!sl_is_number(first_arg) || !sl_number_is_integer(first_arg)) {
        mpz_clears(current_lcm, next_num_z, NULL);
        SL_GC_REMOVE_ROOT(&current_node);
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
            SL_GC_REMOVE_ROOT(&current_node);
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
        SL_GC_REMOVE_ROOT(&current_node);
        return sl_make_errorf("Error (lcm): Improper argument list.");
    }

    result_obj = sl_make_number_from_mpz(current_lcm);
    CHECK_ALLOC(result_obj);

    mpz_clears(current_lcm, next_num_z, NULL);
    SL_GC_REMOVE_ROOT(&current_node);
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
    SL_GC_ADD_ROOT(&num_obj);  // Protect input

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

    SL_GC_REMOVE_ROOT(&num_obj);
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
    SL_GC_ADD_ROOT(&max_obj);  // Protect current max object

    // Get the first number as mpq
    if (!get_number_as_mpq(max_obj, max_q, "max")) {
        // mpq_clears(max_q, current_q, NULL);
        SL_GC_REMOVE_ROOT(&max_obj);
        return sl_make_errorf("Error (max): Invalid number format for first argument.");  // Fallback
    }

    sl_object *current_node = sl_cdr(args);
    SL_GC_ADD_ROOT(&current_node);  // Protect arg list traversal

    while (sl_is_pair(current_node)) {
        sl_object *current_arg = sl_car(current_node);
        if (!sl_is_number(current_arg)) {
            // mpq_clears(max_q, current_q, NULL);
            mpq_clear(max_q);  // Clear max_q
            SL_GC_REMOVE_ROOT(&current_node);
            SL_GC_REMOVE_ROOT(&max_obj);
            return sl_make_errorf("Error (max): Arguments must be numbers.");
        }

        // Get current argument as mpq
        if (!get_number_as_mpq(current_arg, current_q, "max")) {
            // mpq_clears(max_q, current_q, NULL);
            mpq_clear(max_q);  // Clear max_q
            SL_GC_REMOVE_ROOT(&current_node);
            SL_GC_REMOVE_ROOT(&max_obj);
            return sl_make_errorf("Error (max): Invalid number format for argument.");  // Fallback
        }

        // Compare and update max if current is greater
        if (mpq_cmp(current_q, max_q) > 0) {
            // mpq_clear(max_q);             // Clear old max_q
            mpq_set(max_q, current_q);    // Update max_q
            SL_GC_REMOVE_ROOT(&max_obj);  // Unroot old max object
            max_obj = current_arg;        // Update max_obj pointer
            SL_GC_ADD_ROOT(&max_obj);     // Root new max object
        }
        mpq_clear(current_q);  // Clear current_q

        current_node = sl_cdr(current_node);
    }

    // Check for improper list
    if (current_node != SL_NIL) {
        // mpq_clears(max_q, current_q, NULL);
        mpq_clear(max_q);  // Clear old max_q
        SL_GC_REMOVE_ROOT(&current_node);
        SL_GC_REMOVE_ROOT(&max_obj);
        return sl_make_errorf("Error (max): Improper argument list.");
    }

    // mpq_clears(max_q, current_q, NULL);
    mpq_clear(max_q);  // Clear old max_q
    SL_GC_REMOVE_ROOT(&current_node);
    SL_GC_REMOVE_ROOT(&max_obj);  // Unroot the final max object
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
    SL_GC_ADD_ROOT(&min_obj);  // Protect current min object

    // Get the first number as mpq
    if (!get_number_as_mpq(min_obj, min_q, "min")) {
        // mpq_clears(min_q, current_q, NULL);
        SL_GC_REMOVE_ROOT(&min_obj);
        return sl_make_errorf("Error (min): Invalid number format for first argument.");  // Fallback
    }

    sl_object *current_node = sl_cdr(args);
    SL_GC_ADD_ROOT(&current_node);  // Protect arg list traversal

    while (sl_is_pair(current_node)) {
        sl_object *current_arg = sl_car(current_node);
        if (!sl_is_number(current_arg)) {
            // mpq_clears(min_q, current_q, NULL);
            mpq_clear(min_q);  // Clear min_q
            SL_GC_REMOVE_ROOT(&current_node);
            SL_GC_REMOVE_ROOT(&min_obj);
            return sl_make_errorf("Error (min): Arguments must be numbers.");
        }

        // Get current argument as mpq
        if (!get_number_as_mpq(current_arg, current_q, "min")) {
            // mpq_clears(min_q, current_q, NULL);
            mpq_clear(min_q);  // Clear min_q
            SL_GC_REMOVE_ROOT(&current_node);
            SL_GC_REMOVE_ROOT(&min_obj);
            return sl_make_errorf("Error (min): Invalid number format for argument.");  // Fallback
        }

        // Compare and update min if current is smaller
        if (mpq_cmp(current_q, min_q) < 0) {
            mpq_set(min_q, current_q);    // Update min_q
            SL_GC_REMOVE_ROOT(&min_obj);  // Unroot old min object
            min_obj = current_arg;        // Update min_obj pointer
            SL_GC_ADD_ROOT(&min_obj);     // Root new min object
        }
        mpq_clear(current_q);  // Clear current_q

        current_node = sl_cdr(current_node);
    }

    // Check for improper list
    if (current_node != SL_NIL) {
        mpq_clear(min_q);  // Clear min_q
        // mpq_clears(min_q, current_q, NULL);
        SL_GC_REMOVE_ROOT(&current_node);
        SL_GC_REMOVE_ROOT(&min_obj);
        return sl_make_errorf("Error (min): Improper argument list.");
    }

    mpq_clear(min_q);  // Clear min_q
    // mpq_clears(min_q, current_q, NULL);
    SL_GC_REMOVE_ROOT(&current_node);
    SL_GC_REMOVE_ROOT(&min_obj);  // Unroot the final min object
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

    SL_GC_ADD_ROOT(&base_obj);  // Protect base_obj during potential allocations
    SL_GC_ADD_ROOT(&exp_obj);   // Protect exp_obj

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
    SL_GC_REMOVE_ROOT(&exp_obj);
    SL_GC_REMOVE_ROOT(&base_obj);
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
    SL_GC_ADD_ROOT(&num_obj);  // Protect input

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

    SL_GC_REMOVE_ROOT(&num_obj);
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
    SL_GC_ADD_ROOT(&n_obj);  // Protect input

    sl_number_get_z(n_obj, n_z);

    if (mpz_sgn(n_z) < 0) {
        result_obj = sl_make_errorf("Error (exact-integer-sqrt): Argument must be non-negative.");
    } else {
        mpz_sqrtrem(s_z, r_z, n_z);  // Calculate s and r

        sl_object *s_obj = sl_make_number_from_mpz(s_z);
        CHECK_ALLOC(s_obj);
        SL_GC_ADD_ROOT(&s_obj);  // Protect s_obj

        sl_object *r_obj = sl_make_number_from_mpz(r_z);
        CHECK_ALLOC(r_obj);
        SL_GC_ADD_ROOT(&r_obj);  // Protect r_obj

        // Build the result list (s r)
        sl_object *r_list = sl_make_pair(r_obj, SL_NIL);
        CHECK_ALLOC(r_list);
        SL_GC_ADD_ROOT(&r_list);  // Protect r_list

        result_obj = sl_make_pair(s_obj, r_list);
        CHECK_ALLOC(result_obj);

        // Cleanup roots
        SL_GC_REMOVE_ROOT(&r_list);
        SL_GC_REMOVE_ROOT(&r_obj);
        SL_GC_REMOVE_ROOT(&s_obj);
    }

    mpz_clears(n_z, s_z, r_z, NULL);
    SL_GC_REMOVE_ROOT(&n_obj);
    return result_obj;
}

// Helper function to add a factor (as mpz_t) to the list being built
static bool add_factor_z(sl_object **head_root, sl_object **tail_root, mpz_t factor_z) {
    sl_object *factor_obj = sl_make_number_from_mpz(factor_z);              // Creates number object
    if (!factor_obj || factor_obj == SL_OUT_OF_MEMORY_ERROR) return false;  // Allocation failed

    SL_GC_ADD_ROOT(&factor_obj);  // Protect the new factor object
    // append_to_list returns NULL on error (OOM or internal)
    bool success = (append_to_list(head_root, tail_root, factor_obj) != NULL);
    SL_GC_REMOVE_ROOT(&factor_obj);  // Unroot (now reachable via list roots)
    return success;
}

// Helper function to add a factor (as unsigned long) to the list being built
static bool add_factor_ui(sl_object **head_root, sl_object **tail_root, unsigned long factor_ui) {
    sl_object *factor_obj = sl_make_number_si((int64_t)factor_ui, 1);       // Creates number object
    if (!factor_obj || factor_obj == SL_OUT_OF_MEMORY_ERROR) return false;  // Allocation failed

    SL_GC_ADD_ROOT(&factor_obj);  // Protect the new factor object
    // append_to_list returns NULL on error (OOM or internal)
    bool success = (append_to_list(head_root, tail_root, factor_obj) != NULL);
    SL_GC_REMOVE_ROOT(&factor_obj);  // Unroot (now reachable via list roots)
    return success;
}

// (prime-factors n) -> Returns a list of prime factors of integer n
static sl_object *sl_builtin_prime_factors(sl_object *args) {
    sl_object *arity_check = check_arity("prime-factors", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *n_obj = sl_car(args);
    sl_object *result_list = SL_NIL;
    sl_object *tail_node = SL_NIL;  // Tracks the last node for append_to_list

    mpz_t n_z, factor_z, limit_z;  // Use mpz_t for the number and potential large factors/limit
    mpz_inits(n_z, factor_z, limit_z, NULL);

    SL_GC_ADD_ROOT(&n_obj);  // Protect input
    SL_GC_ADD_ROOT(&result_list);
    SL_GC_ADD_ROOT(&tail_node);

    // 1. Get integer value and check type
    if (!get_number_as_mpz(n_obj, n_z, "prime-factors")) {
        // Error: not an integer (get_number_as_mpz checks this)
        result_list = sl_make_errorf("prime-factors: Argument must be an integer.");
        goto cleanup_prime_factors;
    }

    // 2. Handle sign and edge cases (0, 1, -1)
    mpz_abs(n_z, n_z);              // Work with absolute value
    if (mpz_cmp_ui(n_z, 1) <= 0) {  // If n is 0 or 1
        result_list = SL_NIL;       // Return empty list
        goto cleanup_prime_factors;
    }

    // 3. Trial division by 2
    while (mpz_even_p(n_z)) {
        if (!add_factor_ui(&result_list, &tail_node, 2)) goto oom_prime_factors;
        mpz_divexact_ui(n_z, n_z, 2);  // n = n / 2
    }

    // 4. Trial division by odd numbers starting from 3
    // Calculate limit = floor(sqrt(n))
    mpz_sqrt(limit_z, n_z);
    mpz_set_ui(factor_z, 3);  // Start checking with factor 3

    while (mpz_cmp(factor_z, limit_z) <= 0) {  // While factor <= sqrt(n)
        if (mpz_divisible_p(n_z, factor_z)) {  // Check divisibility
            if (!add_factor_z(&result_list, &tail_node, factor_z)) goto oom_prime_factors;
            mpz_divexact(n_z, n_z, factor_z);  // n = n / factor
            mpz_sqrt(limit_z, n_z);            // Recalculate sqrt(n) as n has changed
        } else {
            mpz_add_ui(factor_z, factor_z, 2);  // Go to the next odd number
        }
    }

    // 5. If n is still > 1, the remaining n is prime
    if (mpz_cmp_ui(n_z, 1) > 0) {
        if (!add_factor_z(&result_list, &tail_node, n_z)) goto oom_prime_factors;
    }

    goto cleanup_prime_factors;  // Success path

oom_prime_factors:
    result_list = SL_OUT_OF_MEMORY_ERROR;  // Signal OOM

cleanup_prime_factors:
    mpz_clears(n_z, factor_z, limit_z, NULL);
    SL_GC_REMOVE_ROOT(&tail_node);
    SL_GC_REMOVE_ROOT(&result_list);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&n_obj);
    return result_list;
}

// (next-prime n) -> Returns the smallest prime number strictly greater than integer n
static sl_object *sl_builtin_next_prime(sl_object *args) {
    sl_object *arity_check = check_arity("next-prime", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *n_obj = sl_car(args);
    sl_object *result_obj = NULL;  // Initialize to NULL

    mpz_t n_z, result_z;
    mpz_inits(n_z, result_z, NULL);

    SL_GC_ADD_ROOT(&n_obj);  // Protect input

    // 1. Get integer value and check type
    if (!get_number_as_mpz(n_obj, n_z, "next-prime")) {
        // Error: not an integer (get_number_as_mpz checks this)
        result_obj = sl_make_errorf("next-prime: Argument must be an integer.");
        goto cleanup_next_prime;
    }

    // 2. Find the next prime using GMP
    mpz_nextprime(result_z, n_z);

    // 3. Convert result back to Scheme object
    result_obj = sl_make_number_from_mpz(result_z);
    if (!result_obj || result_obj == SL_OUT_OF_MEMORY_ERROR) {
        result_obj = SL_OUT_OF_MEMORY_ERROR;  // Ensure error propagation
    }

cleanup_next_prime:
    mpz_clears(n_z, result_z, NULL);
    SL_GC_REMOVE_ROOT(&n_obj);
    // result_obj is the intended return value, no need to root/unroot here
    return result_obj;
}

#define DEFAULT_FLOAT_PRECISION 10  // Default decimal places if not specified

// (float num [precision]) -> Returns string representation of num to precision decimal places.
static sl_object *sl_builtin_float(sl_object *args) {
    // Arity check: 1 or 2 arguments
    size_t arg_count = 0;
    sl_object *current = args;
    while (sl_is_pair(current)) {
        arg_count++;
        current = sl_cdr(current);
    }
    if (current != SL_NIL || (arg_count != 1 && arg_count != 2)) {
        return sl_make_errorf("Error (float): Expected 1 or 2 arguments, got %zu in %s list.",
                              arg_count, (current == SL_NIL ? "a proper" : "an improper"));
    }

    sl_object *num_obj = sl_car(args);
    sl_object *prec_obj = (arg_count == 2) ? sl_cadr(args) : NULL;
    long precision = DEFAULT_FLOAT_PRECISION;  // Use long for precision value

    if (!sl_is_number(num_obj)) {
        return sl_make_errorf("Error (float): First argument must be a number.");
    }

    // Validate precision argument if provided
    if (prec_obj) {
        if (!sl_is_number(prec_obj) || !sl_number_is_integer(prec_obj)) {
            return sl_make_errorf("Error (float): Precision argument must be an integer.");
        }
        // Get precision value, check range
        mpz_t prec_z;
        mpz_init(prec_z);
        sl_number_get_z(prec_obj, prec_z);  // We know it's an integer
        if (!mpz_fits_slong_p(prec_z) || mpz_sgn(prec_z) < 0) {
            mpz_clear(prec_z);
            return sl_make_errorf("Error (float): Precision must be a non-negative integer fitting in a long.");
        }
        precision = mpz_get_si(prec_z);
        mpz_clear(prec_z);
    }

    // --- Perform Conversion ---
    sl_object *result_obj = NULL;
    mpq_t num_q;
    mpz_t num_z, den_z, scale_factor, scaled_num, scaled_result, scaled_rem, two_rem;
    bool is_neg = false;

    // Initialize GMP variables
    // mpq_init(num_q);  // Init in get_number_as_mpq!
    mpz_inits(num_z, den_z, scale_factor, scaled_num, scaled_result, scaled_rem, two_rem, NULL);

    SL_GC_ADD_ROOT(&num_obj);                 // Protect input number
    if (prec_obj) SL_GC_ADD_ROOT(&prec_obj);  // Protect precision if it exists

    // Get the number as mpq_t
    if (!get_number_as_mpq(num_obj, num_q, "float")) {
        result_obj = sl_make_errorf("Error (float): Invalid number format.");  // Fallback
        goto cleanup_float;
    }

    // Handle sign
    if (mpq_sgn(num_q) < 0) {
        is_neg = true;
        mpq_abs(num_q, num_q);  // Work with absolute value
    }

    // Get absolute numerator and denominator
    mpq_get_num(num_z, num_q);
    mpq_get_den(den_z, num_q);

    // Calculate scale factor = 10^precision
    mpz_ui_pow_ui(scale_factor, 10, precision);

    // Calculate scaled numerator = num_z * scale_factor
    mpz_mul(scaled_num, num_z, scale_factor);

    // Perform division with remainder: scaled_num / den_z
    mpz_tdiv_qr(scaled_result, scaled_rem, scaled_num, den_z);

    // Rounding: Add 1 to scaled_result if 2 * remainder >= denominator
    mpz_mul_ui(two_rem, scaled_rem, 2);
    if (mpz_cmp(two_rem, den_z) >= 0) {
        mpz_add_ui(scaled_result, scaled_result, 1);
    }

    // Convert the scaled integer result to string
    // Add 2 extra bytes: 1 for potential sign, 1 for null terminator
    // Add 'precision' extra bytes: for potential "0." prefix and padding
    size_t result_str_len_needed = mpz_sizeinbase(scaled_result, 10) + 2 + precision;
    char *result_str = malloc(result_str_len_needed);
    if (!result_str) {
        result_obj = SL_OUT_OF_MEMORY_ERROR;
        goto cleanup_float;
    }
    mpz_get_str(result_str, 10, scaled_result);

    size_t num_digits = strlen(result_str);
    size_t final_str_len = 0;
    char *final_str = NULL;

    // Allocate final string buffer (consider sign, decimal point, padding)
    // Max length: sign + integer_digits + '.' + precision_digits + null
    final_str_len = (is_neg ? 1 : 0) + num_digits + (precision > 0 ? 1 : 0) + 1;
    // If result is small (e.g., 0.001 with precision 3 -> scaled "1"), need padding
    if (precision > 0 && num_digits <= precision) {
        final_str_len += (precision - num_digits + 1);  // Need space for "0." and leading zeros
    }
    final_str = malloc(final_str_len);
    if (!final_str) {
        free(result_str);
        result_obj = SL_OUT_OF_MEMORY_ERROR;
        goto cleanup_float;
    }

    // Format the final string with decimal point and sign
    char *p = final_str;
    if (is_neg) {
        *p++ = '-';
    }

    if (precision == 0) {
        // No decimal part, just copy the integer string
        strcpy(p, result_str);
    } else if (num_digits > precision) {
        // Integer part exists
        size_t int_part_len = num_digits - precision;
        strncpy(p, result_str, int_part_len);
        p += int_part_len;
        *p++ = '.';
        strcpy(p, result_str + int_part_len);
    } else {
        // No integer part (or integer part is 0)
        *p++ = '0';
        *p++ = '.';
        // Add leading zeros if needed
        for (size_t i = 0; i < (precision - num_digits); ++i) {
            *p++ = '0';
        }
        strcpy(p, result_str);
    }

    result_obj = sl_make_string(final_str);  // Creates string object (copies final_str)
    CHECK_ALLOC(result_obj);

    // Free intermediate strings
    free(result_str);
    free(final_str);

cleanup_float:
    // Clear all GMP variables
    mpq_clear(num_q);
    mpz_clears(num_z, den_z, scale_factor, scaled_num, scaled_result, scaled_rem, two_rem, NULL);

    // Unroot objects
    SL_GC_REMOVE_ROOT(&num_obj);
    if (prec_obj) SL_GC_REMOVE_ROOT(&prec_obj);
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

    SL_GC_ADD_ROOT(&head);         // Protect the list being built
    SL_GC_ADD_ROOT(&current_arg);  // Protect the argument list traversal

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
        SL_GC_REMOVE_ROOT(&current_arg);
        SL_GC_REMOVE_ROOT(&head);
        return sl_make_errorf("Error (list): Improper argument list provided to list constructor.");
    }

    SL_GC_REMOVE_ROOT(&current_arg);
    SL_GC_REMOVE_ROOT(&head);
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
    SL_GC_ADD_ROOT(&list);

    while (sl_is_pair(current)) {
        count++;
        current = sl_cdr(current);
    }

    if (current != SL_NIL) {  // Check if it was a proper list
        SL_GC_REMOVE_ROOT(&list);
        return sl_make_errorf("Error (length): Argument must be a proper list.");
    }

    SL_GC_REMOVE_ROOT(&list);

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
    SL_GC_ADD_ROOT(&input_list);
    SL_GC_ADD_ROOT(&reversed_list);

    while (sl_is_pair(current)) {
        sl_object *element = sl_car(current);
        // cons the element onto the front of the reversed list
        reversed_list = sl_make_pair(element, reversed_list);
        CHECK_ALLOC(reversed_list);  // Check allocation, return OOM error if needed

        current = sl_cdr(current);
    }

    if (current != SL_NIL) {  // Check if input was a proper list
        SL_GC_REMOVE_ROOT(&reversed_list);
        SL_GC_REMOVE_ROOT(&input_list);
        return sl_make_errorf("Error (reverse): Argument must be a proper list.");
    }

    SL_GC_REMOVE_ROOT(&reversed_list);
    SL_GC_REMOVE_ROOT(&input_list);
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
    SL_GC_ADD_ROOT(&result_head);
    SL_GC_ADD_ROOT(&current_arg_node);

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
        SL_GC_ADD_ROOT(&current_element_node);  // Protect inner loop traversal

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
            SL_GC_REMOVE_ROOT(&current_element_node);
            SL_GC_REMOVE_ROOT(&current_arg_node);
            SL_GC_REMOVE_ROOT(&result_head);
            return sl_make_errorf("Error (append): Argument before last must be a proper list.");
        }
        SL_GC_REMOVE_ROOT(&current_element_node);
        // --- End processing list argument ---

        current_arg_node = next_arg_node;  // Move to the next argument in the main list
    }

    // Check if the main argument list itself was improper (shouldn't happen if called correctly)
    if (current_arg_node != SL_NIL && !sl_is_pair(current_arg_node)) {
        SL_GC_REMOVE_ROOT(&current_arg_node);
        SL_GC_REMOVE_ROOT(&result_head);
        return sl_make_errorf("Error (append): Internal error - improper argument list structure.");
    }

    SL_GC_REMOVE_ROOT(&current_arg_node);
    SL_GC_REMOVE_ROOT(&result_head);
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
    case SL_TYPE_CHAR:  // <<< ADDED
        return obj1->data.code_point == obj2->data.code_point;
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

// (write obj) -> Writes obj's external representation to stdout.
static sl_object *sl_builtin_write(sl_object *args) {
    sl_object *arity_check = check_arity("write", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *obj_to_write = sl_car(args);
    SL_GC_ADD_ROOT(&obj_to_write);  // Protect input

    char *str_repr = sl_object_to_string(obj_to_write);
    if (!str_repr) {
        SL_GC_REMOVE_ROOT(&obj_to_write);
        // sl_object_to_string returns NULL on failure (e.g., OOM)
        return sl_make_errorf("write: Failed to convert object to string (allocation failed?)");
    }

    fputs(str_repr, stdout);
    free(str_repr);  // Free the allocated string
    fflush(stdout);  // Ensure output is flushed

    SL_GC_REMOVE_ROOT(&obj_to_write);
    // R7RS specifies write returns an unspecified value. Use SL_NIL.
    return SL_NIL;
}

// (read) -> Reads one S-expression from stdin.
static sl_object *sl_builtin_read(sl_object *args) {
    sl_object *arity_check = check_arity("read", args, 0);
    if (arity_check != SL_TRUE) return arity_check;

    // Call the stream parser directly on stdin
    sl_object *result = sl_parse_stream(stdin);

    // sl_parse_stream returns SL_EOF_OBJECT on EOF,
    // SL_PARSE_ERROR on syntax errors,
    // SL_OUT_OF_MEMORY_ERROR on allocation failure,
    // or the parsed object on success.
    if (result == SL_PARSE_ERROR) {
        // Error message should have been printed by the parser
        // Return a generic error or propagate SL_PARSE_ERROR?
        // Let's return a new error object for now.
        return sl_make_errorf("read: Failed to parse S-expression from input.");
    }
    // Propagate EOF, OOM, or return the valid object
    return result;
}

// (eval expr env) -> Evaluates expr in the context of env.
static sl_object *sl_builtin_eval(sl_object *args) {
    sl_object *arity_check = check_arity("eval", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *expr = sl_car(args);
    sl_object *env_obj = sl_cadr(args);

    // Check if the second argument is actually an environment
    if (!sl_is_env(env_obj)) {
        return sl_make_errorf("eval: Second argument must be an environment, got %s.", sl_type_name(env_obj ? env_obj->type : -1));
    }

    // Root arguments and evaluate
    SL_GC_ADD_ROOT(&expr);
    SL_GC_ADD_ROOT(&env_obj);

    sl_object *result = sl_eval(expr, env_obj);  // Call the core evaluator

    // Result is already managed by sl_eval's rooting, just unroot args
    SL_GC_REMOVE_ROOT(&env_obj);
    SL_GC_REMOVE_ROOT(&expr);

    return result;  // Return the result of evaluation (could be value or error)
}

// (interaction-environment) -> Returns the global REPL environment. R5RS/R7RS.
static sl_object *sl_builtin_interaction_environment(sl_object *args) {
    sl_object *arity_check = check_arity("interaction-environment", args, 0);
    if (arity_check != SL_TRUE) return arity_check;
    // Simply return the global environment (needs to be accessible)
    if (!sl_global_env) {
        // This should ideally not happen if initialized correctly
        return sl_make_errorf("interaction-environment: Global environment not initialized.");
    }
    return sl_global_env;
}

// (environment . bindings) -> Creates a new environment
// Simplistic version: (environment) -> creates empty child of interaction-environment.
static sl_object *sl_builtin_environment(sl_object *args) {
    // For now, just create an empty environment whose parent is the global env.
    sl_object *arity_check = check_arity("environment", args, 0);  // Simplest version: 0 args
    if (arity_check != SL_TRUE) return arity_check;

    if (!sl_global_env) {
        return sl_make_errorf("environment: Global environment not initialized (cannot create child).");
    }
    sl_object *new_env = sl_env_create(sl_global_env);  // Parent is global env
    CHECK_ALLOC(new_env);
    return new_env;
}

// --- Builtin Initialization ---

// Helper to define a builtin function in an environment
void define_builtin(sl_object *env, const char *name, sl_builtin_func_ptr func_ptr) {
    // --- FIX: Root env temporarily ---
    SL_GC_ADD_ROOT(&env);  // Protect env during allocations below

    sl_object *sym = sl_make_symbol(name);
    if (!sym || sym == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "FATAL: Failed to create symbol for builtin '%s'\n", name);
        SL_GC_REMOVE_ROOT(&env);  // Unroot env before potentially exiting
        // Consider exiting or handling more gracefully
        exit(EXIT_FAILURE);
    }
    // --- FIX: Root sym ---
    SL_GC_ADD_ROOT(&sym);

    sl_object *func = sl_make_builtin(name, func_ptr);
    if (!func || func == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "FATAL: Failed to create builtin object for '%s'\n", name);
        // --- FIX: Unroot sym and env ---
        SL_GC_REMOVE_ROOT(&sym);
        SL_GC_REMOVE_ROOT(&env);
        // Consider exiting
        exit(EXIT_FAILURE);
    }
    // --- FIX: Root func ---
    SL_GC_ADD_ROOT(&func);

    // Define the symbol-function pair in the environment
    // sl_env_define now handles its internal rooting correctly,
    // but we needed to root sym and func before calling it.
    sl_env_define(env, sym, func);

    // --- FIX: Unroot temporary variables ---
    SL_GC_REMOVE_ROOT(&func);
    SL_GC_REMOVE_ROOT(&sym);
    SL_GC_REMOVE_ROOT(&env);  // Unroot env
}

// Helper to append an item to a list being built (handles GC rooting)
// Returns the new head on success, or NULL on OOM error or internal error.
// Takes pointers to the head and tail roots.
sl_object *append_to_list_bad(sl_object **head_root, sl_object **tail_root, sl_object *item) {
    // Item is assumed to be rooted by the caller if necessary before calling append_to_list
    sl_object *new_pair = sl_make_pair(item, SL_NIL);
    if (!new_pair) return NULL;  // OOM

    SL_GC_ADD_ROOT(&new_pair);  // Root the new pair

    if (*head_root == SL_NIL) {
        *head_root = new_pair;
        *tail_root = new_pair;
    } else {
        // Ensure tail_root points to a valid pair before setting cdr
        if (!sl_is_pair(*tail_root)) {
            SL_GC_REMOVE_ROOT(&new_pair);
            return NULL;  // Internal error
        }
        // TODO: Ensure sl_set_cdr exists and is GC-safe if needed
        sl_set_cdr(*tail_root, new_pair);
        *tail_root = new_pair;
    }
    SL_GC_REMOVE_ROOT(&new_pair);  // Unroot new_pair (now reachable from head/tail roots)
    return *head_root;
}

// Helper to append an item to a list being built (handles GC rooting)
// Returns the new head on success, or NULL on OOM error or internal error.
// Takes pointers to the head root variable and the tail node root variable.
// NOTE: The second argument's meaning has changed! It's now the address
//       of the variable holding the pointer to the current tail *node*.
sl_object *append_to_list(sl_object **head_root_var, sl_object **tail_node_root_var, sl_object *item) {
    // Item is assumed to be rooted by the caller if necessary before calling append_to_list
    sl_object *new_node = sl_make_pair(item, SL_NIL);  // This is the new list node (pair)
    if (!new_node) return NULL;                        // OOM

    SL_GC_ADD_ROOT(&new_node);  // Root the new node

    if (*head_root_var == SL_NIL) {
        // List was empty, new_node is both head and tail
        *head_root_var = new_node;
        *tail_node_root_var = new_node;  // The tail node is the new node
    } else {
        // Append to existing list
        sl_object *current_tail_node = *tail_node_root_var;
        // Ensure tail_node_root_var pointed to a valid pair before setting cdr
        if (!sl_is_pair(current_tail_node)) {
            fprintf(stderr, "[append_to_list] Internal Error: tail_node_root_var does not point to a pair.\n");
            SL_GC_REMOVE_ROOT(&new_node);
            return NULL;  // Internal error
        }
        // Modify the cdr of the *current* last node
        sl_set_cdr(current_tail_node, new_node);

        // Update the tail_node_root_var variable itself to point to the new last node.
        *tail_node_root_var = new_node;
    }
    SL_GC_REMOVE_ROOT(&new_node);  // Unroot new_node (now reachable from head/tail roots)
    return *head_root_var;         // Return the head of the list
}

// --- Control Primitives ---

sl_object *sl_builtin_apply(sl_object *args) {
    sl_object *proc = SL_NIL;
    sl_object *final_args_head = SL_NIL;  // Head of the final list to pass to proc
    sl_object *final_args_tail = SL_NIL;  // Tail for efficient appending
    sl_object *current_arg_node = SL_NIL;
    sl_object *last_arg_node = SL_NIL;
    sl_object *last_arg_val = SL_NIL;
    sl_object *result = SL_NIL;

    // Root everything that needs protection across allocations/calls
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&proc);
    SL_GC_ADD_ROOT(&final_args_head);
    SL_GC_ADD_ROOT(&final_args_tail);
    SL_GC_ADD_ROOT(&current_arg_node);
    SL_GC_ADD_ROOT(&last_arg_node);
    SL_GC_ADD_ROOT(&last_arg_val);
    SL_GC_ADD_ROOT(&result);

    // 1. Check arity (at least 2 args: proc and list)
    if (!sl_is_pair(args) || sl_cdr(args) == SL_NIL) {
        result = sl_make_errorf("apply: Expected at least 2 arguments (function and list)");
        goto cleanup_apply_builtin;
    }

    // 2. Extract proc
    proc = sl_car(args);
    if (!sl_is_function(proc)) {
        result = sl_make_errorf("apply: First argument must be a function");
        goto cleanup_apply_builtin;
    }

    // 3. Find the last argument node in the list passed to 'apply'
    last_arg_node = args;
    while (sl_is_pair(sl_cdr(last_arg_node))) {
        last_arg_node = sl_cdr(last_arg_node);
    }
    // Now last_arg_node points to the pair containing the last argument for 'apply'

    // 4. Extract and validate the last argument (must be a proper list)
    last_arg_val = sl_car(last_arg_node);
    if (!sl_is_list(last_arg_val)) {  // Checks for proper list (ends in NIL)
        result = sl_make_errorf("apply: Last argument must be a proper list");
        goto cleanup_apply_builtin;
    }

    // 5. Construct the final argument list (final_args_head) to pass to 'proc'
    final_args_head = SL_NIL;  // Start empty
    final_args_tail = SL_NIL;

    // 5a. Add intermediate arguments (arg1 ... argN-1 from 'apply' call)
    current_arg_node = sl_cdr(args);  // Start from the node containing arg1
    while (current_arg_node != last_arg_node) {
        if (!sl_is_pair(current_arg_node)) {  // Should not happen if args is proper list
            result = sl_make_errorf("apply: Internal error - unexpected non-pair in arguments");
            goto cleanup_apply_builtin;
        }
        sl_object *item = sl_car(current_arg_node);
        SL_GC_ADD_ROOT(&item);  // Root item before passing to helper
        if (append_to_list(&final_args_head, &final_args_tail, item) == NULL) {
            SL_GC_REMOVE_ROOT(&item);
            result = sl_make_errorf("apply: Failed to build argument list (OOM or internal error)");
            goto cleanup_apply_builtin;
        }
        SL_GC_REMOVE_ROOT(&item);  // Unroot item (now part of final_args_head)
        current_arg_node = sl_cdr(current_arg_node);
    }

    // 5b. Append elements from the last argument list (last_arg_val)
    current_arg_node = last_arg_val;  // Iterate through the list itself
    while (current_arg_node != SL_NIL) {
        if (!sl_is_pair(current_arg_node)) {  // Should not happen if last_arg_val is proper list
            result = sl_make_errorf("apply: Internal error - unexpected non-pair in last argument list");
            goto cleanup_apply_builtin;
        }
        sl_object *item = sl_car(current_arg_node);
        SL_GC_ADD_ROOT(&item);  // Root item before passing to helper
        if (append_to_list(&final_args_head, &final_args_tail, item) == NULL) {
            SL_GC_REMOVE_ROOT(&item);
            result = sl_make_errorf("apply: Failed to build argument list (OOM or internal error)");
            goto cleanup_apply_builtin;
        }
        SL_GC_REMOVE_ROOT(&item);  // Unroot item (now part of final_args_head)
        current_arg_node = sl_cdr(current_arg_node);
    }

    // 6. Call sl_apply (internal C apply)
    // Pass NULL for obj_ptr and env_ptr as this builtin cannot participate
    // in the main eval loop's TCO mechanism directly. The call *to* proc
    // can still be optimized internally by sl_apply if called from sl_eval.
    result = sl_apply(proc, final_args_head, NULL, NULL);

cleanup_apply_builtin:
    // Unroot everything
    SL_GC_REMOVE_ROOT(&result);
    SL_GC_REMOVE_ROOT(&last_arg_val);
    SL_GC_REMOVE_ROOT(&last_arg_node);
    SL_GC_REMOVE_ROOT(&current_arg_node);
    SL_GC_REMOVE_ROOT(&final_args_tail);
    SL_GC_REMOVE_ROOT(&final_args_head);
    SL_GC_REMOVE_ROOT(&proc);
    SL_GC_REMOVE_ROOT(&args);

    return result;
}

void sl_builtins_init(sl_object *global_env) {
    // Core functions
    define_builtin(global_env, "car", sl_builtin_car);
    define_builtin(global_env, "cdr", sl_builtin_cdr);
    define_builtin(global_env, "cadr", sl_builtin_cadr);
    define_builtin(global_env, "caar", sl_builtin_caar);
    define_builtin(global_env, "caddr", sl_builtin_caddr);
    define_builtin(global_env, "cddr", sl_builtin_cddr);
    define_builtin(global_env, "cons", sl_builtin_cons);
    define_builtin(global_env, "cons*", sl_builtin_cons_star);   // <<< ADDED
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
    define_builtin(global_env, "prime-factors", sl_builtin_prime_factors);            // <<< ADDED
    define_builtin(global_env, "next-prime", sl_builtin_next_prime);                  // <<< ADDED
    define_builtin(global_env, "float", sl_builtin_float);                            // <<< ADDED

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
    define_builtin(global_env, "write", sl_builtin_write);  // <<< ADDED
    define_builtin(global_env, "read", sl_builtin_read);    // <<< ADDED

    // Evaluation
    define_builtin(global_env, "eval", sl_builtin_eval);                                        // <<< ADDED
    define_builtin(global_env, "interaction-environment", sl_builtin_interaction_environment);  // <<< ADDED
    define_builtin(global_env, "environment", sl_builtin_environment);                          // <<< ADDED
    define_builtin(global_env, "apply", sl_builtin_apply);

    sl_predicates_init(global_env);
    sl_strings_init(global_env);
    sl_register_higher_order_primitives(global_env);
}