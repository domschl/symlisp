#include <stdio.h>

#include "sl_predicates.h"
#include "sl_builtins.h"  // For check_arity, define_builtin
#include "sl_core.h"
#include "sl_unicode_case.h"

// --- Type Predicates ---

// (boolean? obj) -> #t if obj is #t or #f, #f otherwise
static sl_object *sl_predicate_booleanp(sl_object *args) {
    sl_object *arity_check = check_arity("boolean?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return (obj == SL_TRUE || obj == SL_FALSE) ? SL_TRUE : SL_FALSE;
}

// (pair? obj) -> #t if obj is a pair (cons cell), #f otherwise
static sl_object *sl_predicate_pairp(sl_object *args) {
    sl_object *arity_check = check_arity("pair?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_pair(obj) ? SL_TRUE : SL_FALSE;
}

// (symbol? obj) -> #t if obj is a symbol, #f otherwise
static sl_object *sl_predicate_symbolp(sl_object *args) {
    sl_object *arity_check = check_arity("symbol?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_symbol(obj) ? SL_TRUE : SL_FALSE;
}

// (number? obj) -> #t if obj is a number, #f otherwise
static sl_object *sl_predicate_numberp(sl_object *args) {
    sl_object *arity_check = check_arity("number?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_number(obj) ? SL_TRUE : SL_FALSE;
}

// (string? obj) -> #t if obj is a string, #f otherwise
static sl_object *sl_predicate_stringp(sl_object *args) {
    sl_object *arity_check = check_arity("string?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_string(obj) ? SL_TRUE : SL_FALSE;
}

// (procedure? obj) -> #t if obj is a procedure (builtin or closure), #f otherwise
static sl_object *sl_predicate_procedurep(sl_object *args) {
    sl_object *arity_check = check_arity("procedure?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_function(obj) ? SL_TRUE : SL_FALSE;
}

// (null? obj) -> #t if obj is the empty list '(), #f otherwise
static sl_object *sl_predicate_nullp(sl_object *args) {
    sl_object *arity_check = check_arity("null?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return (obj == SL_NIL) ? SL_TRUE : SL_FALSE;
}

// (list? obj) -> #t if obj is a proper list, #f otherwise
static sl_object *sl_predicate_listp(sl_object *args) {
    sl_object *arity_check = check_arity("list?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    // Use a helper function (you might need to create sl_is_proper_list)
    return sl_is_list(obj) ? SL_TRUE : SL_FALSE;
}

// (error? obj) -> #t if obj is an error object, #f otherwise
static sl_object *sl_predicate_errorp(sl_object *args) {
    // <<< FIX: Need arity check and argument extraction >>>
    sl_object *arity_check = check_arity("error?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_error(obj) ? SL_TRUE : SL_FALSE;
}

// (char? obj) -> #t if obj is a character, #f otherwise
static sl_object *sl_predicate_charp(sl_object *args) {
    sl_object *arity_check = check_arity("char?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_char(obj) ? SL_TRUE : SL_FALSE;
}

// (environment? obj) -> #t if obj is an environment, #f otherwise
static sl_object *sl_predicate_environmentp(sl_object *args) {
    sl_object *arity_check = check_arity("environment?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    // Use the existing sl_is_env helper if available, otherwise check type directly
    return sl_is_env(obj) ? SL_TRUE : SL_FALSE;
    // Or: return (obj != NULL && obj->type == SL_TYPE_ENV) ? SL_TRUE : SL_FALSE;
}

// (atom? obj) -> #t if obj is not a pair, #f otherwise
static sl_object *sl_predicate_atomp(sl_object *args) {
    sl_object *arity_check = check_arity("atom?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_pair(obj) ? SL_FALSE : SL_TRUE;
}

// --- Type Predicates ---

// --- Numeric Predicates ---

// Helper: Returns true if obj is a number representing an integer, false otherwise.
static bool is_integer_object(sl_object *obj) {
    if (!sl_is_number(obj)) {
        return false;
    }
    sl_number *num = &obj->data.number;
    if (num->is_bignum) {
        return mpz_cmp_ui(mpq_denref(num->value.big_num), 1) == 0;
    } else {
        return num->value.small_num.den == 1;
    }
}

// (integer? obj) -> #t if obj is an integer number, #f otherwise
static sl_object *sl_predicate_integerp(sl_object *args) {
    sl_object *arity_check = check_arity("integer?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return is_integer_object(obj) ? SL_TRUE : SL_FALSE;
}

// (odd? n) -> #t if n is an odd integer, #f otherwise
static sl_object *sl_predicate_oddp(sl_object *args) {
    sl_object *arity_check = check_arity("odd?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);

    if (!is_integer_object(obj)) {  // <<< Uses the helper
        return SL_FALSE;            // Not an integer
    }

    sl_number *num = &obj->data.number;
    bool is_odd = false;

    if (num->is_bignum) {  // Handle mpq_t
        is_odd = mpz_odd_p(mpq_numref(num->value.big_num));
    } else {  // Handle small_num
        is_odd = (num->value.small_num.num % 2 != 0);
    }
    return is_odd ? SL_TRUE : SL_FALSE;
}

// (even? n) -> #t if n is an even integer, #f otherwise
static sl_object *sl_predicate_evenp(sl_object *args) {
    sl_object *arity_check = check_arity("even?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);

    if (!is_integer_object(obj)) {  // <<< Uses the helper
        return SL_FALSE;            // Not an integer
    }

    sl_number *num = &obj->data.number;
    bool is_even = false;

    if (num->is_bignum) {  // Handle mpq_t
        is_even = mpz_even_p(mpq_numref(num->value.big_num));
    } else {  // Handle small_num
        is_even = (num->value.small_num.num % 2 == 0);
    }
    return is_even ? SL_TRUE : SL_FALSE;
}

// (prime? n) -> #t if n is a prime integer, #f otherwise
static sl_object *sl_predicate_primep(sl_object *args) {
    sl_object *arity_check = check_arity("prime?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);

    if (!is_integer_object(obj)) {
        return SL_FALSE;  // Must be an integer
    }

    sl_number *num = &obj->data.number;
    int result = 0;  // 0 = composite, 1 = probably prime, 2 = definitely prime

    if (num->is_bignum) {
        // Use mpz_probab_prime_p directly on the numerator
        // Check if positive first
        if (mpz_sgn(mpq_numref(num->value.big_num)) <= 0) {
            return SL_FALSE;  // Primes must be positive
        }
        result = mpz_probab_prime_p(mpq_numref(num->value.big_num), 25);  // 25 reps is strong
    } else {
        // Handle small_num
        int64_t val = num->value.small_num.num;
        if (val <= 1) {
            return SL_FALSE;  // 1 and non-positive numbers are not prime
        }
        // Convert small int to mpz_t for testing
        mpz_t temp_z;
        mpz_init_set_si(temp_z, val);
        result = mpz_probab_prime_p(temp_z, 25);
        mpz_clear(temp_z);
    }

    // Return #t if result is 1 (probably prime) or 2 (definitely prime)
    return (result > 0) ? SL_TRUE : SL_FALSE;
}

// --- Character Predicate Helper ---
static sl_object *char_predicate(const char *name, sl_object *args, bool (*pred_func)(uint32_t)) {
    sl_object *arity_check = check_arity(name, args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *char_obj = sl_car(args);
    if (!sl_is_char(char_obj)) {
        return sl_make_errorf("%s: argument must be a character, got %s", name, sl_type_name(char_obj->type));
    }

    uint32_t cp = char_obj->data.code_point;
    return pred_func(cp) ? SL_TRUE : SL_FALSE;
}

// --- Builtin Predicates ---

sl_object *sl_builtin_char_alphabetic(sl_object *args) {
    return char_predicate("char-alphabetic?", args, sl_unicode_is_alphabetic);
}

sl_object *sl_builtin_char_numeric(sl_object *args) {
    return char_predicate("char-numeric?", args, sl_unicode_is_numeric);
}

sl_object *sl_builtin_char_whitespace(sl_object *args) {
    return char_predicate("char-whitespace?", args, sl_unicode_is_whitespace);
}

sl_object *sl_builtin_char_upper_case(sl_object *args) {
    return char_predicate("char-upper-case?", args, sl_unicode_is_uppercase);
}

sl_object *sl_builtin_char_lower_case(sl_object *args) {
    return char_predicate("char-lower-case?", args, sl_unicode_is_lowercase);
}

// --- Initialization ---

void sl_predicates_init(sl_object *global_env) {
    // Type Predicates
    define_builtin(global_env, "boolean?", sl_predicate_booleanp);
    define_builtin(global_env, "pair?", sl_predicate_pairp);
    define_builtin(global_env, "symbol?", sl_predicate_symbolp);
    define_builtin(global_env, "number?", sl_predicate_numberp);
    define_builtin(global_env, "string?", sl_predicate_stringp);
    define_builtin(global_env, "procedure?", sl_predicate_procedurep);
    define_builtin(global_env, "null?", sl_predicate_nullp);
    define_builtin(global_env, "list?", sl_predicate_listp);
    define_builtin(global_env, "char?", sl_predicate_charp);
    define_builtin(global_env, "error?", sl_predicate_errorp);
    define_builtin(global_env, "environment?", sl_predicate_environmentp);
    define_builtin(global_env, "atom?", sl_predicate_atomp);  // <<< ADDED

    // Numeric Predicates
    define_builtin(global_env, "integer?", sl_predicate_integerp);  // <<< ADDED
    define_builtin(global_env, "odd?", sl_predicate_oddp);
    define_builtin(global_env, "even?", sl_predicate_evenp);
    define_builtin(global_env, "prime?", sl_predicate_primep);  // <<< ADDED

    // Character Predicates
    define_builtin(global_env, "char-alphabetic?", sl_builtin_char_alphabetic);
    define_builtin(global_env, "char-numeric?", sl_builtin_char_numeric);
    define_builtin(global_env, "char-whitespace?", sl_builtin_char_whitespace);
    define_builtin(global_env, "char-upper-case?", sl_builtin_char_upper_case);
    define_builtin(global_env, "char-lower-case?", sl_builtin_char_lower_case);
}