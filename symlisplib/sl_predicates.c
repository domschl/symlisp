#include <stdio.h>

#include "sl_predicates.h"
#include "sl_builtins.h"  // For check_arity, define_builtin
#include "sl_core.h"

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
    // --- DEBUG ---
    // fprintf(stderr, "[DEBUG error?] Checking object at %p, type=%d (%s)\n",
    //        (void *)args, args ? args->type : -1, args ? sl_type_name(args->type) : "null");
    bool is_err = sl_is_error(args);  // Assuming sl_is_error(obj) is ((obj) != NULL && (obj)->type == SL_TYPE_ERROR)
    // fprintf(stderr, "[DEBUG error?] sl_is_error macro returned: %s\n", is_err ? "true" : "false");
    // --- END DEBUG ---

    sl_object *result_bool = is_err ? SL_TRUE : SL_FALSE;

    // --- DEBUG ---
    // fprintf(stderr, "[DEBUG error?] Returning boolean object %s at %p, type=%d\n",
    //        is_err ? "SL_TRUE" : "SL_FALSE",
    //        (void *)result_bool, result_bool ? result_bool->type : -1);
    // --- END DEBUG ---

    return result_bool;
}

// (char? obj) -> #t if obj is a character, #f otherwise
static sl_object *sl_predicate_charp(sl_object *args) {
    sl_object *arity_check = check_arity("char?", args, 1);
    if (arity_check != SL_TRUE) return arity_check;
    sl_object *obj = sl_car(args);
    return sl_is_char(obj) ? SL_TRUE : SL_FALSE;
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
    define_builtin(global_env, "error?", sl_predicate_errorp);  // <<< ADDED

    // Add other predicate groups here later (Equivalence, Numeric)
}