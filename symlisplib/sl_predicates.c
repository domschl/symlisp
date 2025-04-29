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

    // Add other predicate groups here later (Equivalence, Numeric)
}