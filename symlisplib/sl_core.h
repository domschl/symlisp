#ifndef SL_CORE_H
#define SL_CORE_H

#include <stddef.h>   // For size_t
#include <stdbool.h>  // For bool
#include <stdint.h>   // For int64_t
#include <gmp.h>

// Forward declarations
typedef struct sl_object sl_object;
typedef struct sl_env sl_env;  // Environment structure (to be defined elsewhere, e.g., sl_eval.h)

// --- Number Representation ---
// Represents an arbitrary precision rational number.
// Uses int64_t for small integers/rationals and mpq_t for larger ones.
typedef struct {
    bool is_bignum;  // True if mpq_t is used, false if int64_t fields are used
    union {
        // Small number representation (fits in int64_t)
        struct {
            int64_t num;  // Numerator
            int64_t den;  // Denominator (must be > 0)
        } small;
        // Large number representation (using GMP)
        // mpq_t is an array type (mpq_t[1]), so it's stored directly.
        mpq_t big_num;
    } value;
} sl_number;

// --- Function Representation ---
typedef struct {
    bool is_builtin;
    union {
        // User-defined function (closure)
        struct {
            sl_object *params;  // List of symbols
            sl_object *body;    // List of expressions
            sl_env *env;        // Captured environment
        } closure;
        // Built-in function
        struct {
            const char *name;  // Name for debugging/printing
            // Signature: takes list of evaluated args, returns result
            sl_object *(*func_ptr)(sl_object *args);
        } builtin;
    } def;
} sl_function;

// --- Core Object Types ---
typedef enum {
    SL_TYPE_NUMBER,
    SL_TYPE_STRING,
    SL_TYPE_BOOLEAN,
    SL_TYPE_SYMBOL,
    SL_TYPE_PAIR,
    SL_TYPE_FUNCTION,
    SL_TYPE_NIL,
    SL_TYPE_FREE  // Internal type for free list management
} sl_object_type;

// Structure for a Scheme pair (cons cell)
typedef struct {
    sl_object *car;
    sl_object *cdr;
} sl_pair;

// The core Scheme object structure
// Using a union for type-specific data
struct sl_object {
    sl_object_type type;
    bool marked;  // For garbage collection
    union {
        sl_number number;      // Rational number (placeholder precision)
        char *string;          // Allocated UTF-8 string
        bool boolean;          // Boolean value (#t or #f)
        char *symbol;          // Allocated, interned symbol name
        sl_pair pair;          // Cons cell
        sl_function function;  // Function (builtin or closure)
        // NIL has no specific data field
    } data;
    sl_object *next;  // For managing the heap / free list
};

// --- Global Variables ---

// The 'root set' for garbage collection (start with globals/stack)
extern sl_object *sl_global_env;    // The global environment
extern sl_object *sl_symbol_table;  // Interned symbol table (e.g., a list or hash table)

// --- Constants ---

extern sl_object *SL_NIL;    // The unique empty list object
extern sl_object *SL_TRUE;   // The unique boolean true object
extern sl_object *SL_FALSE;  // The unique boolean false object

// --- Memory Management Functions ---

// Initialize the memory management system
void sl_mem_init(size_t initial_heap_size);

// Allocate specific object types (examples)
sl_object *sl_make_number_si(int64_t num, int64_t den);  // Create from signed integers
// sl_object* sl_make_number_big(gmp_num, gmp_den); // Example for bignums
sl_object *sl_make_string(const char *str);
sl_object *sl_make_symbol(const char *name);  // Interns the symbol
sl_object *sl_make_pair(sl_object *car, sl_object *cdr);
sl_object *sl_make_closure(sl_object *params, sl_object *body, sl_env *env);
sl_object *sl_make_builtin(const char *name, sl_object *(*func_ptr)(sl_object *args));

// Trigger garbage collection
void sl_gc();

// Clean up memory management system
void sl_mem_shutdown();

// --- Type Checking Macros/Functions ---
#define sl_is_pair(obj) ((obj) != NULL && (obj)->type == SL_TYPE_PAIR)
#define sl_is_nil(obj) ((obj) == SL_NIL)
#define sl_is_number(obj) ((obj) != NULL && (obj)->type == SL_TYPE_NUMBER)
#define sl_is_string(obj) ((obj) != NULL && (obj)->type == SL_TYPE_STRING)
#define sl_is_boolean(obj) ((obj) != NULL && (obj)->type == SL_TYPE_BOOLEAN)
#define sl_is_true(obj) ((obj) == SL_TRUE)
#define sl_is_false(obj) ((obj) == SL_FALSE || (obj) == SL_NIL)  // Scheme standard: nil is falsey
#define sl_is_symbol(obj) ((obj) != NULL && (obj)->type == SL_TYPE_SYMBOL)
#define sl_is_function(obj) ((obj) != NULL && (obj)->type == SL_TYPE_FUNCTION)
#define sl_is_closure(obj) (sl_is_function(obj) && !(obj)->data.function.is_builtin)
#define sl_is_builtin(obj) (sl_is_function(obj) && (obj)->data.function.is_builtin)

// --- Accessor Macros/Functions (Add error checking!) ---
#define sl_car(obj) ((obj)->data.pair.car)
#define sl_cdr(obj) ((obj)->data.pair.cdr)
#define sl_set_car(obj, val) ((obj)->data.pair.car = (val))
#define sl_set_cdr(obj, val) ((obj)->data.pair.cdr = (val))

#define sl_number_num(obj) ((obj)->data.number.num)  // Placeholder
#define sl_number_den(obj) ((obj)->data.number.den)  // Placeholder

#define sl_string_value(obj) ((obj)->data.string)
#define sl_symbol_name(obj) ((obj)->data.symbol)
#define sl_boolean_value(obj) ((obj)->data.boolean)  // Direct bool value

#define sl_closure_params(obj) ((obj)->data.function.def.closure.params)
#define sl_closure_body(obj) ((obj)->data.function.def.closure.body)
#define sl_closure_env(obj) ((obj)->data.function.def.closure.env)
#define sl_builtin_name(obj) ((obj)->data.function.def.builtin.name)
#define sl_builtin_func(obj) ((obj)->data.function.def.builtin.func_ptr)

#endif  // SL_CORE_H