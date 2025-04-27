#ifndef SL_CORE_H
#define SL_CORE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <gmp.h>

// Forward declaration
struct sl_object;
// Remove forward declaration for sl_env, it's integrated now
// struct sl_env;

// --- Core Object Types ---
typedef enum {
    SL_TYPE_FREE = 0,  // Must be 0 for default memset state
    SL_TYPE_NIL,
    SL_TYPE_BOOLEAN,
    SL_TYPE_NUMBER,
    SL_TYPE_STRING,
    SL_TYPE_SYMBOL,
    SL_TYPE_PAIR,
    SL_TYPE_FUNCTION,
    SL_TYPE_ENV,
    SL_TYPE_ERROR  // New type for error objects
} sl_object_type;

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
        } small_num;
        // Large number representation (using GMP)
        // mpq_t is an array type (mpq_t[1]), so it's stored directly.
        mpq_t big_num;
    } value;
} sl_number;

// --- Function Definition ---
typedef struct sl_object *(*sl_builtin_func_ptr)(struct sl_object *args);

typedef union {
    // Builtin function
    struct {
        const char *name;  // Name for debugging/printing
        sl_builtin_func_ptr func_ptr;
    } builtin;
    // Closure (user-defined function)
    struct {
        struct sl_object *params;  // List of parameter symbols
        struct sl_object *body;    // Body expression (or list for implicit begin)
        struct sl_object *env;     // Captured environment (SL_TYPE_ENV)
    } closure;
} sl_function_def;

typedef struct {
    bool is_builtin;
    sl_function_def def;
} sl_function;

// --- Core Object Structure ---
typedef struct sl_object {
    sl_object_type type;
    bool marked;             // For garbage collection
    struct sl_object *next;  // Used for free list linking

    union {
        bool boolean;
        sl_number number;
        char *string;
        char *symbol;  // Pointer to the interned symbol name string
        struct {
            struct sl_object *car;
            struct sl_object *cdr;
        } pair;
        sl_function function;
        // New struct for environment data within the object
        struct {
            struct sl_object *bindings;  // List of pairs: ((symbol . value) ...)
            struct sl_object *outer;     // Pointer to the enclosing environment object (or SL_NIL)
        } env;
        char *error_message;  // Store error message string
    } data;
} sl_object;

// --- Global Variables ---
extern sl_object *sl_global_env;    // Now an sl_object*
extern sl_object *sl_symbol_table;  // Interned symbol table (e.g., a list or hash table)

// --- Constants ---

extern sl_object *SL_NIL;    // The unique empty list object
extern sl_object *SL_TRUE;   // The unique boolean true object
extern sl_object *SL_FALSE;  // The unique boolean false object

// --- Memory Management Functions ---

// Initialize the memory management system (Must also initialize GMP if needed)
void sl_mem_init(size_t initial_heap_size);

// Allocate specific object types
sl_object *sl_make_number_si(int64_t num, int64_t den);  // Create from signed 64-bit integers
sl_object *sl_make_number_z(const mpz_t num);            // Create integer from GMP mpz_t
sl_object *sl_make_number_q(const mpq_t value);          // Create rational from GMP mpq_t
sl_object *sl_make_string(const char *str);
sl_object *sl_make_symbol(const char *name);  // Interns the symbol
sl_object *sl_make_pair(sl_object *car, sl_object *cdr);
sl_object *sl_make_closure(sl_object *params, sl_object *body, sl_object *env);
sl_object *sl_make_builtin(const char *name, sl_object *(*func_ptr)(sl_object *args));
/**
 * @brief Creates a new error object with a formatted message.
 * Allocates the error object and the message string from the SymLisp heap.
 *
 * @param fmt The format string (printf-style).
 * @param ... Variable arguments for the format string.
 * @return sl_object* A pointer to the newly created error object (SL_TYPE_ERROR),
 *                  or SL_NIL on allocation failure.
 */
sl_object *sl_make_errorf(const char *fmt, ...);

// Trigger garbage collection
// NOTE: GC sweep phase must call mpq_clear() on unreachable SL_TYPE_NUMBER objects
//       where is_bignum is true before adding them to the free list.
void sl_gc();
static void sl_gc_mark(sl_object *obj);
sl_object *sl_allocate_object();

// Clean up memory management system (Must also clean up any global GMP state if needed)
void sl_mem_shutdown();

// --- Type Checking Macros/Functions ---
#define sl_is_pair(obj) ((obj) != NULL && (obj)->type == SL_TYPE_PAIR)
#define sl_is_nil(obj) ((obj) == SL_NIL)
#define sl_is_number(obj) ((obj) != NULL && (obj)->type == SL_TYPE_NUMBER)
#define sl_is_bignum(obj) (sl_is_number(obj) && (obj)->data.number.is_bignum)
#define sl_is_smallnum(obj) (sl_is_number(obj) && !(obj)->data.number.is_bignum)
#define sl_is_string(obj) ((obj) != NULL && (obj)->type == SL_TYPE_STRING)
#define sl_is_boolean(obj) ((obj) != NULL && (obj)->type == SL_TYPE_BOOLEAN)
#define sl_is_true(obj) ((obj) == SL_TRUE)
#define sl_is_false(obj) ((obj) == SL_FALSE || (obj) == SL_NIL)  // Scheme standard: nil is falsey
#define sl_is_symbol(obj) ((obj) != NULL && (obj)->type == SL_TYPE_SYMBOL)
#define sl_is_function(obj) ((obj) != NULL && (obj)->type == SL_TYPE_FUNCTION)
#define sl_is_closure(obj) (sl_is_function(obj) && !(obj)->data.function.is_builtin)
#define sl_is_builtin(obj) (sl_is_function(obj) && (obj)->data.function.is_builtin)
#define sl_is_env(obj) ((obj) && (obj)->type == SL_TYPE_ENV)
#define sl_is_error(obj) ((obj) && (obj)->type == SL_TYPE_ERROR)

// --- Accessor Macros/Functions ---
// Pair accessors
#define sl_car(obj) ((obj)->data.pair.car)
#define sl_cdr(obj) ((obj)->data.pair.cdr)
#define sl_set_car(obj, val) ((obj)->data.pair.car = (val))
#define sl_set_cdr(obj, val) ((obj)->data.pair.cdr = (val))

// Number accessors (Implementations needed in .c file)
// Attempts to get value as int64_t. Returns false if it's a bignum or doesn't fit.
bool sl_number_get_si(sl_object *obj, int64_t *num, int64_t *den);
// Gets the value as a GMP rational (copies into rop). Converts smallnum if necessary.
// 'rop' must be initialized by the caller (mpq_init).
void sl_number_get_q(sl_object *obj, mpq_t rop);
// Gets the numerator as GMP integer (copies into rop).
// 'rop' must be initialized by the caller (mpz_init).
void sl_number_get_num_z(sl_object *obj, mpz_t rop);
// Gets the denominator as GMP integer (copies into rop).
// 'rop' must be initialized by the caller (mpz_init).
void sl_number_get_den_z(sl_object *obj, mpz_t rop);
bool fits_int64(const mpz_t val);

// String, Symbol, Boolean accessors
#define sl_string_value(obj) ((obj)->data.string)
#define sl_symbol_name(obj) ((obj)->data.symbol)
#define sl_boolean_value(obj) ((obj)->data.boolean)  // Direct bool value

// Function accessors
#define sl_closure_params(obj) ((obj)->data.function.def.closure.params)
#define sl_closure_body(obj) ((obj)->data.function.def.closure.body)
#define sl_closure_env(obj) ((obj)->data.function.def.closure.env)
#define sl_builtin_name(obj) ((obj)->data.function.def.builtin.name)
#define sl_builtin_ptr(obj) ((obj)->data.function.def.builtin.func_ptr)

// Environment accessors
#define sl_env_bindings(obj) ((obj)->data.env.bindings)
#define sl_env_outer(obj) ((obj)->data.env.outer)
#define sl_set_env_bindings(obj, val) ((obj)->data.env.bindings = (val))

// --- Number accessors and helpers ---
// Gets the value as a GMP rational (copies into rop). Converts smallnum if necessary.
// 'rop' must be initialized by the caller (mpq_init).
void sl_number_get_q(sl_object *obj, mpq_t rop);
// Gets the numerator as GMP integer (copies into rop).
// 'rop' must be initialized by the caller (mpz_init).
void sl_number_get_num_z(sl_object *obj, mpz_t rop);
// Gets the denominator as GMP integer (copies into rop).
// 'rop' must be initialized by the caller (mpz_init).
void sl_number_get_den_z(sl_object *obj, mpz_t rop);
bool fits_int64(const mpz_t val);

// --- Error Handling Accessors ---
#define sl_error_message(obj) ((obj)->data.error_message)

#endif  // SL_CORE_H