#ifndef SL_CORE_H
#define SL_CORE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <gmp.h>

// --- Type Definitions ---
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
    SL_TYPE_ERROR,  // New type for error objects
    SL_TYPE_CHAR,
    SL_TYPE_HTML,      // New type for HTML content (jupyter kernel)
    SL_TYPE_MARKDOWN,  // New type for Markdown content (jupyter kernel)
    SL_TYPE_EOF,       // New type for EOF object
    SL_TYPE_UNDEFINED  // <<< ADDED: Placeholder for letrec*/define
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
    bool marked;
    struct sl_object *next;

    union {
        bool boolean;
        sl_number number;
        char *string_val;   // <<< CORRECTED
        char *symbol_name;  // <<< CORRECTED
        struct {
            struct sl_object *car;
            struct sl_object *cdr;
        } pair;
        sl_function function;
        struct {
            struct sl_object *bindings;
            struct sl_object *outer;
            struct sl_object *macros;
        } env;
        char *error_str;      // <<< CORRECTED
        uint32_t code_point;  // Store Unicode code point for char
        struct {
            char *content;
        } rich_content;  // HTML/Markdown content
    } data;
} sl_object;

// --- Global Variables ---
extern sl_object *sl_global_env;    // Now an sl_object*
extern sl_object *sl_symbol_table;  // Interned symbol table (e.g., a list or hash table)

// --- Global Constants ---
extern sl_object *SL_NIL;
extern sl_object *SL_TRUE;
extern sl_object *SL_FALSE;
extern sl_object *SL_EOF_OBJECT;           // <<< ADDED
extern sl_object *SL_OUT_OF_MEMORY_ERROR;  // <<< ADDED
extern sl_object *SL_PARSE_ERROR;
extern sl_object *SL_UNDEFINED;        // <<< ADDED
#define SL_CONTINUE_EVAL SL_UNDEFINED  // <<< ADDED: Marker for TCO jump

// --- Helper macro for checking allocation result ---
#define CHECK_ALLOC(obj_ptr)                                                         \
    if ((obj_ptr) == SL_OUT_OF_MEMORY_ERROR) { return SL_OUT_OF_MEMORY_ERROR; }      \
    if (!(obj_ptr)) { /* Should not happen if SL_OUT_OF_MEMORY_ERROR is used */      \
        fprintf(stderr, "Internal Error: Allocation returned NULL unexpectedly.\n"); \
        return SL_OUT_OF_MEMORY_ERROR;                                               \
    }

// --- ADD CHECK_ALLOC_GOTO Macro ---
#define CHECK_ALLOC_GOTO(obj_ptr, label, result_var)                                 \
    if ((obj_ptr) == SL_OUT_OF_MEMORY_ERROR) {                                       \
        result_var = SL_OUT_OF_MEMORY_ERROR;                                         \
        goto label;                                                                  \
    }                                                                                \
    if (!(obj_ptr)) { /* Should not happen if SL_OUT_OF_MEMORY_ERROR is used */      \
        fprintf(stderr, "Internal Error: Allocation returned NULL unexpectedly.\n"); \
        result_var = SL_OUT_OF_MEMORY_ERROR;                                         \
        goto label;                                                                  \
    }

// --- ADD DEBUG_GC_ROOTS Macro ---
#define DEBUG_GC_ROOTS  // Enable debug GC root tracking

#ifdef DEBUG_GC_ROOTS  // Make it conditional for release builds
#define SL_GC_ADD_ROOT(ptr) sl_gc_add_root_debug(ptr, __FILE__, __LINE__)
#define SL_GC_REMOVE_ROOT(ptr) sl_gc_remove_root_debug(ptr, __FILE__, __LINE__)
#else
#define SL_GC_ADD_ROOT(ptr) sl_gc_add_root_impl(ptr)
#define SL_GC_REMOVE_ROOT(ptr) sl_gc_remove_root_impl(ptr)
#endif

// Function declarations (implement in sl_core.c)
#ifdef DEBUG_GC_ROOTS
void sl_gc_add_root_debug(sl_object **root_ptr, const char *file, int line);
void sl_gc_remove_root_debug(sl_object **root_ptr, const char *file, int line);  // Pass file/line for error messages
#else
void sl_gc_add_root_impl(sl_object **root_ptr);
void sl_gc_remove_root_impl(sl_object **root_ptr);
#endif

// --- Memory Management ---
// Initialize the memory management system (Must also initialize GMP if needed)
void sl_mem_init(size_t initial_heap_size);

// Allocate specific object types
sl_object *sl_make_number_si(int64_t num, int64_t den);              // Create from signed 64-bit integers
sl_object *sl_make_number_z(const mpz_t num);                        // Create integer from GMP mpz_t
sl_object *sl_make_number_zz(const mpz_t num_z, const mpz_t den_z);  // Create rational from GMP mpz_t
sl_object *sl_make_number_q(const mpq_t value);                      // Create rational from GMP mpq_t
sl_object *sl_make_string(const char *str);
sl_object *sl_make_char(uint32_t code_point);  // <<< ADDED
sl_object *sl_make_html(const char *html_content);
sl_object *sl_make_markdown(const char *md_content);
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

// --- String Conversion ---
/**
 * @brief Converts an sl_object to its string representation.
 * Dynamically allocates memory. Caller must free().
 * @param obj The object to convert.
 * @return char* Allocated string or NULL on failure.
 */
char *sl_object_to_string(sl_object *obj);                              // <<< ADDED
sl_object *sl_make_string_from_len(const char *buffer, size_t length);  // <<< ADDED
// --- Trigger garbage collection ---
// NOTE: GC sweep phase must call mpq_clear() on unreachable SL_TYPE_NUMBER objects
//       where is_bignum is true before adding them to the free list.
void sl_gc();
static void sl_gc_mark(sl_object *obj);
void sl_gc_add_root(sl_object **root_ptr);
void sl_gc_remove_root(sl_object **root_ptr);
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
#define sl_is_char(obj) ((obj) != NULL && (obj)->type == SL_TYPE_CHAR)  // <<< ADDED
#define sl_is_boolean(obj) ((obj) != NULL && (obj)->type == SL_TYPE_BOOLEAN)
#define sl_is_true(obj) ((obj) == SL_TRUE)
#define sl_is_false(obj) ((obj) == SL_FALSE || (obj) == SL_NIL)  // Scheme standard: nil is falsey
#define sl_is_symbol(obj) ((obj) != NULL && (obj)->type == SL_TYPE_SYMBOL)
#define sl_is_function(obj) ((obj) != NULL && (obj)->type == SL_TYPE_FUNCTION)
#define sl_is_closure(obj) (sl_is_function(obj) && !(obj)->data.function.is_builtin)
#define sl_is_builtin(obj) (sl_is_function(obj) && (obj)->data.function.is_builtin)
#define sl_is_env(obj) ((obj) && (obj)->type == SL_TYPE_ENV)
// #define sl_is_error(obj) ((obj) && (obj)->type == SL_TYPE_ERROR)
bool sl_is_list(sl_object *obj);  // <<< ADD DECLARATION

// Types
const char *sl_type_name(sl_object_type type);  // <<< ADDED

// --- Accessor Macros/Functions ---
// Pair accessors
// #define sl_car(obj) ((obj)->data.pair.car)
// #define sl_cdr(obj) ((obj)->data.pair.cdr)
// #define sl_set_car(obj, val) ((obj)->data.pair.car = (val))
// #define sl_set_cdr(obj, val) ((obj)->data.pair.cdr = (val))

static inline sl_object *sl_car(sl_object *obj) {
    if (!sl_is_pair(obj)) {
        // Option 1: Return NIL (safer for many Lisp operations)
        return SL_NIL;
        // Option 2: Trigger an error (stricter)
        // return sl_make_errorf("car: Expected a pair, got %s", sl_type_name(obj ? obj->type : -1));
    }
    return obj->data.pair.car;
}

static inline sl_object *sl_cdr(sl_object *obj) {
    if (!sl_is_pair(obj)) {
        // Option 1: Return NIL
        return SL_NIL;
        // Option 2: Trigger an error
        // return sl_make_errorf("cdr: Expected a pair, got %s", sl_type_name(obj ? obj->type : -1));
    }
    return obj->data.pair.cdr;
}

static inline void sl_set_car(sl_object *obj, sl_object *val) {
    if (!sl_is_pair(obj)) {
        // Handle error: maybe print to stderr, maybe make it return bool?
        fprintf(stderr, "Warning: set-car! called on non-pair.\n");
        return;
    }
    obj->data.pair.car = val;
}

static inline void sl_set_cdr(sl_object *obj, sl_object *val) {
    if (!sl_is_pair(obj)) {
        // Handle error
        fprintf(stderr, "Warning: set-cdr! called on non-pair.\n");
        return;
    }
    obj->data.pair.cdr = val;
}

sl_object *sl_caar(sl_object *list);
sl_object *sl_cadr(sl_object *list);
sl_object *sl_caddr(sl_object *list);
sl_object *sl_cddr(sl_object *list);

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
sl_object *sl_make_number_from_mpz(mpz_t val);
bool fits_int64(const mpz_t val);

// --- Number Predicates ---
bool sl_number_is_integer(sl_object *obj);  // <<< ADDED
bool sl_number_is_zero(sl_object *obj);     // <<< ADDED

// --- Number Accessors ---
// ... existing accessors ...
// Gets the integer value as GMP integer (copies into rop).
// Returns false and sets rop to 0 if obj is not an integer number.
// 'rop' must be initialized by the caller (mpz_init).
bool sl_number_get_z(sl_object *obj, mpz_t rop);  // <<< RENAMED from sl_number_get_num_z
// Gets the denominator as GMP integer (copies into rop).
// 'rop' must be initialized by the caller (mpz_init).
void sl_number_get_den_z(sl_object *obj, mpz_t rop);

// String, Symbol, Boolean accessors
#define sl_string_value(obj) ((obj)->data.string_val)  // <<< CORRECTED
#define sl_symbol_name(obj) ((obj)->data.symbol_name)  // <<< CORRECTED
#define sl_boolean_value(obj) ((obj)->data.boolean)    // Direct bool value

// UTF-8 helpers
// Unicode Replacement Character U+FFFD
#define UTF8_REPLACEMENT_CHAR 0xFFFD
int encode_utf8(uint32_t code_point, char *buffer);
uint32_t decode_utf8(const char **ptr);

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
sl_object *make_number_from_mpq(mpq_t val);
// --- Error Handling Accessors ---
// #define sl_error_message(obj) ((obj)->data.error_str)  // <<< CORRECTED

typedef struct {
    char *buffer;
    size_t length;
    size_t capacity;
} sl_string_builder;

void sl_sb_init(sl_string_builder *sb);
bool sl_sb_ensure_capacity(sl_string_builder *sb, size_t additional_needed);
bool sl_sb_append_str(sl_string_builder *sb, const char *str);
bool sl_sb_append_char(sl_string_builder *sb, char c);
char *sl_sb_finalize(sl_string_builder *sb);

// Jupyter kernel exports

// Add SL_EXPORT for cross-platform shared library visibility if needed
#if defined(_WIN32) || defined(_WIN64)
#ifdef SYMLISPLIB_EXPORTS  // Defined when building the DLL
#define SL_EXPORT __declspec(dllexport)
#else  // Defined when using the DLL
#define SL_EXPORT __declspec(dllimport)
#endif
#else
#define SL_EXPORT __attribute__((visibility("default")))
#endif

SL_EXPORT bool sl_is_error(sl_object *obj);              // <<< ADD FUNCTION DECLARATION
SL_EXPORT const char *sl_error_message(sl_object *obj);  // <<< ADD FUNCTION DECLARATION

SL_EXPORT sl_object_type sl_get_object_type(sl_object *obj);
SL_EXPORT const char *sl_get_rich_content_html(sl_object *obj);
SL_EXPORT const char *sl_get_rich_content_markdown(sl_object *obj);
SL_EXPORT void sl_free_c_string(char *str);  // For strings returned by sl_object_to_string

#endif  // SL_CORE_H