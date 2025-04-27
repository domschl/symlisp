#include "sl_core.h"
#include "sl_env.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <gmp.h>
#include <stdarg.h>  // For variadic functions (va_list, etc.)

// --- Global Variables ---
sl_object *sl_global_env = NULL;    // Now an sl_object* pointing to an SL_TYPE_ENV object
sl_object *sl_symbol_table = NULL;  // Interned symbol table (needs proper init/management)
sl_object *SL_NIL = NULL;           // The unique empty list object
sl_object *SL_TRUE = NULL;          // The unique boolean true object
sl_object *SL_FALSE = NULL;         // The unique boolean false object

// --- Memory Management Internals ---
static sl_object *heap_start = NULL;  // Pointer to the start of the allocated heap
static sl_object *free_list = NULL;   // Pointer to the first free object
static size_t heap_size = 0;          // Total number of objects in the heap
static size_t free_count = 0;         // Number of free objects

// Helper function to check if a value fits in int64_t
// Note: GMP's mpz_fits_slong_p uses 'long', which might not be 64-bit.
// We need a check specifically for int64_t range if long != int64_t.
// For simplicity here, we assume long is sufficient or handle potential truncation.
// A more robust solution might involve mpz_export or string conversion.
bool fits_int64(const mpz_t val) {
#if LONG_MAX >= INT64_MAX && LONG_MIN <= INT64_MIN
    // If long is guaranteed to be at least 64 bits, mpz_fits_slong_p is sufficient
    return mpz_fits_slong_p(val);
#else
    // If long might be smaller than 64 bits (e.g., 32-bit systems),
    // mpz_fits_slong_p is too strict. We need a more direct check.
    // Check if the number of bits is within the range for a signed 64-bit integer.
    // mpz_sizeinbase(val, 2) gives the number of bits.
    // It must be less than 64 for positive numbers (0 to 2^63 - 1)
    // or exactly 64 for the most negative number (-2^63).
    size_t bits = mpz_sizeinbase(val, 2);
    if (bits < 64) {
        return true;  // Fits within 63 bits + sign bit
    } else if (bits == 64) {
        // Check if it's exactly the minimum int64_t value (-2^63)
        // We can do this by comparing it to a temporary mpz_t initialized to INT64_MIN
        static bool min_int64_initialized = false;
        static mpz_t min_int64_z;
        if (!min_int64_initialized) {
            mpz_init(min_int64_z);
            // Set mpz from int64_t requires intermediate string conversion or careful bit manipulation
            // Using string is easiest here.
            char min_int64_str[30];  // Sufficient buffer
            snprintf(min_int64_str, sizeof(min_int64_str), "%" PRId64, INT64_MIN);
            mpz_set_str(min_int64_z, min_int64_str, 10);
            min_int64_initialized = true;
            // TODO: Add cleanup for min_int64_z in sl_mem_shutdown or via atexit?
        }
        return mpz_cmp(val, min_int64_z) == 0;
    } else {
        // More than 64 bits, definitely doesn't fit.
        return false;
    }
#endif
}

// --- Memory Management Functions ---

void sl_mem_init(size_t initial_heap_size) {
    if (heap_start != NULL) {
        return;  // Already initialized
    }

    heap_size = initial_heap_size;
    if (heap_size == 0) heap_size = 4096;  // Default heap size (increased a bit)

    heap_start = (sl_object *)malloc(heap_size * sizeof(sl_object));
    if (heap_start == NULL) {
        perror("Failed to allocate heap");
        exit(EXIT_FAILURE);
    }

    // Initialize all objects and link them into the free list
    free_list = heap_start;
    for (size_t i = 0; i < heap_size; ++i) {
        heap_start[i].type = SL_TYPE_FREE;
        heap_start[i].marked = false;
        heap_start[i].next = (i + 1 < heap_size) ? &heap_start[i + 1] : NULL;
        // Ensure union is zeroed out initially
        memset(&heap_start[i].data, 0, sizeof(heap_start[i].data));
    }
    free_count = heap_size;

    // --- Allocate Constants ---
    // These are allocated outside the main heap for simplicity,
    // they won't be garbage collected by the current scheme.
    // A more integrated approach might put them in a separate static area.

    // NIL
    SL_NIL = (sl_object *)malloc(sizeof(sl_object));
    if (!SL_NIL) {
        perror("Failed to allocate NIL");
        exit(EXIT_FAILURE);
    }
    SL_NIL->type = SL_TYPE_NIL;
    SL_NIL->marked = false;  // Always reachable conceptually
    SL_NIL->next = NULL;
    memset(&SL_NIL->data, 0, sizeof(SL_NIL->data));

    // TRUE
    SL_TRUE = (sl_object *)malloc(sizeof(sl_object));
    if (!SL_TRUE) {
        perror("Failed to allocate TRUE");
        exit(EXIT_FAILURE);
    }
    SL_TRUE->type = SL_TYPE_BOOLEAN;
    SL_TRUE->marked = false;  // Always reachable
    SL_TRUE->next = NULL;
    SL_TRUE->data.boolean = true;

    // FALSE
    SL_FALSE = (sl_object *)malloc(sizeof(sl_object));
    if (!SL_FALSE) {
        perror("Failed to allocate FALSE");
        exit(EXIT_FAILURE);
    }
    SL_FALSE->type = SL_TYPE_BOOLEAN;
    SL_FALSE->marked = false;  // Always reachable
    SL_FALSE->next = NULL;
    SL_FALSE->data.boolean = false;

    // Initialize global environment and symbol table
    sl_global_env = sl_env_create(SL_NIL);  // Create the global env object (outer is NIL)
    if (sl_global_env == SL_NIL) {
        fprintf(stderr, "FATAL: Failed to create global environment.\n");
        // Maybe free constants and heap before exiting?
        exit(EXIT_FAILURE);
    }
    sl_symbol_table = SL_NIL;  // Placeholder
}

void sl_mem_shutdown() {
    // --- Clean up Symbol Table Strings (Placeholder Logic) ---
    // Since the placeholder sl_make_symbol uses strdup and adds all symbols
    // to the table, we must free the strings here before freeing the heap.
    sl_object *current = sl_symbol_table;
    while (sl_is_pair(current)) {
        sl_object *pair = current;
        sl_object *symbol_obj = sl_car(pair);
        // Check if the car is actually a symbol (it should be)
        if (symbol_obj && symbol_obj->type == SL_TYPE_SYMBOL && symbol_obj->data.symbol) {
            // Free the string allocated by strdup in the placeholder sl_make_symbol
            free(symbol_obj->data.symbol);
            // Set to NULL to prevent potential double-free if somehow processed again
            symbol_obj->data.symbol = NULL;
        }
        current = sl_cdr(pair);
        // Note: We don't free the pair objects themselves here; they are part of the main heap
        // and will be freed along with it.
    }
    sl_symbol_table = SL_NIL;  // Clear the root pointer after processing

    // --- Clear GMP numbers and free strings in the rest of the heap ---
    // (Strings other than symbol names, if any were left)
    if (heap_start) {
        for (size_t i = 0; i < heap_size; ++i) {
            sl_object *obj = &heap_start[i];
            if (obj->type != SL_TYPE_FREE) {
                if (obj->type == SL_TYPE_NUMBER && obj->data.number.is_bignum) {
                    mpq_clear(obj->data.number.value.big_num);
                } else if (obj->type == SL_TYPE_STRING && obj->data.string) {
                    free(obj->data.string);
                    obj->data.string = NULL;
                }
                // No need to handle SL_TYPE_SYMBOL here (done above).
                // No need to handle SL_TYPE_ENV here (contents are GC'd objects).
            }
        }
    }

    // --- Free the heap itself ---
    free(heap_start);
    heap_start = NULL;
    free_list = NULL;
    heap_size = 0;
    free_count = 0;

    // --- Free Constants ---
    free(SL_NIL);
    SL_NIL = NULL;
    free(SL_TRUE);
    SL_TRUE = NULL;
    free(SL_FALSE);
    SL_FALSE = NULL;

    // --- Clean up Globals ---
    sl_global_env = NULL;  // Clear the global env pointer

    // --- GMP Cleanup ---
    // mp_set_memory_functions(NULL, NULL, NULL); // Restore default allocators if changed
}

// Simple allocation from the free list
sl_object *sl_allocate_object() {
    if (free_list == NULL) {
        sl_gc();  // Attempt to collect garbage
        if (free_list == NULL) {
            // Still no memory, maybe expand heap or signal error
            fprintf(stderr, "Error: Out of memory! Heap size: %zu\n", heap_size);
            // In a real implementation, you might try to expand the heap here.
            // For now, we exit. Consider using realloc for heap_start.
            exit(EXIT_FAILURE);
        }
    }

    sl_object *new_obj = free_list;
    free_list = new_obj->next;  // Remove from free list
    new_obj->next = NULL;       // Clear the next pointer
    new_obj->marked = false;    // Newly allocated objects are not marked initially
    // Ensure data area is clean before use
    memset(&new_obj->data, 0, sizeof(new_obj->data));
    free_count--;
    return new_obj;
}

// --- Object Constructors ---

sl_object *sl_make_number_si(int64_t num, int64_t den) {
    if (den == 0) {
        fprintf(stderr, "Error: Division by zero in number creation.\n");
        // Return NIL or signal error appropriately
        return SL_NIL;  // Or perhaps exit?
    }
    // Canonical form: denominator positive
    if (den < 0) {
        num = -num;
        den = -den;
    }

    sl_object *new_num = sl_allocate_object();
    new_num->type = SL_TYPE_NUMBER;
    new_num->data.number.is_bignum = false;  // Assume small initially
    new_num->data.number.value.small.num = num;
    new_num->data.number.value.small.den = den;

    // Note: We don't automatically promote to bignum here.
    // Arithmetic operations will need to handle potential overflow
    // and create bignums as needed. This constructor just stores
    // the int64_t values directly.

    return new_num;
}

sl_object *sl_make_number_z(const mpz_t num_z) {
    sl_object *new_num = sl_allocate_object();
    new_num->type = SL_TYPE_NUMBER;
    new_num->data.number.is_bignum = true;
    mpq_init(new_num->data.number.value.big_num);
    mpq_set_z(new_num->data.number.value.big_num, num_z);
    // mpq_canonicalize(new_num->data.number.value.big_num); // Already canonical
    return new_num;
}

sl_object *sl_make_number_q(const mpq_t value_q) {
    sl_object *new_num = sl_allocate_object();
    new_num->type = SL_TYPE_NUMBER;

    // Check if the GMP rational fits into int64_t components
    mpz_t num_z, den_z;
    mpz_inits(num_z, den_z, NULL);
    mpq_get_num(num_z, value_q);
    mpq_get_den(den_z, value_q);  // Denominator is always positive after canonicalization

    if (fits_int64(num_z) && fits_int64(den_z)) {
        // It fits, store as smallnum
        new_num->data.number.is_bignum = false;
        new_num->data.number.value.small.num = mpz_get_si(num_z);  // Assuming long fits int64_t based on fits_int64 logic
        new_num->data.number.value.small.den = mpz_get_si(den_z);
    } else {
        // Doesn't fit, store as bignum
        new_num->data.number.is_bignum = true;
        mpq_init(new_num->data.number.value.big_num);
        mpq_set(new_num->data.number.value.big_num, value_q);
        // No need to canonicalize here, assuming input value_q is already canonical or mpq_set handles it.
        // If input might not be canonical, add: mpq_canonicalize(new_num->data.number.value.big_num);
    }

    mpz_clears(num_z, den_z, NULL);
    return new_num;
}

sl_object *sl_make_string(const char *str) {
    sl_object *new_str = sl_allocate_object();
    new_str->type = SL_TYPE_STRING;
    new_str->data.string = strdup(str);  // Use strdup for allocation and copy
    if (new_str->data.string == NULL) {
        perror("Failed to allocate memory for string");
        // Handle error: maybe free new_str and return NIL?
        // For now, let allocation failure propagate (likely caught by malloc checks)
        // Put the object back on the free list?
        new_str->type = SL_TYPE_FREE;
        new_str->next = free_list;
        free_list = new_str;
        free_count++;
        return SL_NIL;  // Indicate failure
    }
    return new_str;
}

sl_object *sl_make_symbol(const char *name) {
    // TODO: Implement symbol interning using sl_symbol_table
    // 1. Search sl_symbol_table for an existing symbol with 'name'.
    // 2. If found, return the existing sl_object*.
    // 3. If not found:
    //    a. Allocate a new sl_object.
    //    b. Set type to SL_TYPE_SYMBOL.
    //    c. Allocate and copy 'name' using strdup into data.symbol. Check for alloc failure.
    //    d. Add the new symbol object to sl_symbol_table (e.g., cons onto the list).
    //    e. Return the new object.

    // Placeholder implementation (no interning):
    sl_object *new_sym = sl_allocate_object();
    new_sym->type = SL_TYPE_SYMBOL;
    new_sym->data.symbol = strdup(name);  // Allocate and copy
    if (new_sym->data.symbol == NULL) {
        perror("Failed to allocate memory for symbol name");
        // Put object back on free list
        new_sym->type = SL_TYPE_FREE;
        new_sym->next = free_list;
        free_list = new_sym;
        free_count++;
        return SL_NIL;  // Indicate failure
    }
    // Add to symbol table (simple list prepend for now)
    sl_symbol_table = sl_make_pair(new_sym, sl_symbol_table);

    return new_sym;
}

sl_object *sl_make_pair(sl_object *car, sl_object *cdr) {
    sl_object *new_pair = sl_allocate_object();
    new_pair->type = SL_TYPE_PAIR;
    new_pair->data.pair.car = car;
    new_pair->data.pair.cdr = cdr;
    return new_pair;
}

sl_object *sl_make_closure(sl_object *params, sl_object *body, sl_object *env_obj) {
    if (!sl_is_env(env_obj) && env_obj != SL_NIL) {  // Allow NIL env? Maybe not.
        fprintf(stderr, "Error (make_closure): Invalid environment object provided.\n");
        return SL_NIL;  // Indicate error
    }
    sl_object *new_func = sl_allocate_object();
    if (!new_func) return SL_NIL;  // Allocation failed

    new_func->type = SL_TYPE_FUNCTION;
    new_func->data.function.is_builtin = false;
    new_func->data.function.def.closure.params = params;
    new_func->data.function.def.closure.body = body;
    new_func->data.function.def.closure.env = env_obj;  // Store the env object pointer
    return new_func;
}

sl_object *sl_make_builtin(const char *name, sl_object *(*func_ptr)(sl_object *args)) {
    sl_object *new_func = sl_allocate_object();
    new_func->type = SL_TYPE_FUNCTION;
    new_func->data.function.is_builtin = true;
    // Note: 'name' is assumed to be a static string literal, not allocated.
    new_func->data.function.def.builtin.name = name;
    new_func->data.function.def.builtin.func_ptr = func_ptr;
    return new_func;
}

sl_object *sl_make_errorf(const char *fmt, ...) {
    sl_object *err_obj = sl_allocate_object();
    if (!err_obj) return SL_NIL;  // Allocation failed

    err_obj->type = SL_TYPE_ERROR;

    va_list args1, args2;
    va_start(args1, fmt);
    va_copy(args2, args1);  // Need a copy because vsnprintf might be called twice

    // Determine required size
    int size = vsnprintf(NULL, 0, fmt, args1);
    va_end(args1);

    if (size < 0) {
        perror("vsnprintf size calculation failed");
        // Put object back on free list?
        err_obj->type = SL_TYPE_FREE;
        err_obj->next = free_list;
        free_list = err_obj;
        free_count++;
        va_end(args2);
        return SL_NIL;  // Indicate failure
    }

    // Allocate buffer (+1 for null terminator)
    char *buffer = (char *)malloc(size + 1);
    if (!buffer) {
        perror("malloc failed for error message");
        // Put object back on free list?
        err_obj->type = SL_TYPE_FREE;
        err_obj->next = free_list;
        free_list = err_obj;
        free_count++;
        va_end(args2);
        return SL_NIL;  // Indicate failure
    }

    // Format the string into the buffer
    vsnprintf(buffer, size + 1, fmt, args2);
    va_end(args2);

    err_obj->data.error_message = buffer;  // Store the allocated string
    return err_obj;
}

// --- Number Accessors ---

bool sl_number_get_si(sl_object *obj, int64_t *num_out, int64_t *den_out) {
    if (!sl_is_number(obj)) return false;  // Not a number

    if (obj->data.number.is_bignum) {
        // It's a bignum, check if it fits in int64_t
        mpz_t num_z, den_z;
        mpz_inits(num_z, den_z, NULL);
        mpq_get_num(num_z, obj->data.number.value.big_num);
        mpq_get_den(den_z, obj->data.number.value.big_num);

        bool fits = fits_int64(num_z) && fits_int64(den_z);
        if (fits) {
            *num_out = mpz_get_si(num_z);  // Use mpz_get_si (long) - check limits if needed
            *den_out = mpz_get_si(den_z);
        }
        mpz_clears(num_z, den_z, NULL);
        return fits;
    } else {
        // It's already a small number
        *num_out = obj->data.number.value.small.num;
        *den_out = obj->data.number.value.small.den;
        return true;
    }
}

void sl_number_get_q(sl_object *obj, mpq_t rop) {
    // Assumes rop is initialized by caller (mpq_init)
    if (!sl_is_number(obj)) {
        mpq_set_si(rop, 0, 1);  // Set to 0 if not a number? Or signal error?
        return;
    }

    if (obj->data.number.is_bignum) {
        mpq_set(rop, obj->data.number.value.big_num);
    } else {
        // Convert small number to mpq_t
        mpq_set_si(rop,
                   obj->data.number.value.small.num,
                   obj->data.number.value.small.den);
        mpq_canonicalize(rop);  // Ensure canonical form
    }
}

void sl_number_get_num_z(sl_object *obj, mpz_t rop) {
    // Assumes rop is initialized by caller (mpz_init)
    if (!sl_is_number(obj)) {
        mpz_set_si(rop, 0);  // Set to 0 if not a number? Or signal error?
        return;
    }
    if (obj->data.number.is_bignum) {
        mpq_get_num(rop, obj->data.number.value.big_num);
    } else {
        mpz_set_si(rop, obj->data.number.value.small.num);
    }
}

void sl_number_get_den_z(sl_object *obj, mpz_t rop) {
    // Assumes rop is initialized by caller (mpz_init)
    if (!sl_is_number(obj)) {
        mpz_set_ui(rop, 1);  // Set to 1 if not a number? Or signal error?
        return;
    }
    if (obj->data.number.is_bignum) {
        mpq_get_den(rop, obj->data.number.value.big_num);
    } else {
        mpz_set_si(rop, obj->data.number.value.small.den);
    }
}

// --- Garbage Collection (Mark and Sweep) ---

// Mark phase: Recursively mark all reachable objects
void sl_gc_mark(sl_object *obj) {
    // Check if obj pointer is valid within the heap range OR if it's a known constant
    bool is_in_heap = (obj >= heap_start && obj < heap_start + heap_size);
    bool is_constant = (obj == SL_NIL || obj == SL_TRUE || obj == SL_FALSE);

    if (obj == NULL || (!is_in_heap && !is_constant)) {
        // fprintf(stderr, "Warning: Trying to mark invalid pointer: %p\n", (void*)obj);
        return;  // Not a valid object pointer we manage or NULL
    }

    // Constants don't live in the heap, don't try to mark them directly
    if (is_constant) {
        return;
    }

    // If it's in the heap, check if already marked
    if (obj->marked) {
        return;
    }

    obj->marked = true;

    // Recursively mark children based on type
    switch (obj->type) {
    case SL_TYPE_PAIR:
        sl_gc_mark(obj->data.pair.car);
        sl_gc_mark(obj->data.pair.cdr);
        break;
    case SL_TYPE_FUNCTION:
        if (!obj->data.function.is_builtin) {
            // Mark closure components
            sl_gc_mark(obj->data.function.def.closure.params);
            sl_gc_mark(obj->data.function.def.closure.body);
            // Mark the captured environment object
            sl_gc_mark(obj->data.function.def.closure.env);  // Mark the sl_object*
        }
        break;
    case SL_TYPE_ENV:                        // New case for environment objects
        sl_gc_mark(obj->data.env.bindings);  // Mark the list of bindings
        sl_gc_mark(obj->data.env.outer);     // Mark the outer environment object
        break;
    case SL_TYPE_NUMBER:   // Numbers don't reference other sl_objects
    case SL_TYPE_STRING:   // String data is freed in sweep, no sl_object refs
    case SL_TYPE_BOOLEAN:  // Constants or simple value
    case SL_TYPE_SYMBOL:   // Symbol name is freed in sweep (if not interned), no sl_object refs
    case SL_TYPE_NIL:      // Constant
    case SL_TYPE_FREE:     // Should not be reachable if logic is correct
        break;
    default:
        // Should not happen
        fprintf(stderr, "Warning: Unknown object type %d during GC mark.\n", obj->type);
        break;
    }
}

// Sweep phase: Collect all unmarked objects and add them to the free list
static void sl_gc_sweep() {
    free_list = NULL;  // Rebuild the free list from scratch
    free_count = 0;

    for (size_t i = 0; i < heap_size; ++i) {
        sl_object *obj = &heap_start[i];

        if (obj->type == SL_TYPE_FREE) {
            // Already free, add it to the rebuilt list
            obj->next = free_list;
            free_list = obj;
            free_count++;
            continue;  // Skip to next object
        }

        // Object was allocated, check if marked
        if (obj->marked) {
            obj->marked = false;
        } else {
            // Unreachable: free associated resources
            switch (obj->type) {
            case SL_TYPE_NUMBER:
                if (obj->data.number.is_bignum) {
                    mpq_clear(obj->data.number.value.big_num);
                }
                break;
            case SL_TYPE_STRING:
                free(obj->data.string);  // Free the string data
                obj->data.string = NULL;
                break;
            case SL_TYPE_SYMBOL:
                // Since the current sl_make_symbol uses strdup (no interning),
                // we MUST free the string when the symbol object is collected.
                free(obj->data.symbol);   // XXX Note: Once you implement proper symbol interning, you will need to change this again. With interning, the symbol table would own the strings, and they would only be freed during sl_mem_shutdown after clearing the table, not during the regular GC sweep.)
                obj->data.symbol = NULL;  // Optional: clear pointer after freeing
                break;
            case SL_TYPE_ERROR:  // Add case for ERROR
                free(obj->data.error_message);
                obj->data.error_message = NULL;
                break;
            case SL_TYPE_PAIR:
            case SL_TYPE_FUNCTION:  // No extra freeing needed for function object itself
            case SL_TYPE_ENV:       // No extra freeing needed for env object itself
            case SL_TYPE_BOOLEAN:
            case SL_TYPE_NIL:
                break;  // No extra freeing needed besides the object slot itself
            default:
                // Should not happen for valid allocated types
                fprintf(stderr, "Warning: Sweeping unexpected type %d\n", obj->type);
                break;
            }

            // Clear the object's data and set type to FREE
            memset(&obj->data, 0, sizeof(obj->data));
            obj->type = SL_TYPE_FREE;

            // Add to the rebuilt free list
            obj->next = free_list;
            free_list = obj;
            free_count++;
        }
    }
    // printf("GC Sweep finished. Total free: %zu\n", free_count);
}

// Main GC function
void sl_gc() {
    // printf("Starting GC... Free count before: %zu\n", free_count);

    // 1. Mark all objects reachable from the root set
    sl_gc_mark(sl_global_env);  // Mark the global environment object
    sl_gc_mark(sl_symbol_table);

    // TODO: Mark stack roots

    // 2. Sweep unreachable objects
    sl_gc_sweep();

    // printf("GC finished. Free count after: %zu\n", free_count);
}