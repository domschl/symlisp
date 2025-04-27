#ifndef SL_CORE_H
#define SL_CORE_H

#include <stddef.h>   // For size_t
#include <stdbool.h>  // For bool

// Forward declaration
typedef struct sl_object sl_object;

// Basic Scheme types (extend as needed)
typedef enum {
    SL_TYPE_PAIR,
    SL_TYPE_NIL,
    // Add other types like SL_TYPE_NUMBER, SL_TYPE_SYMBOL, SL_TYPE_PROCEDURE etc.
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
        sl_pair pair;
        // Add fields for other types, e.g., double number; char* symbol_name;
    } data;
    sl_object *next;  // For managing the heap / free list
};

// --- Global Variables ---

// The 'root set' for garbage collection (start with globals/stack)
// For simplicity, let's assume a single global environment pointer for now.
extern sl_object *sl_global_env;

// --- Constants ---

extern sl_object *SL_NIL;  // The empty list object

// --- Memory Management Functions ---

// Initialize the memory management system
void sl_mem_init(size_t initial_heap_size);

// Allocate a new Scheme pair
sl_object *sl_make_pair(sl_object *car, sl_object *cdr);

// Trigger garbage collection
void sl_gc();

// Clean up memory management system
void sl_mem_shutdown();

// --- Type Checking Macros/Functions ---
#define sl_is_pair(obj) ((obj) != NULL && (obj)->type == SL_TYPE_PAIR)
#define sl_is_nil(obj) ((obj) == SL_NIL)

#define sl_car(obj) (sl_is_pair(obj) ? (obj)->data.pair.car : NULL)  // Error handling needed
#define sl_cdr(obj) (sl_is_pair(obj) ? (obj)->data.pair.cdr : NULL)  // Error handling needed
#define sl_set_car(obj, val)                               \
    do {                                                   \
        if (sl_is_pair(obj)) (obj)->data.pair.car = (val); \
    } while (0)  // Error handling needed
#define sl_set_cdr(obj, val)                               \
    do {                                                   \
        if (sl_is_pair(obj)) (obj)->data.pair.cdr = (val); \
    } while (0)  // Error handling needed

#endif  // SL_CORE_H