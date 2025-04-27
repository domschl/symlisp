#include "sl_core.h"
#include <stdio.h>
#include <stdlib.h>  // For malloc, free, exit
#include <string.h>  // For memset

// --- Global Variables ---
sl_object *sl_global_env = NULL;  // Initialize global environment
sl_object *SL_NIL = NULL;         // The unique NIL object

// --- Memory Management Internals ---
static sl_object *heap_start = NULL;  // Pointer to the start of the allocated heap
static sl_object *free_list = NULL;   // Pointer to the first free object
static size_t heap_size = 0;          // Total number of objects in the heap
static size_t free_count = 0;         // Number of free objects

// --- Memory Management Functions ---

void sl_mem_init(size_t initial_heap_size) {
    if (heap_start != NULL) {
        // Already initialized
        return;
    }

    heap_size = initial_heap_size;
    if (heap_size == 0) heap_size = 1024;  // Default heap size

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
    }
    free_count = heap_size;

    // Allocate the unique NIL object (doesn't need GC itself, but marks others)
    // For simplicity, allocate it like any other object for now.
    // A more robust implementation might place constants outside the main heap.
    SL_NIL = (sl_object *)malloc(sizeof(sl_object));  // Allocate separately for now
    if (SL_NIL == NULL) {
        perror("Failed to allocate NIL");
        exit(EXIT_FAILURE);
    }
    SL_NIL->type = SL_TYPE_NIL;
    SL_NIL->marked = false;  // Should always be considered reachable
    SL_NIL->next = NULL;     // Not part of the free list or heap management chains

    // Initialize the global environment (e.g., an empty list)
    // sl_global_env = SL_NIL; // Or allocate an empty environment frame
}

void sl_mem_shutdown() {
    free(heap_start);
    free(SL_NIL);  // Free the separately allocated NIL
    heap_start = NULL;
    free_list = NULL;
    SL_NIL = NULL;
    sl_global_env = NULL;
    heap_size = 0;
    free_count = 0;
}

// Simple allocation from the free list
static sl_object *sl_allocate_object() {
    if (free_list == NULL) {
        sl_gc();  // Attempt to collect garbage
        if (free_list == NULL) {
            // Still no memory, maybe expand heap or signal error
            fprintf(stderr, "Error: Out of memory!\n");
            // In a real implementation, you might try to expand the heap here.
            exit(EXIT_FAILURE);  // Simple exit for now
        }
    }

    sl_object *new_obj = free_list;
    free_list = new_obj->next;  // Remove from free list
    new_obj->next = NULL;       // Clear the next pointer
    new_obj->marked = false;    // Newly allocated objects are not marked initially
    free_count--;
    return new_obj;
}

sl_object *sl_make_pair(sl_object *car, sl_object *cdr) {
    sl_object *new_pair = sl_allocate_object();
    new_pair->type = SL_TYPE_PAIR;
    new_pair->data.pair.car = car;
    new_pair->data.pair.cdr = cdr;
    return new_pair;
}

// --- Garbage Collection (Mark and Sweep) ---

// Mark phase: Recursively mark all reachable objects
static void sl_gc_mark(sl_object *obj) {
    if (obj == NULL || obj->marked) {
        return;  // Already marked or not a heap object (like SL_NIL if handled specially)
    }

    // Check if the object is within our managed heap
    // This check is basic; a more robust check might be needed
    // if objects can be allocated outside this specific heap.
    if (obj < heap_start || obj >= heap_start + heap_size) {
        if (obj == SL_NIL) {  // Handle NIL specially if needed
            // SL_NIL itself doesn't need marking within the heap context,
            // but we don't want to crash trying to access its potential children.
            return;
        }
        // Potentially log or handle error for pointers outside the heap
        // fprintf(stderr, "Warning: Trying to mark object outside heap: %p\n", (void*)obj);
        return;
    }

    obj->marked = true;

    // Recursively mark children based on type
    switch (obj->type) {
    case SL_TYPE_PAIR:
        sl_gc_mark(obj->data.pair.car);
        sl_gc_mark(obj->data.pair.cdr);
        break;
    // Add cases for other types that hold references (e.g., closures, vectors)
    case SL_TYPE_NIL:   // Should ideally not be in the markable heap or handled above
    case SL_TYPE_FREE:  // Should not be reachable
        break;
    // Add cases for numbers, symbols, etc. which don't have children to mark
    default:
        break;
    }
}

// Sweep phase: Collect all unmarked objects and add them to the free list
static void sl_gc_sweep() {
    free_list = NULL;
    free_count = 0;
    sl_object *current_free_tail = NULL;

    for (size_t i = 0; i < heap_size; ++i) {
        sl_object *obj = &heap_start[i];
        if (obj->type != SL_TYPE_FREE) {  // Don't sweep already free blocks
            if (obj->marked) {
                // Object is reachable, unmark it for the next GC cycle
                obj->marked = false;
            } else {
                // Object is unreachable, add it to the free list
                obj->type = SL_TYPE_FREE;
                // Clear data for safety/debugging (optional)
                memset(&obj->data, 0, sizeof(obj->data));

                // Add to free list
                obj->next = free_list;
                free_list = obj;
                free_count++;
            }
        } else {
            // If it was already free, ensure it's linked correctly (or handle fragmentation)
            // For this simple contiguous heap, just count it if it wasn't already counted.
            // A more complex allocator might need to merge free blocks here.
            // In this simple list approach, already-free blocks are implicitly handled
            // when rebuilding the list. We just need to ensure the count is right.
            // Let's rebuild the free list entirely from scratch.
        }
    }

    // Rebuild the free list completely to ensure correctness
    free_list = NULL;
    free_count = 0;
    for (size_t i = 0; i < heap_size; ++i) {
        sl_object *obj = &heap_start[i];
        if (obj->type == SL_TYPE_FREE) {
            obj->next = free_list;
            free_list = obj;
            free_count++;
        }
    }

    // printf("GC Sweep: Freed %zu objects. Total free: %zu\n", free_count /* newly freed might be complex to track here */, free_count);
}

// Main GC function
void sl_gc() {
    // printf("Starting GC...\n");

    // 1. Mark all objects reachable from the root set
    // Mark objects reachable from the global environment
    sl_gc_mark(sl_global_env);

    // Mark SL_NIL (important if it references things, though usually doesn't)
    // If SL_NIL is allocated outside the heap, this mark call might do nothing
    // or need special handling depending on the sl_gc_mark implementation.
    // sl_gc_mark(SL_NIL); // Usually not needed unless NIL has structure

    // TODO: Mark objects reachable from the C stack (requires stack scanning or careful management)
    // TODO: Mark objects reachable from registers (less common to handle directly)

    // 2. Sweep unreachable objects
    sl_gc_sweep();

    // printf("GC finished. Free count: %zu\n", free_count);
}