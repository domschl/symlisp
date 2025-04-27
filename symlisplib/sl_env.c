#include "sl_env.h"
#include "sl_core.h"  // Access to sl_make_pair, sl_car, sl_cdr, etc.
#include <stdio.h>
#include <stdlib.h>  // For malloc, free (no longer needed for env struct)
#include <string.h>  // For strcmp

// --- Environment Operations ---

sl_object *sl_env_create(sl_object *outer_env_obj) {
    // Ensure outer is either NIL or an ENV object
    if (outer_env_obj != SL_NIL && !sl_is_env(outer_env_obj)) {
        fprintf(stderr, "Error (env_create): Outer environment must be NIL or an ENV object.\n");
        return SL_NIL;  // Indicate error
    }

    sl_object *env_obj = sl_allocate_object();
    if (!env_obj) {
        // Allocation failure already printed by sl_allocate_object or sl_gc
        return SL_NIL;  // Indicate failure
    }
    env_obj->type = SL_TYPE_ENV;
    env_obj->data.env.bindings = SL_NIL;  // Start with an empty list of bindings
    env_obj->data.env.outer = outer_env_obj;
    // env_obj->marked is set to false by sl_allocate_object

    return env_obj;
}

// sl_env_destroy is removed as environments are now GC'd sl_objects

void sl_env_define(sl_object *env_obj, sl_object *symbol, sl_object *value) {
    if (!sl_is_env(env_obj) || !sl_is_symbol(symbol)) {
        fprintf(stderr, "Error (define): Invalid environment object or symbol.\n");
        return;
    }

    // Check if the symbol already exists *in this specific frame* using strcmp
    sl_object *current_binding = sl_env_bindings(env_obj);
    while (sl_is_pair(current_binding)) {
        sl_object *pair = sl_car(current_binding);
        if (sl_is_pair(pair) && sl_is_symbol(sl_car(pair)) &&
            strcmp(sl_symbol_name(sl_car(pair)), sl_symbol_name(symbol)) == 0) {
            // Found existing binding in this frame, update it
            sl_set_cdr(pair, value);
            return;
        }
        current_binding = sl_cdr(current_binding);
    }

    // Not found in this frame, create a new binding pair and prepend it
    sl_object *new_binding_pair = sl_make_pair(symbol, value);
    sl_set_env_bindings(env_obj, sl_make_pair(new_binding_pair, sl_env_bindings(env_obj)));
}

// Update sl_env_set to use strcmp
bool sl_env_set(sl_object *env_obj, sl_object *symbol, sl_object *value) {
    if (!sl_is_symbol(symbol)) {
        fprintf(stderr, "Error (set!): Invalid symbol.\n");
        return false;
    }

    sl_object *current_env_obj = env_obj;
    while (sl_is_env(current_env_obj)) {
        sl_object *current_binding = sl_env_bindings(current_env_obj);
        while (sl_is_pair(current_binding)) {
            sl_object *pair = sl_car(current_binding);
            // Use strcmp for comparison
            if (sl_is_pair(pair) && sl_is_symbol(sl_car(pair)) &&
                strcmp(sl_symbol_name(sl_car(pair)), sl_symbol_name(symbol)) == 0) {
                sl_set_cdr(pair, value);
                return true;
            }
            current_binding = sl_cdr(current_binding);
        }
        current_env_obj = sl_env_outer(current_env_obj);
    }
    // If current_env_obj became SL_NIL, we reached the end without finding it.
    if (current_env_obj != SL_NIL) {
        fprintf(stderr, "Error (set!): Invalid environment structure encountered.\n");
        return false;
    }

    // Symbol not found in any environment
    fprintf(stderr, "Error (set!): Unbound variable '%s'.\n", sl_symbol_name(symbol));
    return false;
}

// Update sl_env_lookup to use strcmp
sl_object *sl_env_lookup(sl_object *env_obj, sl_object *symbol) {
    if (!sl_is_symbol(symbol)) {
        fprintf(stderr, "Error (lookup): Invalid symbol.\n");
        return NULL;  // Indicate not found
    }

    sl_object *current_env_obj = env_obj;
    while (sl_is_env(current_env_obj)) {
        sl_object *current_binding = sl_env_bindings(current_env_obj);
        while (sl_is_pair(current_binding)) {
            sl_object *pair = sl_car(current_binding);
            // Use strcmp for comparison
            if (sl_is_pair(pair) && sl_is_symbol(sl_car(pair)) &&
                strcmp(sl_symbol_name(sl_car(pair)), sl_symbol_name(symbol)) == 0) {
                // Found the binding, return the value (cdr of the inner pair)
                return sl_cdr(pair);
            }
            current_binding = sl_cdr(current_binding);
        }
        current_env_obj = sl_env_outer(current_env_obj);
    }
    // If current_env_obj became SL_NIL, we reached the end without finding it.
    if (current_env_obj != SL_NIL) {
        fprintf(stderr, "Error (lookup): Invalid environment structure encountered.\n");
        return NULL;
    }

    // Symbol not found in any environment
    // fprintf(stderr, "Warning (lookup): Unbound variable '%s'.\n", sl_symbol_name(symbol));
    return NULL;  // Indicate not found
}

// sl_gc_mark_env is removed as its logic is now in sl_gc_mark