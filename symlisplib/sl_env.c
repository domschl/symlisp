#include "sl_env.h"
#include "sl_core.h"  // Access to sl_make_pair, sl_car, sl_cdr, etc.
#include <stdio.h>
#include <stdlib.h>  // For malloc, free (no longer needed for env struct)
#include <string.h>  // For strcmp

// --- Environment Operations ---

sl_object *sl_env_create(sl_object *outer_env_obj) {
    // Ensure outer is NULL (from C), SL_NIL (from Lisp), or a valid ENV object
    if (outer_env_obj != NULL && outer_env_obj != SL_NIL && !sl_is_env(outer_env_obj)) {  // <<< CORRECTED CHECK
        fprintf(stderr, "Error (env_create): Outer environment must be NULL, SL_NIL, or an ENV object.\n");
        return SL_NIL;  // Indicate error
    }

    sl_object *env_obj = sl_allocate_object();
    if (!env_obj) {
        // Allocation failure already printed by sl_allocate_object or sl_gc
        // Return SL_OUT_OF_MEMORY_ERROR instead of SL_NIL for consistency
        return SL_OUT_OF_MEMORY_ERROR;
    }
    env_obj->type = SL_TYPE_ENV;
    env_obj->data.env.bindings = SL_NIL;  // Start with an empty list of bindings
    // Store NULL or SL_NIL if passed, otherwise the valid outer env
    env_obj->data.env.outer = outer_env_obj;
    env_obj->data.env.macros = SL_NIL;  // <<< ADDED: Initialize macros list

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
    if (!new_binding_pair || new_binding_pair == SL_OUT_OF_MEMORY_ERROR) {
        // Handle allocation failure for the inner pair
        fprintf(stderr, "Error (define): Failed to allocate binding pair.\n");
        // Potentially return an error object or signal failure differently
        return;
    }

    // --- FIX: Root the intermediate pair before the next allocation ---
    SL_GC_ADD_ROOT(&new_binding_pair);

    sl_object *new_bindings_head = sl_make_pair(new_binding_pair, sl_env_bindings(env_obj));

    // --- FIX: Unroot the intermediate pair ---
    SL_GC_REMOVE_ROOT(&new_binding_pair);

    if (!new_bindings_head || new_bindings_head == SL_OUT_OF_MEMORY_ERROR) {
        // Handle allocation failure for the outer pair
        fprintf(stderr, "Error (define): Failed to allocate updated bindings list head.\n");
        // The inner new_binding_pair might leak here until next GC if not handled
        return;
    }

    // Update the environment's bindings list
    sl_set_env_bindings(env_obj, new_bindings_head);
}

// DEBUG: Function to dump the bindings of an environment
void sl_env_dump(sl_object *env_obj, const char *label) {
    if (!env_obj || !sl_is_env(env_obj)) {
        printf("[DEBUG ENV DUMP - %s] Invalid environment object provided.\n", label);
        return;
    }
    printf("[DEBUG ENV DUMP - %s] Env: %p, Outer: %p, Bindings List: %p\n",
           label, (void *)env_obj, (void *)env_obj->data.env.outer, (void *)env_obj->data.env.bindings);

    sl_object *current_binding_node = env_obj->data.env.bindings;
    int count = 0;
    while (sl_is_pair(current_binding_node)) {
        sl_object *pair = sl_car(current_binding_node);
        if (sl_is_pair(pair)) {
            sl_object *sym = sl_car(pair);
            sl_object *val = sl_cdr(pair);  // Value object
            const char *sym_name = sl_is_symbol(sym) ? sym->data.symbol_name : "INVALID_SYM";
            printf("    [%d] Node: %p, Pair: %p, Sym: %s (%p), Val: %p (Type %d), NextNode: %p\n",
                   count,
                   (void *)current_binding_node,
                   (void *)pair,
                   sym_name,
                   (void *)sym,
                   (void *)val,
                   val ? val->type : -1,  // Print value type
                   (void *)sl_cdr(current_binding_node));

            // Check specifically for set!
            if (sl_is_symbol(sym) && strcmp(sym_name, "set!") == 0) {
                printf("        >>> Found 'set!' binding <<<\n");
            }

        } else {
            printf("    [%d] Node: %p, Malformed binding (car is not a pair): %p\n",
                   count, (void *)current_binding_node, (void *)pair);
            break;  // Stop if malformed
        }
        current_binding_node = sl_cdr(current_binding_node);
        count++;
        if (count > 1000) {  // Safety break for potential cycles
            printf("    Stopping dump after 1000 bindings (potential cycle?).\n");
            break;
        }
    }
    if (current_binding_node != SL_NIL) {
        printf("    Bindings list terminated improperly with: %p (Type %d)\n",
               (void *)current_binding_node, current_binding_node ? current_binding_node->type : -1);
    } else {
        printf("    Bindings list terminated properly with NIL.\n");
    }
    printf("[DEBUG ENV DUMP - %s] End Dump.\n", label);
}

// Sets the value of an existing variable in the nearest environment where it's defined.
bool sl_env_set(sl_object *env_obj, sl_object *symbol, sl_object *value) {
    if (!sl_is_symbol(symbol)) {
        fprintf(stderr, "Error (set!): Target must be a symbol.\n");
        return false;  // Indicate error
    }
    const char *target_name = sl_symbol_name(symbol);  // Get name once

    sl_object *current_env_obj = env_obj;
    while (current_env_obj != NULL && current_env_obj != SL_NIL) {
        if (!sl_is_env(current_env_obj)) {
            fprintf(stderr, "Error (set!): Invalid environment structure encountered.\n");
            return false;  // Indicate error
        }

        sl_object *bindings = current_env_obj->data.env.bindings;
        sl_object *current_binding = bindings;
        while (current_binding != SL_NIL) {
            if (!sl_is_pair(current_binding)) break;  // Malformed env protection
            sl_object *pair = sl_car(current_binding);
            if (!sl_is_pair(pair)) break;  // Malformed env protection
            sl_object *current_sym = sl_car(pair);

            // --- CORRECTED COMPARISON ---
            // if (sl_car(pair) == symbol) { // <<< OLD POINTER COMPARISON
            if (sl_is_symbol(current_sym) && strcmp(sl_symbol_name(current_sym), target_name) == 0) {  // <<< NAME COMPARISON                                                                                                       // Found the symbol in this frame, update the value
                sl_set_cdr(pair, value);                                                               // Assumes sl_set_cdr handles GC if needed
                return true;                                                                           // Success
            }
            current_binding = sl_cdr(current_binding);
        }
        // Not found in current frame, move to outer
        current_env_obj = current_env_obj->data.env.outer;
    }

    // Symbol not found in any environment
    fprintf(stderr, "Error (set!): Unbound variable '%s'.\n", target_name);
    return false;  // Indicate error
}

// Looks up a variable's value, searching outwards from the given environment.
sl_object *sl_env_lookup(sl_object *env_obj, sl_object *symbol) {
    if (!sl_is_symbol(symbol)) {
        fprintf(stderr, "Internal Error (lookup): Target must be a symbol.\n");
        return SL_NIL;  // Or an error object
    }
    const char *target_name = sl_symbol_name(symbol);  // Get name once

    sl_object *current_env_obj = env_obj;
    while (current_env_obj != NULL && current_env_obj != SL_NIL) {
        if (!sl_is_env(current_env_obj)) {
            fprintf(stderr, "Error (lookup): Invalid environment structure encountered.\n");
            return SL_NIL;
        }

        sl_object *bindings = current_env_obj->data.env.bindings;  // Get the list of pairs: ((sym1 . val1) (sym2 . val2) ...)
        sl_object *current_binding_node = bindings;
        while (sl_is_pair(current_binding_node)) {           // Iterate through the list of pairs
            sl_object *pair = sl_car(current_binding_node);  // Get the (sym . val) pair
            if (sl_is_pair(pair)) {                          // Check if it's actually a pair
                sl_object *current_sym = sl_car(pair);       // Get the symbol from the pair

                // --- CORRECTED COMPARISON ---
                if (sl_is_symbol(current_sym) && strcmp(sl_symbol_name(current_sym), target_name) == 0) {  // <<< NAME COMPARISON
                    // Found the symbol, return the binding pair itself
                    return pair;  // <<< RETURN THE PAIR
                }
            } else {
                // Malformed binding list, stop searching this frame
                fprintf(stderr, "Warning (lookup): Malformed binding list encountered in env %p.\n", (void *)current_env_obj);
                break;
            }
            current_binding_node = sl_cdr(current_binding_node);  // Move to the next node in the list of pairs
        }
        // Not found in current frame, move to outer
        current_env_obj = current_env_obj->data.env.outer;
    }

    // Symbol not found in any environment up to NULL/SL_NIL
    return SL_NIL;  // Indicate not found
}

// Defines a macro in the given environment.
// Overwrites if keyword_sym already exists as a macro in this frame.
void sl_env_define_macro(sl_object *env_obj, sl_object *keyword_sym, sl_object *transformer_proc) {
    if (!sl_is_env(env_obj) || !sl_is_symbol(keyword_sym) || !sl_is_function(transformer_proc)) {
        fprintf(stderr, "Error (define-macro): Invalid environment, keyword, or transformer procedure.\n");
        // Consider returning an error object or signaling failure differently if this can be user-triggered.
        return;
    }

    // Check if the macro keyword already exists *in this specific frame*
    sl_object *current_macro_binding = env_obj->data.env.macros;
    while (sl_is_pair(current_macro_binding)) {
        sl_object *pair = sl_car(current_macro_binding);  // (keyword . transformer)
        if (sl_is_pair(pair) && sl_is_symbol(sl_car(pair)) &&
            strcmp(sl_symbol_name(sl_car(pair)), sl_symbol_name(keyword_sym)) == 0) {
            // Found existing macro in this frame, update its transformer
            sl_set_cdr(pair, transformer_proc);
            return;
        }
        current_macro_binding = sl_cdr(current_macro_binding);
    }

    // Not found in this frame, create a new macro binding pair and prepend it
    sl_object *new_macro_pair = sl_make_pair(keyword_sym, transformer_proc);
    if (!new_macro_pair || new_macro_pair == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Error (define-macro): Failed to allocate macro binding pair.\n");
        return;
    }

    SL_GC_ADD_ROOT(&new_macro_pair);  // Protect before next allocation

    sl_object *new_macros_head = sl_make_pair(new_macro_pair, env_obj->data.env.macros);

    SL_GC_REMOVE_ROOT(&new_macro_pair);  // Unroot

    if (!new_macros_head || new_macros_head == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Error (define-macro): Failed to allocate updated macros list head.\n");
        // The inner new_macro_pair might leak here until next GC if not handled
        return;
    }

    // Update the environment's macros list
    env_obj->data.env.macros = new_macros_head;
}

// Looks up a macro transformer, searching outwards from the given environment.
// Returns the transformer procedure object or SL_NIL if not found.
sl_object *sl_env_lookup_macro(sl_object *env_obj, sl_object *keyword_sym) {
    if (!sl_is_symbol(keyword_sym)) {
        // This should ideally not happen if called correctly
        fprintf(stderr, "Internal Error (lookup-macro): Target must be a symbol.\n");
        return SL_NIL;
    }
    const char *target_name = sl_symbol_name(keyword_sym);

    sl_object *current_env_obj = env_obj;
    while (current_env_obj != NULL && current_env_obj != SL_NIL) {
        if (!sl_is_env(current_env_obj)) {
            fprintf(stderr, "Error (lookup-macro): Invalid environment structure encountered.\n");
            return SL_NIL;  // Or an error object
        }

        sl_object *current_macro_binding_node = current_env_obj->data.env.macros;
        while (sl_is_pair(current_macro_binding_node)) {
            sl_object *pair = sl_car(current_macro_binding_node);  // (keyword . transformer)
            if (sl_is_pair(pair)) {
                sl_object *current_keyword_sym = sl_car(pair);
                if (sl_is_symbol(current_keyword_sym) &&
                    strcmp(sl_symbol_name(current_keyword_sym), target_name) == 0) {
                    return sl_cdr(pair);  // Return the transformer procedure
                }
            } else {
                fprintf(stderr, "Warning (lookup-macro): Malformed macro binding list in env %p.\n", (void *)current_env_obj);
                break;  // Stop searching this malformed frame
            }
            current_macro_binding_node = sl_cdr(current_macro_binding_node);
        }
        current_env_obj = current_env_obj->data.env.outer;  // Move to outer environment
    }
    return SL_NIL;  // Macro not found
}

// --- Environment API for C Clients ---

sl_object *sl_env_get_value(sl_object *env_obj, const char *var_name) {
    if (!env_obj || !var_name) return NULL;

    // Create a temporary symbol. This is inefficient without interning.
    // The created symbol might leak if not GC'd properly before next use,
    // but standard GC should handle it eventually if it becomes unreachable.
    // Proper interning is the real solution.
    sl_object *temp_symbol = sl_make_symbol(var_name);
    if (!temp_symbol) {
        fprintf(stderr, "API Error (sl_env_get_value): Failed to create temporary symbol for '%s'.\n", var_name);
        return NULL;  // Allocation failure
    }

    sl_object *value = sl_env_lookup(env_obj, temp_symbol);

    // Note: temp_symbol is now potentially garbage if not used elsewhere.
    // GC will eventually collect it.

    return value;  // Returns NULL if lookup failed
}

bool sl_env_set_value(sl_object *env_obj, const char *var_name, sl_object *value) {
    if (!env_obj || !var_name) return false;

    sl_object *temp_symbol = sl_make_symbol(var_name);
    if (!temp_symbol) {
        fprintf(stderr, "API Error (sl_env_set_value): Failed to create temporary symbol for '%s'.\n", var_name);
        return false;  // Allocation failure
    }

    bool success = sl_env_set(env_obj, temp_symbol, value);

    // temp_symbol potentially garbage

    return success;  // Returns false if set failed (variable not found)
}

void sl_env_define_value(sl_object *env_obj, const char *var_name, sl_object *value) {
    if (!env_obj || !var_name) {
        fprintf(stderr, "API Error (sl_env_define_value): Invalid environment or variable name.\n");
        return;
    }

    sl_object *temp_symbol = sl_make_symbol(var_name);
    if (!temp_symbol) {
        fprintf(stderr, "API Error (sl_env_define_value): Failed to create temporary symbol for '%s'.\n", var_name);
        // Cannot proceed without symbol
        return;
    }

    sl_env_define(env_obj, temp_symbol, value);

    // temp_symbol potentially garbage, but now held by the environment bindings
}