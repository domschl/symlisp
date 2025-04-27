#ifndef SL_ENV_H
#define SL_ENV_H

#include "sl_core.h"  // For sl_object definition

// --- Environment Operations ---

/**
 * @brief Creates a new environment object enclosed by an outer environment object.
 * Allocates the environment object from the SymLisp heap.
 *
 * @param outer_env_obj The enclosing environment object (can be SL_NIL for the global scope).
 * @return sl_object* A pointer to the newly created environment object (SL_TYPE_ENV),
 *                  or SL_NIL on allocation failure.
 */
sl_object *sl_env_create(sl_object *outer_env_obj);

/**
 * @brief Defines a variable in the *current* environment frame.
 * If the symbol already exists in this frame, its value is updated.
 *
 * @param env_obj The environment object (must be SL_TYPE_ENV) where the definition occurs.
 * @param symbol The symbol to define (must be an SL_TYPE_SYMBOL object).
 * @param value The value to bind to the symbol.
 */
void sl_env_define(sl_object *env_obj, sl_object *symbol, sl_object *value);

/**
 * @brief Sets the value of an *existing* variable in the environment chain.
 * Searches the current environment object and then outwards. Errors if the symbol is not found.
 *
 * @param env_obj The starting environment object (must be SL_TYPE_ENV) for the search.
 * @param symbol The symbol whose value needs to be set (must be an SL_TYPE_SYMBOL object).
 * @param value The new value to assign.
 * @return bool True if the variable was found and set, false otherwise (error printed).
 */
bool sl_env_set(sl_object *env_obj, sl_object *symbol, sl_object *value);

/**
 * @brief Looks up the value bound to a symbol in the environment chain.
 * Searches the current environment object and then outwards.
 *
 * @param env_obj The starting environment object (must be SL_TYPE_ENV) for the lookup.
 * @param symbol The symbol to look up (must be an SL_TYPE_SYMBOL object).
 * @return sl_object* The value bound to the symbol, or NULL if the symbol is not found.
 */
sl_object *sl_env_lookup(sl_object *env_obj, sl_object *symbol);

#endif  // SL_ENV_H