#ifndef SL_ENV_H
#define SL_ENV_H

#include "sl_core.h"
#include <stdbool.h>

// --- Environment Operations ---

/** @brief Creates a new environment enclosed by an outer environment. */
sl_object *sl_env_create(sl_object *outer_env);

/** @brief Defines a variable in the specified environment frame. Overwrites if exists in *this* frame. */
void sl_env_define(sl_object *env_obj, sl_object *symbol, sl_object *value);

/** @brief Sets the value of an existing variable in the nearest environment where it's defined. */
bool sl_env_set(sl_object *env_obj, sl_object *symbol, sl_object *value);

/** @brief Looks up a variable's value, searching outwards from the given environment. */
sl_object *sl_env_lookup(sl_object *env_obj, sl_object *symbol);

// --- Environment API for C Clients ---

/**
 * @brief Looks up a variable by name in an environment chain.
 * @param env_obj The starting environment object.
 * @param var_name The C string name of the variable.
 * @return The sl_object* value if found, otherwise NULL.
 *         Returns NULL also on internal allocation error for the temporary symbol.
 */
sl_object *sl_env_get_value(sl_object *env_obj, const char *var_name);

/**
 * @brief Sets the value of an existing variable by name in an environment chain.
 * @param env_obj The starting environment object.
 * @param var_name The C string name of the variable.
 * @param value The sl_object* value to set.
 * @return true if the variable was found and set, false otherwise (not found or internal error).
 */
bool sl_env_set_value(sl_object *env_obj, const char *var_name, sl_object *value);

/**
 * @brief Defines a variable by name in a specific environment frame.
 * @param env_obj The environment object where the definition should occur.
 * @param var_name The C string name of the variable.
 * @param value The sl_object* value to define.
 * @note This does not check outer environments; it defines directly in env_obj.
 *       Potential internal allocation errors are not explicitly returned but might cause issues.
 */
void sl_env_define_value(sl_object *env_obj, const char *var_name, sl_object *value);

#endif  // SL_ENV_H