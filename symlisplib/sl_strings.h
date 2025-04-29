#ifndef SL_STRINGS_H
#define SL_STRINGS_H

#include "sl_core.h"
#include "sl_env.h"

/**
 * @brief Initializes and registers all string built-in functions.
 *
 * @param global_env The global environment to define the functions in.
 */
void sl_strings_init(sl_object *global_env);

#endif  // SL_STRINGS_H