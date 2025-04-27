#ifndef SL_BUILTINS_H
#define SL_BUILTINS_H

#include "sl_core.h"  // For sl_object
#include "sl_env.h"   // For sl_env_define

/**
 * @brief Initializes all built-in procedures and defines them
 *        in the provided global environment.
 *
 * @param global_env The global environment object (SL_TYPE_ENV).
 */
void sl_builtins_init(sl_object *global_env);

#endif  // SL_BUILTINS_H