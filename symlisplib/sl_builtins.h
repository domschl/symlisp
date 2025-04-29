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
sl_object *check_arity(const char *func_name, sl_object *args, size_t expected);
void define_builtin(sl_object *env, const char *name, sl_builtin_func_ptr func_ptr);
bool get_number_as_int64(sl_object *obj, int64_t *out, const char *func_name);

#endif  // SL_BUILTINS_H