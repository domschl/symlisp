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
sl_object *check_arity_range(const char *func_name, sl_object *args, size_t min_expected, size_t max_expected);
void define_builtin(sl_object *env, const char *name, sl_builtin_func_ptr func_ptr);
bool get_number_as_int64(sl_object *obj, int64_t *out, const char *func_name);
bool get_number_as_mpq(sl_object *obj, mpq_t out, const char *func_name);
sl_object *append_to_list(sl_object **head_root, sl_object **tail_root, sl_object *item);

#endif  // SL_BUILTINS_H