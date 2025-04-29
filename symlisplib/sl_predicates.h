#ifndef SL_PREDICATES_H
#define SL_PREDICATES_H

#include "sl_core.h"
#include "sl_env.h"

/**
 * @brief Initializes and registers all predicate built-in functions.
 *
 * @param global_env The global environment to define the predicates in.
 */
void sl_predicates_init(sl_object *global_env);

#endif  // SL_PREDICATES_H
