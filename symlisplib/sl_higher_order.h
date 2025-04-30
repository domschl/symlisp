#ifndef SL_HIGHER_ORDER_H
#define SL_HIGHER_ORDER_H

#include "sl_core.h"
#include "sl_env.h"

// Function to register higher-order primitives like map, filter, etc.
void sl_register_higher_order_primitives(sl_object *env);

#endif  // SL_HIGHER_ORDER_H