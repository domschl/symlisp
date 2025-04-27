#ifndef SL_EVAL_H
#define SL_EVAL_H

#include "sl_core.h"  // For sl_object
#include "sl_env.h"   // For environment operations (though env is sl_object*)

/**
 * @brief Evaluates a SymLisp expression within a given environment.
 *
 * @param expr The sl_object representing the expression to evaluate.
 * @param env_obj The environment object (SL_TYPE_ENV) in which to evaluate.
 * @return sl_object* The result of the evaluation, or an SL_TYPE_ERROR object
 *                  if an evaluation error occurs. Returns SL_NIL on allocation
 *                  failure during error object creation.
 */
sl_object *sl_eval(sl_object *expr, sl_object *env_obj);

#endif  // SL_EVAL_H