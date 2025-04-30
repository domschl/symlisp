#ifndef SL_EVAL_H
#define SL_EVAL_H

#include "sl_core.h"
#include "sl_env.h"  // Assuming sl_object* is used for env
#include <stdio.h>   // For FILE*

// Evaluates a single S-expression in the given environment.
sl_object *sl_eval(sl_object *obj, sl_object *env);
sl_object *sl_apply(sl_object *fn, sl_object *args, sl_object **obj_ptr, sl_object **env_ptr);

// Parses and evaluates all S-expressions from the given null-terminated string
// in the specified environment.
// Returns the result of the *last* expression evaluated, or an error object
// if any parsing or evaluation error occurs.
// Returns SL_NIL if the string is empty or contains only whitespace/comments.
sl_object *sl_eval_string(const char *input, sl_object *env);  // <<< NEW

// Reads and evaluates all S-expressions from a stream in the given environment.
// (Implementation might read stream to string first, then call sl_eval_string)
sl_object *sl_eval_stream(FILE *stream, sl_object *env);

// Loads all files ending in ".scm" from the specified directory path.
// Returns SL_TRUE on success, or an error object on failure.
sl_object *sl_load_directory(const char *dir_path, sl_object *env);  // <<< NEW

#endif  // SL_EVAL_H