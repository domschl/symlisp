#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "sl_core.h"  // Now includes CHECK_ALLOC
#include "sl_env.h"
#include "sl_eval.h"
#include "sl_builtins.h"

// --- Forward Declarations ---
sl_object *sl_eval_list(sl_object *list, sl_object *env);
sl_object *sl_apply(sl_object *fn, sl_object *args);

// Helper function to get symbol name safely
static const char *safe_symbol_name(sl_object *obj) {
    if (sl_is_symbol(obj)) {
        return sl_symbol_name(obj);
    }
    return "invalid-symbol";
}

sl_object *sl_eval(sl_object *obj_in, sl_object *env_in) {
    sl_object *obj = obj_in;  // Use local vars that can be rooted
    sl_object *env = env_in;
    sl_object *result = SL_NIL;  // Default result

    // --- Root key local variables ---
    // Any sl_object* here that might hold a value across a call
    // to sl_eval, sl_apply, sl_make_xxx, etc. needs rooting.
    sl_gc_add_root(&obj);
    sl_gc_add_root(&env);
    sl_gc_add_root(&result);
// Add more roots here for other important temporary sl_object* if needed

// Use a label for cleanup to ensure roots are removed on all paths
top_of_eval:;
    if (!obj) {  // Handle NULL input gracefully
        result = sl_make_errorf("Eval: NULL object");
        goto cleanup;
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
    case SL_TYPE_BOOLEAN:
    case SL_TYPE_NUMBER:
    case SL_TYPE_STRING:
    case SL_TYPE_ERROR:  // Errors evaluate to themselves
        result = obj;    // Self-evaluating
        break;

    case SL_TYPE_SYMBOL:
        result = sl_env_lookup(env, obj);  // Use lookup
        if (!result) {
            result = sl_make_errorf("Eval: Unbound symbol '%s'", safe_symbol_name(obj));
        }
        break;

    case SL_TYPE_PAIR: {
        sl_object *op_obj = sl_car(obj);
        sl_object *args = sl_cdr(obj);

        if (sl_is_symbol(op_obj)) {
            const char *op_name = sl_symbol_name(op_obj);

            // --- QUOTE ---
            if (strcmp(op_name, "quote") == 0) {
                sl_gc_add_root(&args);  // Root args for safety
                if (args == SL_NIL || sl_cdr(args) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed quote");
                } else {
                    result = sl_car(args);  // Don't eval args of quote
                }
                sl_gc_remove_root(&args);
                break;  // Break from switch (handled special form)
            }
            // --- IF ---
            else if (strcmp(op_name, "if") == 0) {
                // Root args temporarily
                sl_gc_add_root(&args);
                // (if test conseq alt)
                sl_object *arg1 = sl_car(args);
                sl_object *arg2_pair = sl_cdr(args);
                sl_object *arg3_pair = sl_is_pair(arg2_pair) ? sl_cdr(arg2_pair) : SL_NIL;

                sl_object *test_expr = arg1;
                sl_object *conseq_expr = sl_is_pair(arg2_pair) ? sl_car(arg2_pair) : SL_NIL;
                sl_object *alt_expr = sl_is_pair(arg3_pair) ? sl_car(arg3_pair) : SL_NIL;

                // Check structure
                if (test_expr == SL_NIL || conseq_expr == SL_NIL || !sl_is_pair(arg2_pair) || (sl_is_pair(arg3_pair) && sl_cdr(arg3_pair) != SL_NIL)) {
                    result = sl_make_errorf("Eval: Malformed if structure");
                    sl_gc_remove_root(&args);
                    break;  // Break from switch
                }
                if (!sl_is_pair(arg3_pair)) { alt_expr = SL_NIL; }

                sl_gc_remove_root(&args);  // Unroot args before eval

                // Root expressions needed after test_result is evaluated
                sl_gc_add_root(&conseq_expr);
                sl_gc_add_root(&alt_expr);

                sl_object *test_result = sl_eval(test_expr, env);  // Eval test - MIGHT GC
                sl_gc_add_root(&test_result);

                if (test_result == SL_OUT_OF_MEMORY_ERROR) {
                    result = test_result;                                       // Propagate error
                } else if (test_result != SL_FALSE && test_result != SL_NIL) {  // Truthy?
                    obj = conseq_expr;                                          // Tail call conseq
                } else {
                    obj = alt_expr;  // Tail call alt
                }
                // Unroot temporaries before tail call
                sl_gc_remove_root(&test_result);
                sl_gc_remove_root(&alt_expr);
                sl_gc_remove_root(&conseq_expr);
                goto top_of_eval;  // Tail call optimization
            }
            // --- DEFINE ---
            else if (strcmp(op_name, "define") == 0) {
                sl_gc_add_root(&args);
                // (define symbol value) or (define (fn params...) body...)
                if (args == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed define (no symbol/value)");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *target = sl_car(args);
                sl_object *value_expr_pair = sl_cdr(args);  // This is the body list for functions

                if (sl_is_pair(target)) {  // Function definition
                    sl_object *fn_name_sym = sl_car(target);
                    sl_object *params = sl_cdr(target);
                    sl_object *body_list = value_expr_pair;  // <<< Use the whole list

                    if (!sl_is_symbol(fn_name_sym)) {
                        result = sl_make_errorf("Eval: Function name in define must be a symbol");
                        sl_gc_remove_root(&args);
                        break;
                    }
                    // TODO: Add validation for params list structure
                    // TODO: Handle multiple body expressions (implicit begin)

                    // Store the *list* of body expressions in the closure
                    sl_object *lambda = sl_make_closure(params, body_list, env);  // <<< Pass body_list
                    CHECK_ALLOC(lambda);
                    sl_env_define(env, fn_name_sym, lambda);
                    result = fn_name_sym;

                } else if (sl_is_symbol(target)) {  // Variable definition
                    if (value_expr_pair == SL_NIL || sl_cdr(value_expr_pair) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed define (wrong number of args for variable)");
                        sl_gc_remove_root(&args);
                        break;
                    }
                    sl_object *value_expr = sl_car(value_expr_pair);

                    // Unroot op_obj and args before eval
                    sl_gc_remove_root(&op_obj);
                    sl_gc_remove_root(&args);

                    sl_object *value = sl_eval(value_expr, env);
                    sl_gc_add_root(&value);

                    if (value == SL_OUT_OF_MEMORY_ERROR) {
                        result = value;
                    } else {
                        // sl_env_define returns void
                        sl_env_define(env, target, value);
                        // Check for OOM? See comment above.
                        result = target;  // Return the symbol
                    }
                    sl_gc_remove_root(&value);

                } else {
                    result = sl_make_errorf("Eval: Invalid target for define");
                }
                sl_gc_remove_root(&args);
                break;
            }
            // --- SET! --- <<< ADDED BLOCK
            else if (strcmp(op_name, "set!") == 0) {
                sl_gc_add_root(&args);  // Root args
                // (set! symbol value)
                if (args == SL_NIL || !sl_is_pair(args) || sl_cdr(args) == SL_NIL || !sl_is_pair(sl_cdr(args)) || sl_cdr(sl_cdr(args)) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed set!");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *target_sym = sl_car(args);
                sl_object *value_expr = sl_cadr(args);  // Use helper or sl_car(sl_cdr(args))

                if (!sl_is_symbol(target_sym)) {
                    result = sl_make_errorf("Eval: Target for set! must be a symbol");
                    sl_gc_remove_root(&args);
                    break;
                }

                sl_gc_remove_root(&args);  // Unroot args before eval

                sl_object *value = sl_eval(value_expr, env);  // Eval value - MIGHT GC
                sl_gc_add_root(&value);                       // Root result

                if (value == SL_OUT_OF_MEMORY_ERROR) {
                    result = value;  // Propagate error
                } else {
                    // sl_env_set returns bool indicating if symbol was found and set
                    if (!sl_env_set(env, target_sym, value)) {
                        result = sl_make_errorf("Eval: Cannot set! unbound symbol '%s'", sl_symbol_name(target_sym));
                    } else {
                        result = value;  // set! usually returns the value
                    }
                }
                sl_gc_remove_root(&value);  // Unroot value
                break;                      // Break from switch
            }
            // --- LAMBDA ---
            else if (strcmp(op_name, "lambda") == 0) {
                sl_gc_add_root(&args);  // Root args
                // (lambda (params...) body...)
                if (args == SL_NIL || sl_cdr(args) == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed lambda");
                    sl_gc_remove_root(&args);
                    break;
                }
                sl_object *params = sl_car(args);
                sl_object *body = sl_cdr(args);

                result = sl_make_closure(params, sl_car(body), env);
                CHECK_ALLOC(result);
                sl_gc_remove_root(&args);
                break;  // Break from switch
            }
            // --- END OF SPECIAL FORMS ---
            // If none of the above matched, it's not a special form we handle here.
            // Fall through to the function call logic below.

        }  // end if (sl_is_symbol(op_obj))

        // --- Generic Function Call ---
        // If op_obj wasn't a symbol or wasn't a recognized special form symbol.
        sl_gc_add_root(&op_obj);  // Root operator object itself
        sl_gc_add_root(&args);    // Root arguments list

        sl_object *fn = sl_eval(op_obj, env);  // Evaluate the operator - MIGHT GC
        sl_gc_add_root(&fn);                   // Root the resulting function object

        if (fn == SL_OUT_OF_MEMORY_ERROR) {
            result = fn;
            goto cleanup_fn_call;
        }
        if (!sl_is_function(fn)) {
            char *op_str = sl_object_to_string(op_obj);
            result = sl_make_errorf("Eval: Not a function: %s", op_str ? op_str : "<?>");
            free(op_str);
            goto cleanup_fn_call;
        }

        // Evaluate arguments
        sl_object *evaled_args = sl_eval_list(args, env);  // Eval list - MIGHT GC
        sl_gc_add_root(&evaled_args);                      // Root the evaluated argument list

        if (evaled_args == SL_OUT_OF_MEMORY_ERROR) {
            result = evaled_args;
            goto cleanup_args_call;
        }

        // Apply the function
        result = sl_apply(fn, evaled_args);  // Apply - MIGHT GC

    cleanup_args_call:
        sl_gc_remove_root(&evaled_args);
    cleanup_fn_call:
        sl_gc_remove_root(&fn);
        // Also unroot op_obj and args used in this path
        sl_gc_remove_root(&op_obj);
        sl_gc_remove_root(&args);
        break;  // Break from switch case SL_TYPE_PAIR

    }  // end case SL_TYPE_PAIR

    default:
        result = sl_make_errorf("Eval: Unknown object type %d", obj->type);
        break;

    }  // end switch

cleanup:
    // --- Unroot locals before returning ---
    sl_gc_remove_root(&result);
    sl_gc_remove_root(&env);
    sl_gc_remove_root(&obj);
    return result;
}

// Helper to evaluate a list of arguments
sl_object *sl_eval_list(sl_object *list, sl_object *env) {
    if (list == SL_NIL) {
        return SL_NIL;
    }
    if (!sl_is_pair(list)) {
        // Allow evaluating dotted lists? For now, require proper list.
        return sl_make_errorf("Eval: Invalid argument list structure (not a pair)");
    }

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    sl_object *current_expr = list;
    sl_object *return_value = SL_NIL;  // Variable to hold final return value (list head or error)

    // --- Root key variables ---
    sl_gc_add_root(&head);
    sl_gc_add_root(&env);
    sl_gc_add_root(&current_expr);
    // Do NOT root return_value itself, it just holds the final pointer

    while (current_expr != SL_NIL) {
        if (!sl_is_pair(current_expr)) {
            // Set error object to be returned
            return_value = sl_make_errorf("Eval: Improper argument list (dotted list?)");
            goto cleanup_eval_list;
        }
        sl_object *arg_expr = sl_car(current_expr);
        sl_object *evaled_arg = sl_eval(arg_expr, env);
        sl_gc_add_root(&evaled_arg);

        if (evaled_arg == SL_OUT_OF_MEMORY_ERROR) {
            return_value = evaled_arg;  // Set error object
            sl_gc_remove_root(&evaled_arg);
            goto cleanup_eval_list;
        }

        sl_object *new_pair = sl_make_pair(evaled_arg, SL_NIL);

        if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
            return_value = new_pair;  // Set error object
            sl_gc_remove_root(&evaled_arg);
            goto cleanup_eval_list;
        }

        *tail_ptr = new_pair;
        tail_ptr = &new_pair->data.pair.cdr;

        sl_gc_remove_root(&evaled_arg);
        current_expr = sl_cdr(current_expr);
    }

    return_value = head;  // Success: return the head of the built list

cleanup_eval_list:
    sl_gc_remove_root(&current_expr);
    sl_gc_remove_root(&env);
    sl_gc_remove_root(&head);
    return return_value;  // Return either the list head or an error object
}

sl_object *sl_apply(sl_object *fn, sl_object *args) {
    if (!sl_is_function(fn)) {
        return sl_make_errorf("Apply: Not a function");
    }
    // We assume args is a proper list as constructed by sl_eval_list

    sl_object *result = SL_NIL;
    // --- Root key variables ---
    sl_gc_add_root(&fn);
    sl_gc_add_root(&args);
    sl_gc_add_root(&result);

    if (fn->data.function.is_builtin) {
        result = fn->data.function.def.builtin.func_ptr(args);
    } else {
        // Apply a user-defined closure
        sl_object *params = fn->data.function.def.closure.params;
        sl_object *body_list = fn->data.function.def.closure.body;  // <<< This is now the list
        sl_object *closure_env = fn->data.function.def.closure.env;

        // Root closure parts temporarily
        sl_gc_add_root(&params);
        sl_gc_add_root(&body_list);
        sl_gc_add_root(&closure_env);

        // Create a new environment for the call, extending the closure's environment
        sl_object *call_env = sl_env_create(closure_env);
        sl_gc_add_root(&call_env);

        if (call_env == SL_OUT_OF_MEMORY_ERROR) {
            result = call_env;
            sl_gc_remove_root(&params);  // Unroot closure parts
            sl_gc_remove_root(&body_list);
            sl_gc_remove_root(&closure_env);
            goto cleanup_apply;  // Use goto for cleanup
        }

        // Bind arguments to parameters in the new environment
        sl_object *p = params;
        sl_object *a = args;
        bool bind_ok = true;
        while (p != SL_NIL && a != SL_NIL) {
            if (!sl_is_pair(p) || !sl_is_pair(a)) {
                result = sl_make_errorf("Apply: Mismatched arguments/parameters (improper list)");
                bind_ok = false;
                break;
            }
            if (!sl_is_symbol(sl_car(p))) {
                result = sl_make_errorf("Apply: Parameter list contains non-symbol");
                bind_ok = false;
                break;
            }

            // sl_env_define might allocate if extending bindings list
            sl_env_define(call_env, sl_car(p), sl_car(a));  // <<< REMOVED assignment to define_result
            // Need robust way to check if sl_env_define failed internally (OOM?)
            // For now, assume it succeeded if we got this far.

            p = sl_cdr(p);
            a = sl_cdr(a);
        }

        // Check for mismatched argument counts
        if (bind_ok && (p != SL_NIL || a != SL_NIL)) {
            result = sl_make_errorf("Apply: Mismatched argument count");
            bind_ok = false;
        }

        if (bind_ok) {
            // Evaluate the body expressions (implicit begin)
            if (body_list == SL_NIL) {  // No body expressions? Result is NIL.
                result = SL_NIL;
            } else if (!sl_is_pair(body_list)) {  // Body wasn't a proper list
                result = sl_make_errorf("Apply: Invalid body structure (not a list)");
            } else {
                sl_object *current_body_expr_node = body_list;
                result = SL_NIL;                          // Default result if body is empty, updated by last eval
                sl_gc_add_root(&current_body_expr_node);  // Root list traversal

                while (sl_is_pair(current_body_expr_node)) {
                    sl_object *expr_to_eval = sl_car(current_body_expr_node);
                    // Evaluate the expression
                    result = sl_eval(expr_to_eval, call_env);  // Eval expr - MIGHT GC

                    // Stop evaluation on error
                    if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                        break;
                    }
                    current_body_expr_node = sl_cdr(current_body_expr_node);
                }
                // Check if loop terminated because of improper list end
                if (current_body_expr_node != SL_NIL) {
                    result = sl_make_errorf("Apply: Improper list in function body");
                }
                sl_gc_remove_root(&current_body_expr_node);
            }
        }
        // else: result already holds the binding error

        sl_gc_remove_root(&call_env);
        sl_gc_remove_root(&params);
        sl_gc_remove_root(&body_list);  // Unroot the body list
        sl_gc_remove_root(&closure_env);
    }

cleanup_apply:
    sl_gc_remove_root(&result);
    sl_gc_remove_root(&args);
    sl_gc_remove_root(&fn);
    return result;
}
