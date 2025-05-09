#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <inttypes.h>  // For PRId64
#include <dirent.h>    // <<< For directory listing (POSIX)
#include <errno.h>     // <<< For strerror
#include <sys/stat.h>  // <<< For stat to check if it's a file

#include "sl_eval.h"
#include "sl_core.h"
#include "sl_env.h"
#include "sl_parse.h"  // Need for sl_parse_string
#include "sl_builtins.h"

// --- Forward Declarations ---
sl_object *sl_eval_list(sl_object *list, sl_object *env);

// static sl_object *eval_sequence(sl_object *seq, sl_object *env);  // Helper for begin/body logic
//  Renamed eval_sequence to be more specific
static sl_object *eval_sequence_with_defines(sl_object *seq, sl_object *env, sl_object **obj_ptr, sl_object **env_ptr);
static sl_object *eval_quasiquote_template(sl_object *template, sl_object *env, int level);  // <<< NEW

// Helper function to get symbol name safely
static const char *safe_symbol_name(sl_object *obj) {
    if (sl_is_symbol(obj)) {
        return sl_symbol_name(obj);
    }
    return "invalid-symbol";
}

static char sl_eval_debug_str_buffer[128];  // Static buffer for debug strings

// Provides a simple string representation of an object for debugging.
// Uses a static buffer, so it's not re-entrant or thread-safe.
static const char *sl_to_string_debug(sl_object *obj) {
    if (!obj) {
        strncpy(sl_eval_debug_str_buffer, "<null_obj>", sizeof(sl_eval_debug_str_buffer) - 1);
        sl_eval_debug_str_buffer[sizeof(sl_eval_debug_str_buffer) - 1] = '\0';
        return sl_eval_debug_str_buffer;
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "()");
        break;
    case SL_TYPE_BOOLEAN:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), obj->data.boolean ? "#t" : "#f");
        break;
    case SL_TYPE_SYMBOL:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "'%s", sl_symbol_name(obj) ? sl_symbol_name(obj) : "<?SYM?>");
        break;
    case SL_TYPE_NUMBER:
        // For a full representation, you'd call mpq_get_str or similar.
        // This is a simplified placeholder for debugging.
        if (obj->data.number.is_bignum) {
            // Note: mpq_get_str allocates memory that would need to be freed.
            // For a static buffer debug function, a placeholder is safer.
            snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#<bignum>");
        } else {
            if (obj->data.number.value.small_num.den == 1) {
                snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "%" PRId64, obj->data.number.value.small_num.num);
            } else {
                snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "%" PRId64 "/%" PRId64, obj->data.number.value.small_num.num, obj->data.number.value.small_num.den);
            }
        }
        break;
    case SL_TYPE_STRING:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "\"%.50s%s\"", sl_string_value(obj) ? sl_string_value(obj) : "", (sl_string_value(obj) && strlen(sl_string_value(obj)) > 50) ? "..." : "");
        break;
    case SL_TYPE_CHAR:
        // This is a very basic representation. A full one would handle named chars.
        if (isprint(obj->data.code_point)) {
            snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#\\%c", (char)obj->data.code_point);
        } else {
            snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#\\x%x", obj->data.code_point);
        }
        break;
    case SL_TYPE_PAIR:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "(...)");  // Simplified
        break;
    case SL_TYPE_FUNCTION:
        if (obj->data.function.is_builtin) {
            snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#<builtin:%s>", obj->data.function.def.builtin.name ? obj->data.function.def.builtin.name : "UNKNOWN> ");
        } else {
            snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#<procedure>");
        }
        break;
    case SL_TYPE_ERROR:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#<error: %.50s%s>", sl_error_message(obj) ? sl_error_message(obj) : "", (sl_error_message(obj) && strlen(sl_error_message(obj)) > 50) ? "..." : "");
        break;
    default:
        snprintf(sl_eval_debug_str_buffer, sizeof(sl_eval_debug_str_buffer), "#<type:%d>", obj->type);
        break;
    }
    // Ensure null-termination in all cases, as snprintf might truncate.
    sl_eval_debug_str_buffer[sizeof(sl_eval_debug_str_buffer) - 1] = '\0';
    return sl_eval_debug_str_buffer;
}

// eval_quasiquote_template:
// Processes a quasiquoted template.
// 'level' tracks the quasiquote nesting. level 1 is the outermost active quasiquote.
// This function now primarily transforms the template. Evaluation of unquoted
// expressions at the current level is handled by the list-building part.
static sl_object *eval_quasiquote_template(sl_object *template, sl_object *env, int level) {
    sl_object *result = SL_NIL;
    sl_object *processed_part = SL_NIL;

    SL_GC_ADD_ROOT(&template);
    SL_GC_ADD_ROOT(&env);
    SL_GC_ADD_ROOT(&result);
    SL_GC_ADD_ROOT(&processed_part);

    if (!sl_is_pair(template)) {
        result = template;  // Atoms are returned as is
        goto cleanup_qqt;
    }

    sl_object *op = sl_car(template);
    sl_object *template_args = sl_cdr(template);

    if (sl_is_symbol(op)) {
        const char *op_name = sl_symbol_name(op);

        if (strcmp(op_name, "unquote") == 0) {
            if (!sl_is_pair(template_args) || sl_cdr(template_args) != SL_NIL) {
                result = sl_make_errorf("Eval: Malformed unquote (expected one argument)");
                goto cleanup_qqt;
            }
            sl_object *arg = sl_car(template_args);
            SL_GC_ADD_ROOT(&arg);

            // This is when template IS (unquote ARG) and level is 1.
            if (sl_is_pair(arg) && sl_is_symbol(sl_car(arg))) {
                const char *arg_op_name = safe_symbol_name(sl_car(arg));
                sl_object *arg_op_args = sl_cdr(arg);
                if (sl_is_pair(arg_op_args) && sl_cdr(arg_op_args) == SL_NIL) {  // (op actual_arg)
                    sl_object *actual_inner_arg = sl_car(arg_op_args);
                    SL_GC_ADD_ROOT(&actual_inner_arg);
                    if (strcmp(arg_op_name, "unquote") == 0) {  // template is ,,INNER_ARG
                        // Result is ('unquote INNER_ARG_symbol)
                        sl_object *unquote_sym_lit = sl_make_symbol("unquote");
                        SL_GC_ADD_ROOT(&unquote_sym_lit);
                        sl_object *inner_arg_list = sl_make_pair(actual_inner_arg, SL_NIL);
                        SL_GC_ADD_ROOT(&inner_arg_list);
                        result = sl_make_pair(unquote_sym_lit, inner_arg_list);
                        SL_GC_REMOVE_ROOT(&inner_arg_list);
                        SL_GC_REMOVE_ROOT(&unquote_sym_lit);
                    } else if (strcmp(arg_op_name, "quasiquote") == 0) {  // template is ,`INNER_TMPL
                        // Result is (`INNER_TMPL) which is arg itself
                        result = arg;
                    } else {                         // template is ,X where X is other (op actual_arg)
                        result = sl_eval(arg, env);  // << CHANGED: Evaluate if it's a simple unquoted expression
                    }
                    SL_GC_REMOVE_ROOT(&actual_inner_arg);
                    goto cleanup_qqt_remove_arg;  // arg is already rooted and will be unrooted
                }
            }
            // template is ,X where X is atom or other list not matching ,, or ,`
            result = sl_eval(arg, env);  // << CHANGED: Evaluate if it's a simple unquoted atom/var

        cleanup_qqt_remove_arg:
            SL_GC_REMOVE_ROOT(&arg);
            goto cleanup_qqt;

        } else if (strcmp(op_name, "quasiquote") == 0) {  // Nested quasiquote: `ARG
            if (!sl_is_pair(template_args) || sl_cdr(template_args) != SL_NIL) {
                result = sl_make_errorf("Eval: Malformed nested quasiquote (expected one argument)");
                goto cleanup_qqt;
            }
            sl_object *nested_qq_arg = sl_car(template_args);
            SL_GC_ADD_ROOT(&nested_qq_arg);
            processed_part = eval_quasiquote_template(nested_qq_arg, env, level + 1);
            SL_GC_REMOVE_ROOT(&nested_qq_arg);
            if (processed_part == SL_OUT_OF_MEMORY_ERROR || sl_is_error(processed_part)) {
                result = processed_part;
                goto cleanup_qqt;
            }
            sl_object *quasiquote_sym_lit = sl_make_symbol("quasiquote");
            SL_GC_ADD_ROOT(&quasiquote_sym_lit);
            sl_object *arg_list = sl_make_pair(processed_part, SL_NIL);
            SL_GC_ADD_ROOT(&arg_list);
            result = sl_make_pair(quasiquote_sym_lit, arg_list);
            SL_GC_REMOVE_ROOT(&arg_list);
            SL_GC_REMOVE_ROOT(&quasiquote_sym_lit);
            goto cleanup_qqt;

        } else if (strcmp(op_name, "unquote-splicing") == 0) {  // ,@ARG
            if (!sl_is_pair(template_args) || sl_cdr(template_args) != SL_NIL) {
                result = sl_make_errorf("Eval: Malformed unquote-splicing (expected one argument)");
                goto cleanup_qqt;
            }
            if (level == 1) {  // Active unquote-splicing ,@ARG (template is (unquote-splicing ARG))
                sl_object *arg = sl_car(template_args);
                // Check if ARG is itself a qq-control form, which is an error for splicing
                if (sl_is_pair(arg) && sl_is_symbol(sl_car(arg))) {
                    const char *arg_op_name = safe_symbol_name(sl_car(arg));
                    sl_object *arg_op_args = sl_cdr(arg);
                    if (sl_is_pair(arg_op_args) && sl_cdr(arg_op_args) == SL_NIL &&
                        (strcmp(arg_op_name, "unquote") == 0 ||
                         strcmp(arg_op_name, "unquote-splicing") == 0 ||
                         strcmp(arg_op_name, "quasiquote") == 0)) {
                        result = sl_make_errorf("Eval: unquote-splicing argument cannot be a literal quasiquote control form: %s", sl_to_string_debug(arg));
                        goto cleanup_qqt;
                    }
                }
                result = sl_eval(arg, env);  // Evaluate to get the list to splice
                if (!(result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result) || sl_is_list(result))) {
                    result = sl_make_errorf("Eval: unquote-splicing did not evaluate to a list: %s", sl_to_string_debug(result));
                }
                // The caller (list builder) will handle the actual splicing if this was an item.
                // If this was the *entire* template, e.g. eval(`,@foo`), it's an error at sl_eval stage.
                // This path is for eval_qq( (unquote-splicing ARG), 1)
                // The list builder handles items of this form. If this is the whole template, sl_eval should error.
                // For now, this returns the evaluated list. sl_eval will need to reject it if it's not part of a list construction.
                // This specific error "cannot be at the head" is better handled by sl_eval if it gets a list from here.
                // For now, let eval_qq return the list, and the list builder will use it.
            } else {  // level > 1, dormant unquote-splicing
                sl_object *arg = sl_car(template_args);
                SL_GC_ADD_ROOT(&arg);
                processed_part = eval_quasiquote_template(arg, env, level - 1);
                SL_GC_REMOVE_ROOT(&arg);
                if (processed_part == SL_OUT_OF_MEMORY_ERROR || sl_is_error(processed_part)) {
                    result = processed_part;
                    goto cleanup_qqt;
                }
                sl_object *uqs_sym_lit = sl_make_symbol("unquote-splicing");
                SL_GC_ADD_ROOT(&uqs_sym_lit);
                sl_object *arg_list = sl_make_pair(processed_part, SL_NIL);
                SL_GC_ADD_ROOT(&arg_list);
                result = sl_make_pair(uqs_sym_lit, arg_list);
                SL_GC_REMOVE_ROOT(&arg_list);
                SL_GC_REMOVE_ROOT(&uqs_sym_lit);
                goto cleanup_qqt;
            }
        }
    }

    // General pair: (item . rest_of_template). Build list.
    sl_object *result_head_local = SL_NIL;
    sl_object **result_tail_ptr = &result_head_local;
    sl_object *current_template_node = template;

    SL_GC_ADD_ROOT(&result_head_local);

    while (sl_is_pair(current_template_node)) {
        sl_object *item_in_template = sl_car(current_template_node);
        sl_object *final_value_for_list = SL_NIL;
        bool spliced = false;

        SL_GC_ADD_ROOT(&item_in_template);

        if (sl_is_pair(item_in_template) && sl_is_symbol(sl_car(item_in_template))) {
            const char *item_op_name = safe_symbol_name(sl_car(item_in_template));
            sl_object *item_op_args_list = sl_cdr(item_in_template);

            if (level == 1 && sl_is_pair(item_op_args_list) && sl_cdr(item_op_args_list) == SL_NIL) {
                sl_object *x_arg = sl_car(item_op_args_list);  // This is the argument to unquote or unquote-splicing
                SL_GC_ADD_ROOT(&x_arg);

                if (strcmp(item_op_name, "unquote") == 0) {                  // item is ,X_arg
                    if (sl_is_pair(x_arg) && sl_is_symbol(sl_car(x_arg))) {  // If X_arg is (op ...)
                        const char *x_arg_op_name_inner = safe_symbol_name(sl_car(x_arg));
                        sl_object *x_arg_op_args_inner = sl_cdr(x_arg);
                        if (sl_is_pair(x_arg_op_args_inner) && sl_cdr(x_arg_op_args_inner) == SL_NIL) {
                            sl_object *innermost_arg = sl_car(x_arg_op_args_inner);
                            SL_GC_ADD_ROOT(&innermost_arg);
                            if (strcmp(x_arg_op_name_inner, "unquote") == 0) {       // item is ,,INNERMOST_ARG
                                final_value_for_list = sl_eval(innermost_arg, env);  // Ensures INNERMOST_ARG is evaluated
                                // Result is (unquote INNERMOST_ARG_symbol)
                                sl_object *unquote_sym = sl_make_symbol("unquote");
                                SL_GC_ADD_ROOT(&unquote_sym);
                                sl_object *val_list = sl_make_pair(innermost_arg, SL_NIL);
                                SL_GC_ADD_ROOT(&val_list);
                                final_value_for_list = sl_make_pair(unquote_sym, val_list);
                                SL_GC_REMOVE_ROOT(&val_list);
                                SL_GC_REMOVE_ROOT(&unquote_sym);
                            } else if (strcmp(x_arg_op_name_inner, "quasiquote") == 0) {  // item is ,`INNER_TMPL
                                final_value_for_list = sl_eval(x_arg, env);
                            } else {  // item is ,(op Y)
                                final_value_for_list = sl_eval(x_arg, env);
                            }
                            SL_GC_REMOVE_ROOT(&innermost_arg);
                        } else {  // item is ,(op Y Z ...) or ,(op)
                            final_value_for_list = sl_eval(x_arg, env);
                        }
                    } else {                                         // item is ,ATOM or ,(non-op-led-list ...)
                        final_value_for_list = sl_eval(x_arg, env);  // Ensures X_arg (atom/var) is evaluated
                    }
                } else if (strcmp(item_op_name, "unquote-splicing") == 0) {  // item is ,@X_arg
                    // X_arg cannot be ,,Y or ,`Y or ,@Y for splicing
                    if (sl_is_pair(x_arg) && sl_is_symbol(sl_car(x_arg))) {
                        const char *x_arg_op_name_inner = safe_symbol_name(sl_car(x_arg));
                        sl_object *x_arg_op_args_inner = sl_cdr(x_arg);
                        if (sl_is_pair(x_arg_op_args_inner) && sl_cdr(x_arg_op_args_inner) == SL_NIL &&
                            (strcmp(x_arg_op_name_inner, "unquote") == 0 ||
                             strcmp(x_arg_op_name_inner, "unquote-splicing") == 0 ||
                             strcmp(x_arg_op_name_inner, "quasiquote") == 0)) {
                            result = sl_make_errorf("Eval: unquote-splicing argument cannot be a literal quasiquote control form: %s", sl_to_string_debug(x_arg));
                            goto list_build_error;
                        }
                    }
                    final_value_for_list = sl_eval(x_arg, env);  // This is correct
                    spliced = true;
                } else {  // item is (op ...) but not active unquote/unquote-splicing
                    final_value_for_list = eval_quasiquote_template(item_in_template, env, level);
                }
                SL_GC_REMOVE_ROOT(&x_arg);
            } else {  // item is (op Y Z...) or (op) or not level 1 unquote/unquote-splicing
                final_value_for_list = eval_quasiquote_template(item_in_template, env, level);
            }
        } else {  // item_in_template is atom or list not starting with symbol
            final_value_for_list = eval_quasiquote_template(item_in_template, env, level);
        }
        SL_GC_REMOVE_ROOT(&item_in_template);
        SL_GC_ADD_ROOT(&final_value_for_list);

        if (final_value_for_list == SL_OUT_OF_MEMORY_ERROR || sl_is_error(final_value_for_list)) {
            result = final_value_for_list;
            SL_GC_REMOVE_ROOT(&final_value_for_list);
            goto list_build_error;
        }

        if (spliced) {
            if (!sl_is_list(final_value_for_list)) {
                result = sl_make_errorf("Eval: unquote-splicing did not evaluate to a list: %s", sl_to_string_debug(final_value_for_list));
                SL_GC_REMOVE_ROOT(&final_value_for_list);
                goto list_build_error;
            }
            sl_object *splice_node = final_value_for_list;
            while (sl_is_pair(splice_node)) {
                sl_object *val_to_add = sl_car(splice_node);
                sl_object *new_cell = sl_make_pair(val_to_add, SL_NIL);
                if (new_cell == SL_OUT_OF_MEMORY_ERROR) {
                    result = new_cell;
                    SL_GC_REMOVE_ROOT(&final_value_for_list);
                    goto list_build_error;
                }
                *result_tail_ptr = new_cell;
                result_tail_ptr = &new_cell->data.pair.cdr;
                splice_node = sl_cdr(splice_node);
            }
            if (splice_node != SL_NIL) {  // Improper list from splice
                result = sl_make_errorf("Eval: unquote-splicing argument was not a proper list: %s", sl_to_string_debug(final_value_for_list));
                SL_GC_REMOVE_ROOT(&final_value_for_list);
                goto list_build_error;
            }
        } else {
            sl_object *new_cell = sl_make_pair(final_value_for_list, SL_NIL);
            if (new_cell == SL_OUT_OF_MEMORY_ERROR) {
                result = new_cell;
                SL_GC_REMOVE_ROOT(&final_value_for_list);
                goto list_build_error;
            }
            *result_tail_ptr = new_cell;
            result_tail_ptr = &new_cell->data.pair.cdr;
        }
        SL_GC_REMOVE_ROOT(&final_value_for_list);
        current_template_node = sl_cdr(current_template_node);
    }

    // Tail processing for dotted pairs:
    if (current_template_node != SL_NIL) {
        sl_object *final_cdr_value = SL_NIL;  // Initialize
        SL_GC_ADD_ROOT(&final_cdr_value);     // Protect this local

        if (level == 1 && sl_is_pair(current_template_node) && sl_is_symbol(sl_car(current_template_node))) {
            const char *tail_op_name = safe_symbol_name(sl_car(current_template_node));
            sl_object *tail_op_args = sl_cdr(current_template_node);

            if (strcmp(tail_op_name, "unquote") == 0 && sl_is_pair(tail_op_args) && sl_cdr(tail_op_args) == SL_NIL) {
                sl_object *unquote_arg = sl_car(tail_op_args);  // This is Y from `,Y`
                SL_GC_ADD_ROOT(&unquote_arg);

                // Check if Y itself is ,,Z or ,`Z
                if (sl_is_pair(unquote_arg) && sl_is_symbol(sl_car(unquote_arg))) {
                    const char *uq_arg_op_inner = safe_symbol_name(sl_car(unquote_arg));
                    sl_object *uq_arg_args_inner = sl_cdr(unquote_arg);

                    if (sl_is_pair(uq_arg_args_inner) && sl_cdr(uq_arg_args_inner) == SL_NIL) {
                        sl_object *innermost_arg_val = sl_car(uq_arg_args_inner);
                        SL_GC_ADD_ROOT(&innermost_arg_val);
                        if (strcmp(uq_arg_op_inner, "unquote") == 0) {  // cdr is ,,INNERMOST_ARG_VAL
                            sl_object *unquote_sym = sl_make_symbol("unquote");
                            SL_GC_ADD_ROOT(&unquote_sym);
                            sl_object *val_list = sl_make_pair(innermost_arg_val, SL_NIL);
                            SL_GC_ADD_ROOT(&val_list);
                            final_cdr_value = sl_make_pair(unquote_sym, val_list);
                            SL_GC_REMOVE_ROOT(&val_list);
                            SL_GC_REMOVE_ROOT(&unquote_sym);
                        } else if (strcmp(uq_arg_op_inner, "quasiquote") == 0) {  // cdr is ,`INNERMOST_ARG_VAL
                            final_cdr_value = sl_eval(unquote_arg, env);          // unquote_arg is (`INNERMOST_ARG_VAL)
                        } else {                                                  // cdr is ,(op INNERMOST_ARG_VAL)
                            final_cdr_value = sl_eval(unquote_arg, env);
                        }
                        SL_GC_REMOVE_ROOT(&innermost_arg_val);
                    } else {  // cdr is ,(op Z W...)
                        final_cdr_value = sl_eval(unquote_arg, env);
                    }
                } else {                                          // cdr is ,ATOM (like `,y`), unquote_arg is the ATOM (e.g. symbol y)
                    final_cdr_value = sl_eval(unquote_arg, env);  // This should evaluate the atom
                }
                SL_GC_REMOVE_ROOT(&unquote_arg);
            } else if (strcmp(tail_op_name, "unquote-splicing") == 0) {
                result = sl_make_errorf("Eval: unquote-splicing cannot appear in cdr of a pair");
                SL_GC_REMOVE_ROOT(&final_cdr_value);  // Remove before goto
                goto list_build_error;
            } else {  // Not an active unquote for the cdr, or not (unquote X) form
                final_cdr_value = eval_quasiquote_template(current_template_node, env, level);
            }
        } else {  // Not a form like (unquote X) at level 1 for the cdr, or cdr is not a pair
            final_cdr_value = eval_quasiquote_template(current_template_node, env, level);
        }

        if (final_cdr_value == SL_OUT_OF_MEMORY_ERROR || sl_is_error(final_cdr_value)) {
            result = final_cdr_value;
            SL_GC_REMOVE_ROOT(&final_cdr_value);  // Remove before goto
            goto list_build_error;
        }
        *result_tail_ptr = final_cdr_value;
        SL_GC_REMOVE_ROOT(&final_cdr_value);  // Remove after use
    }
    result = result_head_local;

list_build_error:
    SL_GC_REMOVE_ROOT(&result_head_local);

cleanup_qqt:
    SL_GC_REMOVE_ROOT(&processed_part);
    SL_GC_REMOVE_ROOT(&result);
    SL_GC_REMOVE_ROOT(&env);
    SL_GC_REMOVE_ROOT(&template);
    return result;
}

// --- Sequence Evaluation Helper ---
// Evaluates a sequence of expressions, handling internal defines (letrec* style).
// Returns the value of the last expression, or an error object.
// If the last expression is in tail position relative to the *caller*,
// it modifies *obj_ptr and *env_ptr and returns SL_CONTINUE_EVAL
// to signal the caller to jump to top_of_eval.
static sl_object *eval_sequence_with_defines(sl_object *seq, sl_object *env, sl_object **obj_ptr, sl_object **env_ptr) {
    sl_object *current_node = seq;
    sl_object *defines_list = SL_NIL;
    sl_object **defines_tail_ptr = &defines_list;
    sl_object *body_start_node = seq;     // Where non-define expressions start
    sl_object *sequence_result = SL_NIL;  // Default result for empty sequence

    // --- Root parameters and locals ---
    SL_GC_ADD_ROOT(&seq);
    SL_GC_ADD_ROOT(&env);
    SL_GC_ADD_ROOT(&defines_list);
    SL_GC_ADD_ROOT(&body_start_node);
    SL_GC_ADD_ROOT(&sequence_result);  // remove after GC analysis

    // --- Pass 1: Scan for defines and create placeholders ---
    bool defines_found = false;
    while (sl_is_pair(current_node)) {
        sl_object *expr = sl_car(current_node);
        if (sl_is_pair(expr) && sl_is_symbol(sl_car(expr)) && strcmp(sl_symbol_name(sl_car(expr)), "define") == 0) {
            defines_found = true;
            sl_object *define_args = sl_cdr(expr);
            if (define_args == SL_NIL) {
                sequence_result = sl_make_errorf("Eval: Malformed define in sequence (no target)");
                goto cleanup_sequence;
            }
            sl_object *target = sl_car(define_args);
            sl_object *var_sym = SL_NIL;

            if (sl_is_pair(target)) {  // Function define: (define (f x) ...)
                var_sym = sl_car(target);
            } else if (sl_is_symbol(target)) {  // Variable define: (define x ...)
                var_sym = target;
            } else {
                sequence_result = sl_make_errorf("Eval: Invalid define target in sequence");
                goto cleanup_sequence;
            }

            if (!sl_is_symbol(var_sym)) {
                sequence_result = sl_make_errorf("Eval: Define target must be or start with a symbol");
                goto cleanup_sequence;
            }

            // Add define expression to defines_list
            sl_object *define_pair = sl_make_pair(expr, SL_NIL);
            CHECK_ALLOC_GOTO(define_pair, cleanup_sequence, sequence_result);
            *defines_tail_ptr = define_pair;
            defines_tail_ptr = &define_pair->data.pair.cdr;

            // Define placeholder in the environment
            sl_env_define(env, var_sym, SL_UNDEFINED);
            // Check OOM?

            body_start_node = sl_cdr(current_node);  // Move body start past this define
        } else {
            // First non-define expression found
            break;
        }
        current_node = sl_cdr(current_node);
    }
    // Check for improper list during define scan
    if (!defines_found && current_node != seq) {  // Only error if we advanced
        if (current_node != SL_NIL) {
            sequence_result = sl_make_errorf("Eval: Improper list while scanning for defines");
            goto cleanup_sequence;
        }
    }

    // --- Pass 2: Evaluate define expressions ---
    if (defines_found) {
        current_node = defines_list;
        SL_GC_ADD_ROOT(&current_node);  // Root traversal pointer for defines
        while (current_node != SL_NIL) {
            // We know current_node is a pair containing a define expression
            sl_object *define_expr = sl_car(current_node);
            sl_object *define_args = sl_cdr(define_expr);  // Args of define: (sym val) or ((f p) b)
            sl_object *target = sl_car(define_args);
            sl_object *value_expr_or_body = sl_cdr(define_args);
            sl_object *var_sym = SL_NIL;
            sl_object *value_to_set = SL_NIL;

            SL_GC_ADD_ROOT(&value_to_set);  // Root value slot

            if (sl_is_pair(target)) {  // Function define
                var_sym = sl_car(target);
                sl_object *params = sl_cdr(target);
                sl_object *body = value_expr_or_body;

                value_to_set = sl_make_closure(params, body, env);
                CHECK_ALLOC_GOTO(value_to_set, cleanup_sequence_eval, sequence_result);
            } else {  // Variable define
                var_sym = target;
                if (!sl_is_pair(value_expr_or_body) || sl_cdr(value_expr_or_body) != SL_NIL) {
                    sequence_result = sl_make_errorf("Eval: Malformed variable define in sequence");
                    SL_GC_REMOVE_ROOT(&value_to_set);
                    goto cleanup_sequence_eval;
                }
                sl_object *value_expr = sl_car(value_expr_or_body);
                value_to_set = sl_eval(value_expr, env);  // Eval value - MIGHT GC
                // value_to_set is already rooted
                if (value_to_set == SL_OUT_OF_MEMORY_ERROR || sl_is_error(value_to_set)) {
                    sequence_result = value_to_set;  // Propagate error
                    SL_GC_REMOVE_ROOT(&value_to_set);
                    goto cleanup_sequence_eval;
                }
            }

            // Update the placeholder using set! semantics
            if (!sl_env_set(env, var_sym, value_to_set)) {
                sequence_result = sl_make_errorf("Eval: Internal error - failed to set! defined variable '%s'", sl_symbol_name(var_sym));
                SL_GC_REMOVE_ROOT(&value_to_set);
                goto cleanup_sequence_eval;
            }
            SL_GC_REMOVE_ROOT(&value_to_set);  // Unroot value after set
            current_node = sl_cdr(current_node);
        }  // End while defines_list
    cleanup_sequence_eval:
        SL_GC_REMOVE_ROOT(&current_node);  // Unroot traversal pointer
        if (sequence_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(sequence_result)) {
            goto cleanup_sequence;  // Propagate error from define eval
        }
    }  // End if defines_found

    // --- Pass 3: Evaluate body expressions ---
    current_node = body_start_node;
    SL_GC_ADD_ROOT(&current_node);
    sequence_result = SL_NIL;  // Default if body is empty

    if (current_node == SL_NIL) {
        goto cleanup_sequence_body;
    }

    while (sl_is_pair(current_node)) {
        sl_object *expr_to_eval = sl_car(current_node);
        sl_object *next_node = sl_cdr(current_node);

        if (next_node == SL_NIL) {     // Tail position
            if (obj_ptr && env_ptr) {  // TCO possible
                *obj_ptr = expr_to_eval;
                *env_ptr = env;
                // <<< ADD DEBUG PRINT >>>
                // fprintf(stderr, "[DEBUG TCO Cleanup] Removing roots in eval_sequence_with_defines (env VarAddr=%p)\n", (void *)&env);
                // --- Clean up ALL local roots before returning signal ---
                SL_GC_REMOVE_ROOT(&sequence_result);
                SL_GC_REMOVE_ROOT(&current_node);     // <<< ADD: Unroot body traversal node
                SL_GC_REMOVE_ROOT(&body_start_node);  // <<< ADD
                SL_GC_REMOVE_ROOT(&defines_list);     // <<< ADD
                SL_GC_REMOVE_ROOT(&env);              // <<< ADD: Unroot local copy of env pointer
                SL_GC_REMOVE_ROOT(&seq);              // <<< ADD: Unroot local copy of seq pointer
                // Note: Roots added during Pass 2 (like value_to_set) should have been
                // cleaned up within that pass or its error paths.
                return SL_CONTINUE_EVAL;  // Signal TCO
            } else {                      // No TCO possible
                SL_GC_ADD_ROOT(&expr_to_eval);
                SL_GC_REMOVE_ROOT(&sequence_result);           // Remove root before assigning final value
                sequence_result = sl_eval(expr_to_eval, env);  // Evaluate final expression
                SL_GC_ADD_ROOT(&sequence_result);              // Re-root final value
                SL_GC_REMOVE_ROOT(&expr_to_eval);
                goto cleanup_sequence_body;  // Go to cleanup
            }
        } else {                                           // Not tail position
            SL_GC_REMOVE_ROOT(&sequence_result);           // Remove previous root
            sequence_result = sl_eval(expr_to_eval, env);  // Evaluate intermediate
            SL_GC_ADD_ROOT(&sequence_result);              // Root intermediate

            if (sequence_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(sequence_result)) {
                goto cleanup_sequence_body;  // Error occurred, propagate
            }
        }
        current_node = sl_cdr(current_node);
    }

    if (current_node != SL_NIL) {             // Improper list
        SL_GC_REMOVE_ROOT(&sequence_result);  // Remove root before assigning error
        sequence_result = sl_make_errorf("Eval: Improper list in sequence body");
        SL_GC_ADD_ROOT(&sequence_result);  // Root error
    }

cleanup_sequence_body:
    SL_GC_REMOVE_ROOT(&current_node);
cleanup_sequence:
    SL_GC_REMOVE_ROOT(&body_start_node);
    SL_GC_REMOVE_ROOT(&defines_list);
    SL_GC_REMOVE_ROOT(&env);
    SL_GC_REMOVE_ROOT(&seq);
    SL_GC_REMOVE_ROOT(&sequence_result);  // <<< ADD THIS LINE
    return sequence_result;
}

sl_object *sl_eval(sl_object *obj_in, sl_object *env_in) {
    sl_object *obj = obj_in;
    sl_object *env = env_in;
    sl_object *result = SL_NIL;

    // --- Root key local variables ---
    SL_GC_ADD_ROOT(&obj);
    SL_GC_ADD_ROOT(&env);
    SL_GC_ADD_ROOT(&result);

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
    case SL_TYPE_CHAR:
    case SL_TYPE_ERROR:  // Errors evaluate to themselves
        result = obj;    // Self-evaluating
        break;

    case SL_TYPE_SYMBOL: {
        // Look up the binding pair for the symbol
        sl_object *binding_pair = sl_env_lookup(env, obj);

        if (binding_pair == SL_NIL) {
            // Symbol not found (lookup returned NIL)
            const char *sym_name = sl_symbol_name(obj);
            result = sl_make_errorf("Eval: Unbound symbol '%s'", sym_name);
        } else if (!sl_is_pair(binding_pair)) {
            // Should not happen if lookup is correct, but check defensively
            result = sl_make_errorf("Eval: Internal error - lookup returned non-pair for found symbol '%s'", sl_symbol_name(obj));
        } else {
            // Symbol found, the value is the cdr of the binding pair
            result = sl_cdr(binding_pair);  // <<< CHANGED: Get value from cdr
        }
        break;  // Break from switch case SL_TYPE_SYMBOL
    }

    case SL_TYPE_PAIR: {
        sl_object *op_obj = sl_car(obj);
        sl_object *args = sl_cdr(obj);

        if (sl_is_symbol(op_obj)) {
            const char *op_name = sl_symbol_name(op_obj);

            // --- MACRO EXPANSION CHECK (before special forms) ---
            sl_object *transformer_proc = sl_env_lookup_macro(env, op_obj);
            if (transformer_proc != SL_NIL) {
                if (!sl_is_function(transformer_proc)) {
                    result = sl_make_errorf("Eval: Macro keyword '%s' bound to non-procedure.", op_name);
                    break;
                }

                sl_object *original_form = obj;  // The entire (macro-keyword arg1 arg2 ...) form
                sl_object *transformer_arg_list = NULL;
                sl_object *expanded_code = NULL;

                SL_GC_ADD_ROOT(&original_form);     // Already rooted via obj, but good for clarity
                SL_GC_ADD_ROOT(&transformer_proc);  // Root the transformer
                SL_GC_ADD_ROOT(&transformer_arg_list);
                SL_GC_ADD_ROOT(&expanded_code);

                transformer_arg_list = sl_make_pair(original_form, SL_NIL);
                if (transformer_arg_list == SL_OUT_OF_MEMORY_ERROR) {
                    result = SL_OUT_OF_MEMORY_ERROR;
                    goto cleanup_macro_expansion;
                }

                // Call the transformer procedure. It should return a new S-expression.
                // The transformer is called with the original form, not evaluated arguments.
                // We don't use TCO for the transformer call itself; it must return the expanded code.
                expanded_code = sl_apply(transformer_proc, transformer_arg_list, NULL, NULL);

                if (expanded_code == SL_OUT_OF_MEMORY_ERROR || sl_is_error(expanded_code)) {
                    result = expanded_code;  // Propagate error from transformer
                    goto cleanup_macro_expansion;
                }

                // The result of the transformer is the new code to evaluate.
                // Update 'obj' and loop to evaluate the expanded code.
                SL_GC_REMOVE_ROOT(&obj);  // Unroot old obj
                obj = expanded_code;      // obj now points to the expanded code
                SL_GC_ADD_ROOT(&obj);     // Re-root the new obj

            cleanup_macro_expansion:
                SL_GC_REMOVE_ROOT(&expanded_code);
                SL_GC_REMOVE_ROOT(&transformer_arg_list);
                SL_GC_REMOVE_ROOT(&transformer_proc);
                SL_GC_REMOVE_ROOT(&original_form);  // obj is now expanded_code or an error

                if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                    break;  // Error occurred, break from switch
                }
                goto top_of_eval;  // Tail call: evaluate the expanded code
            }
            // --- END MACRO EXPANSION ---

            // --- DEFINE-SYNTAX ---
            if (strcmp(op_name, "define-syntax") == 0) {
                SL_GC_ADD_ROOT(&args);  // Root the arguments to define-syntax
                // (define-syntax keyword transformer-expr)
                if (!sl_is_pair(args) || !sl_is_pair(sl_cdr(args)) || sl_cdr(sl_cdr(args)) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed define-syntax (expected keyword and transformer-expr)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                sl_object *keyword_sym = sl_car(args);
                sl_object *transformer_expr = sl_cadr(args);

                if (!sl_is_symbol(keyword_sym)) {
                    result = sl_make_errorf("Eval: define-syntax keyword must be a symbol, got %s", sl_type_name(keyword_sym ? keyword_sym->type : -1));
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }

                SL_GC_REMOVE_ROOT(&args);  // Unroot args before eval

                sl_object *transformer_proc_obj = sl_eval(transformer_expr, env);  // Evaluate the transformer expression
                SL_GC_ADD_ROOT(&transformer_proc_obj);

                if (transformer_proc_obj == SL_OUT_OF_MEMORY_ERROR || sl_is_error(transformer_proc_obj)) {
                    result = transformer_proc_obj;  // Propagate error
                } else if (!sl_is_function(transformer_proc_obj)) {
                    result = sl_make_errorf("Eval: define-syntax transformer expression did not evaluate to a procedure.");
                } else {
                    sl_env_define_macro(env, keyword_sym, transformer_proc_obj);
                    result = keyword_sym;  // R7RS says define-syntax returns an unspecified value. Keyword is fine.
                }
                SL_GC_REMOVE_ROOT(&transformer_proc_obj);
                break;  // Break from switch (handled define-syntax)
            }

            // --- QUOTE ---
            if (strcmp(op_name, "quote") == 0) {
                SL_GC_ADD_ROOT(&args);  // Root args for safety
                if (args == SL_NIL || sl_cdr(args) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed quote");
                } else {
                    result = sl_car(args);  // Don't eval args of quote
                }
                SL_GC_REMOVE_ROOT(&args);
                break;  // Break from switch (handled special form)
            }
            // --- QUASIQUOTE ---  // <<< NEW BLOCK
            else if (strcmp(op_name, "quasiquote") == 0) {
                SL_GC_ADD_ROOT(&args);  // Root args for safety
                if (args == SL_NIL || !sl_is_pair(args) || sl_cdr(args) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed quasiquote (expected one argument)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                sl_object *template = sl_car(args);
                // template is part of args, which is rooted.
                // eval_quasiquote_template will root its arguments.
                SL_GC_REMOVE_ROOT(&args);  // Unroot args before calling helper

                result = eval_quasiquote_template(template, env, 1);
                // eval_quasiquote_template handles rooting of its return value or returns OOM/error
                break;
            }
            // --- UNQUOTE (error if top-level) --- // <<< NEW BLOCK
            else if (strcmp(op_name, "unquote") == 0) {
                result = sl_make_errorf("Eval: unquote appeared outside of quasiquote");
                break;
            }
            // --- UNQUOTE-SPLICING (error if top-level) --- // <<< NEW BLOCK
            else if (strcmp(op_name, "unquote-splicing") == 0) {
                result = sl_make_errorf("Eval: unquote-splicing appeared outside of quasiquote");
                break;
            }
            // --- IF ---
            else if (strcmp(op_name, "if") == 0) {
                // Root args temporarily
                SL_GC_ADD_ROOT(&args);
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
                    SL_GC_REMOVE_ROOT(&args);
                    break;  // Break from switch
                }
                if (!sl_is_pair(arg3_pair)) { alt_expr = SL_NIL; }

                SL_GC_REMOVE_ROOT(&args);  // Unroot args before eval

                // Root expressions needed after test_result is evaluated
                SL_GC_ADD_ROOT(&conseq_expr);
                SL_GC_ADD_ROOT(&alt_expr);

                sl_object *test_result = sl_eval(test_expr, env);  // Eval test - MIGHT GC
                SL_GC_ADD_ROOT(&test_result);

                if (test_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(test_result)) {
                    result = test_result;  // Propagate error
                    goto cleanup_if;
                } else if (test_result != SL_FALSE) {
                    obj = conseq_expr;  // Tail call conseq
                } else {
                    obj = alt_expr;  // Tail call alt
                }
                // Unroot temporaries before tail call
                SL_GC_REMOVE_ROOT(&test_result);
                SL_GC_REMOVE_ROOT(&alt_expr);
                SL_GC_REMOVE_ROOT(&conseq_expr);
                goto top_of_eval;  // Tail call optimization

            cleanup_if:                           // Cleanup path for errors during test eval or structure check
                SL_GC_REMOVE_ROOT(&test_result);  // Ensure unrooted
                SL_GC_REMOVE_ROOT(&alt_expr);     // Ensure unrooted
                SL_GC_REMOVE_ROOT(&conseq_expr);  // Ensure unrooted
                // args was already unrooted
                break;  // <<< FIX: Break from switch, result holds the error >>>
            }
            // --- DEFINE ---
            else if (strcmp(op_name, "define") == 0) {
                SL_GC_ADD_ROOT(&args);
                // (define symbol value) or (define (fn params...) body...)
                if (args == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed define (no symbol/value)");
                    SL_GC_REMOVE_ROOT(&args);
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
                        SL_GC_REMOVE_ROOT(&args);
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
                        SL_GC_REMOVE_ROOT(&args);
                        break;
                    }
                    sl_object *value_expr = sl_car(value_expr_pair);

                    // Unroot op_obj and args before eval
                    // SL_GC_REMOVE_ROOT(&op_obj);
                    // SL_GC_REMOVE_ROOT(&args);

                    sl_object *value = sl_eval(value_expr, env);
                    SL_GC_ADD_ROOT(&value);

                    if (value == SL_OUT_OF_MEMORY_ERROR) {
                        result = value;
                    } else {
                        // sl_env_define returns void
                        sl_env_define(env, target, value);
                        // Check for OOM? See comment above.
                        result = target;  // Return the symbol
                    }
                    SL_GC_REMOVE_ROOT(&value);

                } else {
                    result = sl_make_errorf("Eval: Invalid target for define");
                }
                SL_GC_REMOVE_ROOT(&args);
                break;
            }
            // --- SET! --- <<< ADDED BLOCK
            else if (strcmp(op_name, "set!") == 0) {
                SL_GC_ADD_ROOT(&args);  // Root args
                // (set! symbol value)
                if (args == SL_NIL || !sl_is_pair(args) || sl_cdr(args) == SL_NIL || !sl_is_pair(sl_cdr(args)) || sl_cdr(sl_cdr(args)) != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed set!");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                sl_object *target_sym = sl_car(args);
                sl_object *value_expr = sl_cadr(args);  // Use helper or sl_car(sl_cdr(args))

                if (!sl_is_symbol(target_sym)) {
                    result = sl_make_errorf("Eval: Target for set! must be a symbol");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }

                SL_GC_REMOVE_ROOT(&args);  // Unroot args before eval

                sl_object *value = sl_eval(value_expr, env);  // Eval value - MIGHT GC
                SL_GC_ADD_ROOT(&value);                       // Root result

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
                SL_GC_REMOVE_ROOT(&value);  // Unroot value
                break;                      // Break from switch
            }
            // --- LAMBDA ---
            else if (strcmp(op_name, "lambda") == 0) {
                SL_GC_ADD_ROOT(&args);  // Root args
                // (lambda (params...) body...)
                if (args == SL_NIL || sl_cdr(args) == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed lambda (missing params or body)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                sl_object *params = sl_car(args);
                sl_object *body_list = sl_cdr(args);  // Body is the rest of the list

                // TODO: Validate params is a proper list of symbols (or NIL)
                // TODO: Validate body_list is a proper list

                // Create closure with the *list* of body expressions
                result = sl_make_closure(params, body_list, env);
                CHECK_ALLOC(result);  // Checks if result is SL_OUT_OF_MEMORY_ERROR
                SL_GC_REMOVE_ROOT(&args);
                break;  // Break from switch
            }
            // --- BEGIN ---
            else if (strcmp(op_name, "begin") == 0) {
                sl_object *temp_res = eval_sequence_with_defines(args, env, &obj, &env);
                if (temp_res == SL_CONTINUE_EVAL) {
                    // result = temp_res;  // Keep signal
                    goto top_of_eval;
                }
                SL_GC_REMOVE_ROOT(&result);  // Remove old root
                result = temp_res;           // Assign new result
                SL_GC_ADD_ROOT(&result);     // Add new root
                break;
            }
            // --- LET / NAMED LET ---
            else if (strcmp(op_name, "let") == 0) {
                // (let bindings body...) or (let name bindings body...)
                SL_GC_ADD_ROOT(&args);  // Root the rest of the let form

                if (args == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed let (missing bindings/body)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }

                sl_object *first_arg = sl_car(args);
                sl_object *body_list = sl_cdr(args);
                sl_object *name_sym = SL_NIL;  // For named let
                sl_object *bindings = SL_NIL;

                // Check for named let variant
                if (sl_is_symbol(first_arg)) {
                    name_sym = first_arg;
                    SL_GC_ADD_ROOT(&name_sym);  // Root name symbol
                    if (body_list == SL_NIL || !sl_is_pair(body_list)) {
                        result = sl_make_errorf("Eval: Malformed named let (missing bindings/body)");
                        SL_GC_REMOVE_ROOT(&name_sym);
                        SL_GC_REMOVE_ROOT(&args);
                        break;
                    }
                    bindings = sl_car(body_list);
                    body_list = sl_cdr(body_list);
                } else {
                    // Standard let
                    bindings = first_arg;
                }

                // Validate bindings structure and body
                if (!sl_is_list(bindings)) {  // sl_is_list checks for proper list or NIL
                    result = sl_make_errorf("Eval: Malformed let bindings (not a list)");
                    if (name_sym != SL_NIL) SL_GC_REMOVE_ROOT(&name_sym);
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                if (body_list == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed let (missing body)");
                    if (name_sym != SL_NIL) SL_GC_REMOVE_ROOT(&name_sym);
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }

                // Root bindings and body before evaluation loops
                SL_GC_ADD_ROOT(&bindings);
                SL_GC_ADD_ROOT(&body_list);

                // --- Evaluate initializers in the *current* environment ---
                sl_object *vars_list = SL_NIL;
                sl_object *vals_list = SL_NIL;
                sl_object **vars_tail_ptr = &vars_list;
                sl_object **vals_tail_ptr = &vals_list;
                sl_object *current_binding = bindings;
                bool init_eval_ok = true;

                SL_GC_ADD_ROOT(&vars_list);  // Root lists being built
                SL_GC_ADD_ROOT(&vals_list);
                SL_GC_ADD_ROOT(&current_binding);

                while (current_binding != SL_NIL) {
                    if (!sl_is_pair(current_binding)) {  // Should be caught by sl_is_list, but double check
                        result = sl_make_errorf("Eval: Malformed let bindings (improper list)");
                        init_eval_ok = false;
                        break;
                    }
                    sl_object *binding_pair = sl_car(current_binding);
                    if (!sl_is_pair(binding_pair) || sl_cdr(binding_pair) == SL_NIL || !sl_is_pair(sl_cdr(binding_pair)) || sl_cdr(sl_cdr(binding_pair)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed let binding pair");
                        init_eval_ok = false;
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    sl_object *init_expr = sl_cadr(binding_pair);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in let binding must be a symbol");
                        init_eval_ok = false;
                        break;
                    }

                    // Evaluate init_expr in the *outer* env
                    sl_object *init_val = sl_eval(init_expr, env);  // MIGHT GC
                    SL_GC_ADD_ROOT(&init_val);                      // Root the evaluated value

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        SL_GC_REMOVE_ROOT(&init_val);
                        init_eval_ok = false;
                        break;
                    }

                    // Append var_sym to vars_list
                    sl_object *var_pair = sl_make_pair(var_sym, SL_NIL);
                    CHECK_ALLOC_GOTO(var_pair, oom_let, result);
                    *vars_tail_ptr = var_pair;
                    vars_tail_ptr = &var_pair->data.pair.cdr;

                    // Append init_val to vals_list
                    sl_object *val_pair = sl_make_pair(init_val, SL_NIL);
                    CHECK_ALLOC_GOTO(val_pair, oom_let, result);
                    *vals_tail_ptr = val_pair;
                    vals_tail_ptr = &val_pair->data.pair.cdr;

                    SL_GC_REMOVE_ROOT(&init_val);  // Value is now safe in vals_list
                    current_binding = sl_cdr(current_binding);
                }  // End while bindings

                // Unroot temporary binding list traversal pointer
                SL_GC_REMOVE_ROOT(&current_binding);

                if (!init_eval_ok) {   // Error during init eval or binding list parsing
                    goto cleanup_let;  // Result already holds the error
                }

                // --- Create new environment and bind variables ---
                sl_object *let_env = sl_env_create(env);
                CHECK_ALLOC_GOTO(let_env, oom_let, result);
                SL_GC_ADD_ROOT(&let_env);  // Root the new environment

                // --- Handle named let: define the recursive function ---
                if (name_sym != SL_NIL) {
                    // Create lambda: (lambda (vars...) body...)
                    sl_object *lambda = sl_make_closure(vars_list, body_list, let_env);
                    CHECK_ALLOC_GOTO(lambda, oom_let_env, result);
                    // Define the name *within* the let_env
                    sl_env_define(let_env, name_sym, lambda);
                    // Note: lambda is now potentially reachable via let_env
                }

                // --- Bind vars to evaluated vals in the new environment ---
                sl_object *v = vars_list;
                sl_object *val = vals_list;
                while (v != SL_NIL) {  // Assumes vars_list and vals_list have same length
                    sl_env_define(let_env, sl_car(v), sl_car(val));
                    // Check for OOM from define?
                    v = sl_cdr(v);
                    val = sl_cdr(val);
                }

                // --- Evaluate body in the new environment ---
                // Use helper function for sequence evaluation with TCO
                // Unroot lists before potential tail call in eval_sequence
                SL_GC_REMOVE_ROOT(&vals_list);
                SL_GC_REMOVE_ROOT(&vars_list);
                // SL_GC_REMOVE_ROOT(&body_list);
                SL_GC_REMOVE_ROOT(&bindings);
                SL_GC_REMOVE_ROOT(&args);
                if (name_sym != SL_NIL) SL_GC_REMOVE_ROOT(&name_sym);

                sl_object *temp_res = eval_sequence_with_defines(body_list, let_env, &obj, &env);
                SL_GC_REMOVE_ROOT(&let_env);
                SL_GC_REMOVE_ROOT(&body_list);
                if (temp_res == SL_CONTINUE_EVAL) {
                    // result = temp_res;  // Keep signal
                    goto top_of_eval;
                }
                SL_GC_REMOVE_ROOT(&result);  // Remove old root
                result = temp_res;           // Assign new result
                SL_GC_ADD_ROOT(&result);     // Add new root
                                             // Otherwise, result holds the final value or error from the body

            oom_let_env:                        // Label for OOM after let_env creation (if needed)
                                                // SL_GC_REMOVE_ROOT(&let_env); // Already handled
            oom_let:                            // Label for OOM before let_env creation
            cleanup_let:                        // General cleanup label for let
                SL_GC_REMOVE_ROOT(&vals_list);  // Ensure unrooted on error paths
                SL_GC_REMOVE_ROOT(&vars_list);
                SL_GC_REMOVE_ROOT(&body_list);
                SL_GC_REMOVE_ROOT(&bindings);
                SL_GC_REMOVE_ROOT(&args);
                if (name_sym != SL_NIL) SL_GC_REMOVE_ROOT(&name_sym);
                break;  // Break from switch case 'let'
            }  // End LET / NAMED LET block
            // --- LET* --- <<< NEW BLOCK
            else if (strcmp(op_name, "let*") == 0) {
                // (let* bindings body...)
                SL_GC_ADD_ROOT(&args);  // Root the rest of the form

                if (args == SL_NIL || !sl_is_pair(args)) {
                    result = sl_make_errorf("Eval: Malformed let* (missing bindings/body)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                sl_object *bindings = sl_car(args);
                sl_object *body_list = sl_cdr(args);

                if (!sl_is_list(bindings)) {
                    result = sl_make_errorf("Eval: Malformed let* bindings (not a list)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                if (body_list == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed let* (missing body)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }

                SL_GC_ADD_ROOT(&bindings);
                SL_GC_ADD_ROOT(&body_list);

                // --- Sequentially evaluate and bind ---
                sl_object *current_binding_node = bindings;
                sl_object *let_star_env = env;  // Start with outer env
                SL_GC_ADD_ROOT(&let_star_env);  // Root the evolving environment
                SL_GC_ADD_ROOT(&current_binding_node);

                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) {
                        result = sl_make_errorf("Eval: Malformed let* bindings (improper list)");
                        goto cleanup_let_star;
                    }
                    sl_object *binding_pair = sl_car(current_binding_node);
                    if (!sl_is_pair(binding_pair) || sl_cdr(binding_pair) == SL_NIL || !sl_is_pair(sl_cdr(binding_pair)) || sl_cdr(sl_cdr(binding_pair)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed let* binding pair");
                        goto cleanup_let_star;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    sl_object *init_expr = sl_cadr(binding_pair);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in let* binding must be a symbol");
                        goto cleanup_let_star;
                    }

                    // Evaluate init_expr in the *current* let_star_env
                    sl_object *init_val = sl_eval(init_expr, let_star_env);  // MIGHT GC
                    SL_GC_ADD_ROOT(&init_val);

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        SL_GC_REMOVE_ROOT(&init_val);
                        goto cleanup_let_star;
                    }

                    // Create a *new* environment extending the current one
                    sl_object *next_env = sl_env_create(let_star_env);
                    CHECK_ALLOC_GOTO(next_env, cleanup_let_star_val, result);

                    // Define var in the new environment
                    sl_env_define(next_env, var_sym, init_val);
                    // Check OOM?

                    // Update let_star_env for the next iteration
                    SL_GC_REMOVE_ROOT(&let_star_env);  // Unroot old env ptr
                    let_star_env = next_env;           // Point to the new env
                    SL_GC_ADD_ROOT(&let_star_env);     // Root the new env ptr

                cleanup_let_star_val:
                    SL_GC_REMOVE_ROOT(&init_val);                                 // Unroot value after potential use/binding
                    if (result == SL_OUT_OF_MEMORY_ERROR) goto cleanup_let_star;  // Handle OOM from env_create

                    current_binding_node = sl_cdr(current_binding_node);
                }  // End while bindings

                // --- Evaluate body in the final let_star_env ---
                SL_GC_REMOVE_ROOT(&current_binding_node);
                SL_GC_REMOVE_ROOT(&bindings);
                SL_GC_REMOVE_ROOT(&args);

                sl_object *temp_res = eval_sequence_with_defines(body_list, let_star_env, &obj, &env);
                SL_GC_REMOVE_ROOT(&let_star_env);
                SL_GC_REMOVE_ROOT(&body_list);
                if (temp_res == SL_CONTINUE_EVAL) {
                    // result = temp_res;  // Keep signal
                    goto top_of_eval;
                }
                SL_GC_REMOVE_ROOT(&result);  // Remove old root
                result = temp_res;           // Assign new result
                SL_GC_ADD_ROOT(&result);     // Add new root
                                             // Otherwise, result holds the final value or error

            cleanup_let_star:  // Error/cleanup path
                SL_GC_REMOVE_ROOT(&current_binding_node);
                SL_GC_REMOVE_ROOT(&let_star_env);
                SL_GC_REMOVE_ROOT(&body_list);
                SL_GC_REMOVE_ROOT(&bindings);
                SL_GC_REMOVE_ROOT(&args);
                break;  // Break from switch case 'let*'
            }  // End LET* block

            // --- LETREC* --- <<< NEW BLOCK
            else if (strcmp(op_name, "letrec*") == 0) {
                // (letrec* bindings body...)
                SL_GC_ADD_ROOT(&args);  // Root the rest of the form

                if (args == SL_NIL || !sl_is_pair(args)) {
                    result = sl_make_errorf("Eval: Malformed letrec* (missing bindings/body)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                sl_object *bindings = sl_car(args);
                sl_object *body_list = sl_cdr(args);

                if (!sl_is_list(bindings)) {
                    result = sl_make_errorf("Eval: Malformed letrec* bindings (not a list)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }
                if (body_list == SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed letrec* (missing body)");
                    SL_GC_REMOVE_ROOT(&args);
                    break;
                }

                SL_GC_ADD_ROOT(&bindings);
                SL_GC_ADD_ROOT(&body_list);

                // --- Create new environment ---
                sl_object *letrec_env = sl_env_create(env);
                CHECK_ALLOC_GOTO(letrec_env, cleanup_letrec, result);
                SL_GC_ADD_ROOT(&letrec_env);

                // --- Pass 1: Create placeholders ---
                sl_object *current_binding_node = bindings;
                SL_GC_ADD_ROOT(&current_binding_node);
                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) { /* error handled below */
                        break;
                    }
                    sl_object *binding_pair = sl_car(current_binding_node);
                    if (!sl_is_pair(binding_pair) || !sl_is_pair(sl_cdr(binding_pair))) { /* error handled below */
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    if (!sl_is_symbol(var_sym)) { /* error handled below */
                        break;
                    }

                    // Define with placeholder
                    sl_env_define(letrec_env, var_sym, SL_UNDEFINED);
                    // Check OOM?

                    current_binding_node = sl_cdr(current_binding_node);
                }
                SL_GC_REMOVE_ROOT(&current_binding_node);  // Unroot traversal pointer

                // --- Pass 2: Evaluate initializers sequentially and update bindings ---
                current_binding_node = bindings;
                SL_GC_ADD_ROOT(&current_binding_node);
                bool init_ok = true;
                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) {
                        result = sl_make_errorf("Eval: Malformed letrec* bindings (improper list)");
                        init_ok = false;
                        break;
                    }
                    sl_object *binding_pair = sl_car(current_binding_node);
                    if (!sl_is_pair(binding_pair) || sl_cdr(binding_pair) == SL_NIL || !sl_is_pair(sl_cdr(binding_pair)) || sl_cdr(sl_cdr(binding_pair)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed letrec* binding pair");
                        init_ok = false;
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_pair);
                    sl_object *init_expr = sl_cadr(binding_pair);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in letrec* binding must be a symbol");
                        init_ok = false;
                        break;
                    }

                    // Evaluate init_expr in the letrec_env (placeholders are visible)
                    sl_object *init_val = sl_eval(init_expr, letrec_env);  // MIGHT GC
                    SL_GC_ADD_ROOT(&init_val);

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        SL_GC_REMOVE_ROOT(&init_val);
                        init_ok = false;
                        break;
                    }

                    // Update the placeholder binding using set! semantics
                    if (!sl_env_set(letrec_env, var_sym, init_val)) {
                        // Should not happen if placeholder was created correctly
                        result = sl_make_errorf("Eval: Internal error - failed to set! letrec* variable '%s'", sl_symbol_name(var_sym));
                        SL_GC_REMOVE_ROOT(&init_val);
                        init_ok = false;
                        break;
                    }
                    SL_GC_REMOVE_ROOT(&init_val);  // Unroot value after set

                    current_binding_node = sl_cdr(current_binding_node);
                }  // End while bindings (Pass 2)
                SL_GC_REMOVE_ROOT(&current_binding_node);

                if (!init_ok) {  // Error during Pass 2?
                    goto cleanup_letrec_env;
                }

                // --- Evaluate body in the letrec_env ---
                SL_GC_REMOVE_ROOT(&bindings);
                SL_GC_REMOVE_ROOT(&args);

                sl_object *temp_res = eval_sequence_with_defines(body_list, letrec_env, &obj, &env);
                SL_GC_REMOVE_ROOT(&letrec_env);
                SL_GC_REMOVE_ROOT(&body_list);
                if (temp_res == SL_CONTINUE_EVAL) {
                    // result = temp_res;  // Keep signal
                    goto top_of_eval;
                }
                SL_GC_REMOVE_ROOT(&result);  // Remove old root
                result = temp_res;           // Assign new result
                SL_GC_ADD_ROOT(&result);     // Add new root
                                             // Otherwise, result holds the final value or error

            cleanup_letrec_env:
                SL_GC_REMOVE_ROOT(&letrec_env);
            cleanup_letrec:                                // Error/cleanup path
                SL_GC_REMOVE_ROOT(&current_binding_node);  // Ensure unrooted
                SL_GC_REMOVE_ROOT(&body_list);
                SL_GC_REMOVE_ROOT(&bindings);
                SL_GC_REMOVE_ROOT(&args);
                break;  // Break from switch case 'letrec*'
            }  // End LETREC* block

            // --- COND --- <<< ADDED BLOCK
            else if (strcmp(op_name, "cond") == 0) {
                // (cond (test1 body1...) (test2 body2...) ... (else else_body...))
                SL_GC_ADD_ROOT(&args);  // Root the list of clauses

                sl_object *current_clause_node = args;
                result = SL_NIL;  // Default result if no clause matches (unspecified)

                while (current_clause_node != SL_NIL) {
                    if (!sl_is_pair(current_clause_node)) {
                        result = sl_make_errorf("Eval: Malformed cond (improper list of clauses)");
                        goto cleanup_cond;
                    }

                    sl_object *clause = sl_car(current_clause_node);
                    sl_object *next_clause_node = sl_cdr(current_clause_node);

                    if (!sl_is_pair(clause)) {
                        result = sl_make_errorf("Eval: Malformed cond clause (not a pair)");
                        goto cleanup_cond;
                    }

                    sl_object *test_expr = sl_car(clause);
                    sl_object *body_list = sl_cdr(clause);  // List of expressions in the body

                    bool is_else_clause = false;
                    if (sl_is_symbol(test_expr) && strcmp(sl_symbol_name(test_expr), "else") == 0) {
                        is_else_clause = true;
                        // 'else' must be the last clause
                        if (next_clause_node != SL_NIL) {
                            result = sl_make_errorf("Eval: 'else' clause must be the last clause in cond");
                            goto cleanup_cond;
                        }
                    }

                    sl_object *test_result = SL_TRUE;  // Assume true for 'else' clause
                    if (!is_else_clause) {
                        // Evaluate the test expression
                        test_result = sl_eval(test_expr, env);  // MIGHT GC
                        SL_GC_ADD_ROOT(&test_result);           // Root the result

                        if (test_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(test_result)) {
                            result = test_result;  // Propagate error
                            SL_GC_REMOVE_ROOT(&test_result);
                            goto cleanup_cond;
                        }
                    }

                    // Check if test is true (anything but #f)
                    if (is_else_clause || test_result != SL_FALSE) {
                        if (!is_else_clause) {
                            SL_GC_REMOVE_ROOT(&test_result);  // Unroot test result if it was evaluated
                        }

                        // --- Evaluate the body sequence of the chosen clause ---
                        SL_GC_ADD_ROOT(&body_list);  // Root the body list

                        if (body_list == SL_NIL) {
                            // Clause like (test) - result is the test result itself
                            // If it was the else clause, test_result is SL_TRUE, which is wrong.
                            // The result should be the value of the test expression.
                            // Re-evaluating test_expr if needed, or retrieve it.
                            // For simplicity now, let's return the test_result (or SL_TRUE for else)
                            // A clause like (else) is questionable, maybe error? R5RS says unspecified.
                            // Let's return the test result if not else, NIL if else.
                            result = is_else_clause ? SL_NIL : test_result;
                            SL_GC_REMOVE_ROOT(&body_list);
                            goto cleanup_cond;  // Found the clause, evaluation done.
                        }

                        sl_object *current_body_node = body_list;
                        result = SL_NIL;  // Default if body somehow becomes empty during eval

                        while (sl_is_pair(current_body_node)) {
                            sl_object *expr_to_eval = sl_car(current_body_node);
                            sl_object *next_body_node = sl_cdr(current_body_node);

                            // Check for tail call position (last expression in the body)
                            if (next_body_node == SL_NIL) {
                                SL_GC_REMOVE_ROOT(&body_list);  // Unroot body list
                                SL_GC_REMOVE_ROOT(&args);       // Unroot clause list
                                obj = expr_to_eval;             // Set up for tail call
                                goto top_of_eval;               // Jump!
                            } else {
                                // Not the last expression, evaluate normally
                                SL_GC_REMOVE_ROOT(&result);           // Unroot previous result
                                result = sl_eval(expr_to_eval, env);  // MIGHT GC
                                SL_GC_ADD_ROOT(&result);              // Root intermediate result

                                if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                                    SL_GC_REMOVE_ROOT(&body_list);  // Unroot before cleanup
                                    goto cleanup_cond;              // Error occurred
                                }
                                // SL_GC_REMOVE_ROOT(&result);  // Unroot intermediate result
                            }
                            current_body_node = next_body_node;
                        }  // end while body expressions

                        // Check for improper body list
                        if (current_body_node != SL_NIL) {
                            result = sl_make_errorf("Eval: Malformed cond clause body (improper list)");
                        }
                        // If loop finished normally (e.g. empty body list initially), result is NIL.
                        // If error occurred, result holds the error.

                        SL_GC_REMOVE_ROOT(&body_list);  // Unroot body list
                        goto cleanup_cond;              // Clause handled, exit cond evaluation.

                    }  // end if test true or else

                    // Test was false, unroot test result and continue to next clause
                    if (!is_else_clause) {
                        SL_GC_REMOVE_ROOT(&test_result);
                    }
                    current_clause_node = next_clause_node;

                }  // end while clauses

            cleanup_cond:
                SL_GC_REMOVE_ROOT(&args);  // Final unroot of clause list
                break;                     // Break from switch case 'cond'
            }  // End COND block
            // --- AND --- <<< ADDED BLOCK
            else if (strcmp(op_name, "and") == 0) {
                // (and expr1 expr2 ...)
                // Short-circuits on #f. Returns last value if all true. Returns #t if no args.
                SL_GC_ADD_ROOT(&args);  // Root the list of expressions
                result = SL_TRUE;       // Default for (and)

                sl_object *current_node = args;
                while (sl_is_pair(current_node)) {
                    sl_object *expr_to_eval = sl_car(current_node);
                    sl_object *next_node = sl_cdr(current_node);

                    // Check for tail call position (last expression)
                    if (next_node == SL_NIL) {
                        SL_GC_REMOVE_ROOT(&args);  // Unroot args list
                        obj = expr_to_eval;        // Set up for tail call
                        goto top_of_eval;          // Jump!
                    } else {
                        // Not the last expression, evaluate normally
                        result = sl_eval(expr_to_eval, env);  // MIGHT GC
                        // SL_GC_ADD_ROOT(&result);              // Root intermediate result

                        if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                            SL_GC_REMOVE_ROOT(&args);  // Unroot before cleanup
                            goto cleanup;              // Error occurred
                        }

                        // Check for short-circuit condition (#f)
                        if (result == SL_FALSE) {
                            SL_GC_REMOVE_ROOT(&args);  // Unroot args list
                            goto cleanup;              // Result is already #f
                        }
                        // SL_GC_REMOVE_ROOT(&result);  // Unroot intermediate result
                    }
                    current_node = next_node;
                }

                // Check for improper list of arguments
                if (current_node != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed 'and' (improper argument list)");
                }
                // If loop finished normally (e.g. empty args), result is SL_TRUE.
                // If error occurred, result holds the error.
                // If short-circuited, result is SL_FALSE.

                SL_GC_REMOVE_ROOT(&args);  // Final unroot of args list
                break;                     // Break from switch case 'and'
            }  // End AND block
            // --- OR --- <<< ADDED BLOCK
            else if (strcmp(op_name, "or") == 0) {
                // (or expr1 expr2 ...)
                // Short-circuits on first non-#f value. Returns #f if all #f. Returns #f if no args.
                SL_GC_ADD_ROOT(&args);  // Root the list of expressions
                result = SL_FALSE;      // Default for (or)

                sl_object *current_node = args;
                while (sl_is_pair(current_node)) {
                    sl_object *expr_to_eval = sl_car(current_node);
                    sl_object *next_node = sl_cdr(current_node);

                    // Check for tail call position (last expression)
                    if (next_node == SL_NIL) {
                        SL_GC_REMOVE_ROOT(&args);  // Unroot args list
                        obj = expr_to_eval;        // Set up for tail call
                        goto top_of_eval;          // Jump!
                    } else {
                        // Not the last expression, evaluate normally
                        result = sl_eval(expr_to_eval, env);  // MIGHT GC
                        // SL_GC_ADD_ROOT(&result);              // Root intermediate result

                        if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                            SL_GC_REMOVE_ROOT(&args);  // Unroot before cleanup
                            goto cleanup;              // Error occurred
                        }

                        // Check for short-circuit condition (non-#f)
                        if (result != SL_FALSE) {
                            SL_GC_REMOVE_ROOT(&args);  // Unroot args list
                            goto cleanup;              // Result is the first truthy value
                        }
                        // SL_GC_REMOVE_ROOT(&result);  // Unroot intermediate result (it was #f)
                    }
                    current_node = next_node;
                }

                // Check for improper list of arguments
                if (current_node != SL_NIL) {
                    result = sl_make_errorf("Eval: Malformed 'or' (improper argument list)");
                }
                // If loop finished normally (e.g. empty args), result is SL_FALSE.
                // If error occurred, result holds the error.
                // If short-circuited, result holds the first truthy value.

                SL_GC_REMOVE_ROOT(&args);  // Final unroot of args list
                break;                     // Break from switch case 'or'
            }  // End OR block
            // --- DO --- <<< NEW BLOCK
            else if (strcmp(op_name, "do") == 0) {
                // (do ((var1 init1 step1) ...) (test result ...) command ...)
                SL_GC_ADD_ROOT(&args);  // Root the rest of the form

                // --- 1. Parse Structure ---
                if (!sl_is_pair(args) || !sl_is_pair(sl_cdr(args))) {
                    result = sl_make_errorf("Eval: Malformed do (missing bindings or test/result)");
                    goto cleanup_do;
                }
                sl_object *bindings = sl_car(args);
                sl_object *test_result_pair = sl_cadr(args);
                sl_object *commands = sl_cddr(args);  // Can be NIL

                if (!sl_is_list(bindings)) {
                    result = sl_make_errorf("Eval: Malformed do bindings (not a list)");
                    goto cleanup_do;
                }
                if (!sl_is_pair(test_result_pair)) {
                    result = sl_make_errorf("Eval: Malformed do test/result pair");
                    goto cleanup_do;
                }
                if (!sl_is_list(commands)) {  // commands can be NIL, sl_is_list handles that
                    result = sl_make_errorf("Eval: Malformed do commands (improper list)");
                    goto cleanup_do;
                }

                sl_object *test_expr = sl_car(test_result_pair);
                sl_object *result_exprs = sl_cdr(test_result_pair);  // List of result expressions

                // Root parsed parts
                SL_GC_ADD_ROOT(&bindings);
                SL_GC_ADD_ROOT(&test_expr);
                SL_GC_ADD_ROOT(&result_exprs);
                SL_GC_ADD_ROOT(&commands);

                // --- 2. Process Bindings & Evaluate Inits ---
                sl_object *vars_list = SL_NIL;
                sl_object *steps_list = SL_NIL;
                sl_object *init_vals_list = SL_NIL;
                // --- NEW: Pointers TO the last node added ---
                sl_object *vars_tail_node = SL_NIL;
                sl_object *steps_tail_node = SL_NIL;
                sl_object *inits_tail_node = SL_NIL;
                sl_object *current_binding_node = bindings;
                bool init_ok = true;

                SL_GC_ADD_ROOT(&vars_list);
                SL_GC_ADD_ROOT(&steps_list);
                SL_GC_ADD_ROOT(&init_vals_list);
                // --- NEW: Root tail node pointers ---
                SL_GC_ADD_ROOT(&vars_tail_node);
                SL_GC_ADD_ROOT(&steps_tail_node);
                SL_GC_ADD_ROOT(&inits_tail_node);
                // ---
                SL_GC_ADD_ROOT(&current_binding_node);

                while (current_binding_node != SL_NIL) {
                    if (!sl_is_pair(current_binding_node)) { /* caught by sl_is_list */
                        break;
                    }
                    sl_object *binding_spec = sl_car(current_binding_node);

                    // Validate binding spec: (var init step)
                    if (!sl_is_pair(binding_spec) || !sl_is_pair(sl_cdr(binding_spec)) || !sl_is_pair(sl_cddr(binding_spec)) || sl_cdr(sl_cddr(binding_spec)) != SL_NIL) {
                        result = sl_make_errorf("Eval: Malformed do binding spec (should be list of 3 elements)");
                        init_ok = false;
                        break;
                    }
                    sl_object *var_sym = sl_car(binding_spec);
                    sl_object *init_expr = sl_cadr(binding_spec);
                    sl_object *step_expr = sl_caddr(binding_spec);

                    if (!sl_is_symbol(var_sym)) {
                        result = sl_make_errorf("Eval: Variable in do binding must be a symbol");
                        init_ok = false;
                        break;
                    }

                    // Evaluate init_expr in the *outer* environment (env)
                    sl_object *init_val = sl_eval(init_expr, env);  // MIGHT GC
                    SL_GC_ADD_ROOT(&init_val);

                    if (init_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(init_val)) {
                        result = init_val;  // Propagate error
                        SL_GC_REMOVE_ROOT(&init_val);
                        init_ok = false;
                        break;
                    }

                    // --- REVISED: Append var ---
                    sl_object *new_var_node = sl_make_pair(var_sym, SL_NIL);
                    if (!new_var_node || new_var_node == SL_OUT_OF_MEMORY_ERROR) {
                        result = SL_OUT_OF_MEMORY_ERROR;
                        init_ok = false;
                        SL_GC_REMOVE_ROOT(&init_val);
                        break;
                    }
                    SL_GC_ADD_ROOT(&new_var_node);  // Root new node before potential modification/next alloc
                    if (vars_list == SL_NIL) {
                        vars_list = new_var_node;  // First node
                    } else {
                        sl_set_cdr(vars_tail_node, new_var_node);  // Link previous tail to new node
                    }
                    vars_tail_node = new_var_node;     // Update tail node pointer
                    SL_GC_REMOVE_ROOT(&new_var_node);  // Unroot (safe in list or via tail pointer)

                    // --- REVISED: Append step ---
                    sl_object *new_step_node = sl_make_pair(step_expr, SL_NIL);
                    if (!new_step_node || new_step_node == SL_OUT_OF_MEMORY_ERROR) {
                        result = SL_OUT_OF_MEMORY_ERROR;
                        init_ok = false;
                        SL_GC_REMOVE_ROOT(&init_val);
                        break;
                    }
                    SL_GC_ADD_ROOT(&new_step_node);
                    if (steps_list == SL_NIL) {
                        steps_list = new_step_node;
                    } else {
                        sl_set_cdr(steps_tail_node, new_step_node);
                    }
                    steps_tail_node = new_step_node;
                    SL_GC_REMOVE_ROOT(&new_step_node);

                    // --- REVISED: Append init_val ---
                    sl_object *new_init_node = sl_make_pair(init_val, SL_NIL);
                    if (!new_init_node || new_init_node == SL_OUT_OF_MEMORY_ERROR) {
                        result = SL_OUT_OF_MEMORY_ERROR;
                        init_ok = false;
                        SL_GC_REMOVE_ROOT(&init_val);
                        break;
                    }
                    SL_GC_ADD_ROOT(&new_init_node);
                    if (init_vals_list == SL_NIL) {
                        init_vals_list = new_init_node;
                    } else {
                        sl_set_cdr(inits_tail_node, new_init_node);
                    }
                    inits_tail_node = new_init_node;
                    SL_GC_REMOVE_ROOT(&new_init_node);

                    // --- End Appends ---

                    SL_GC_REMOVE_ROOT(&init_val);  // Value is safe in init_vals_list
                    current_binding_node = sl_cdr(current_binding_node);
                }
                SL_GC_REMOVE_ROOT(&current_binding_node);

                // --- NEW: Remove roots for tail node pointers ---
                SL_GC_REMOVE_ROOT(&vars_tail_node);
                SL_GC_REMOVE_ROOT(&steps_tail_node);
                SL_GC_REMOVE_ROOT(&inits_tail_node);
                // ---

                if (!init_ok) { goto cleanup_do_state; }  // Error during init eval or list building

                // --- 3. Create Loop Environment & Bind Initial Values ---
                sl_object *loop_env = sl_env_create(env);
                CHECK_ALLOC_GOTO(loop_env, cleanup_do_state, result);
                SL_GC_ADD_ROOT(&loop_env);

                sl_object *v_iter = vars_list;
                sl_object *iv_iter = init_vals_list;

                while (v_iter != SL_NIL) {
                    // Assumes lists are same length
                    sl_env_define(loop_env, sl_car(v_iter), sl_car(iv_iter));
                    // Check for OOM from define?
                    v_iter = sl_cdr(v_iter);
                    iv_iter = sl_cdr(iv_iter);
                }

                // --- 4. The Loop ---
                sl_object *step_vals_list = SL_NIL;  // Temp storage for evaluated steps
                SL_GC_ADD_ROOT(&step_vals_list);
                while (true) {
                    // a. Evaluate Test in loop_env
                    sl_object *test_result = sl_eval(test_expr, loop_env);  // MIGHT GC
                    SL_GC_ADD_ROOT(&test_result);

                    if (test_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(test_result)) {
                        result = test_result;  // Propagate error
                        SL_GC_REMOVE_ROOT(&test_result);
                        goto cleanup_do_loop;
                    }

                    // b. Check Test Result
                    if (test_result != SL_FALSE) {
                        SL_GC_REMOVE_ROOT(&test_result);  // Unroot test result

                        // Evaluate result expressions sequentially in loop_env
                        sl_object *current_res_expr_node = result_exprs;
                        result = SL_NIL;  // Default if no result exprs
                        SL_GC_ADD_ROOT(&current_res_expr_node);

                        while (current_res_expr_node != SL_NIL) {
                            if (!sl_is_pair(current_res_expr_node)) {
                                result = sl_make_errorf("Eval: Malformed do result expressions (improper list)");
                                goto cleanup_do_results;
                            }
                            sl_object *res_expr = sl_car(current_res_expr_node);
                            SL_GC_REMOVE_ROOT(&result);            // Unroot previous result
                            result = sl_eval(res_expr, loop_env);  // MIGHT GC
                            SL_GC_ADD_ROOT(&result);               // Root new result

                            if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
                                goto cleanup_do_results;  // Propagate error
                            }
                            current_res_expr_node = sl_cdr(current_res_expr_node);
                        }
                    cleanup_do_results:
                        SL_GC_REMOVE_ROOT(&current_res_expr_node);
                        goto cleanup_do_loop;  // Exit loop, result holds final value/error
                    }
                    SL_GC_REMOVE_ROOT(&test_result);  // Unroot test result (#f)

                    // c. Execute Commands in loop_env
                    sl_object *current_cmd_node = commands;
                    SL_GC_ADD_ROOT(&current_cmd_node);
                    while (current_cmd_node != SL_NIL) {
                        if (!sl_is_pair(current_cmd_node)) { /* Handled by initial check */
                            break;
                        }
                        sl_object *cmd_expr = sl_car(current_cmd_node);
                        sl_object *cmd_result = sl_eval(cmd_expr, loop_env);  // MIGHT GC
                        SL_GC_ADD_ROOT(&cmd_result);                          // Root intermediate result

                        if (cmd_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(cmd_result)) {
                            result = cmd_result;  // Propagate error
                            SL_GC_REMOVE_ROOT(&cmd_result);
                            SL_GC_REMOVE_ROOT(&current_cmd_node);
                            goto cleanup_do_loop;
                        }
                        SL_GC_REMOVE_ROOT(&cmd_result);  // Discard command result
                        current_cmd_node = sl_cdr(current_cmd_node);
                    }
                    SL_GC_REMOVE_ROOT(&current_cmd_node);

                    // d. Evaluate Steps in loop_env (store temporarily)
                    step_vals_list = SL_NIL;  // Reset temp list
                    // sl_object **step_vals_tail = &step_vals_list;
                    sl_object *step_vals_tail_node = SL_NIL;  // <<< NEW: Variable holding pointer to tail node
                    sl_object *current_step_expr_node = steps_list;

                    sl_object *v_iter_debug = vars_list;   // Add parallel iterator for var names
                    SL_GC_ADD_ROOT(&v_iter_debug);         // Root it
                    SL_GC_ADD_ROOT(&step_vals_tail_node);  // <<< NEW: Root the tail node pointer variable

                    SL_GC_ADD_ROOT(&current_step_expr_node);
                    bool step_ok = true;

                    while (current_step_expr_node != SL_NIL) {
                        sl_object *step_expr = sl_car(current_step_expr_node);
                        const char *current_var_name = sl_is_pair(v_iter_debug) ? sl_symbol_name(sl_car(v_iter_debug)) : "???";

                        sl_object *step_val = sl_eval(step_expr, loop_env);  // <<< EVALUATE STEP

                        SL_GC_ADD_ROOT(&step_val);

                        if (step_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(step_val)) {
                            result = step_val;  // Propagate error
                            SL_GC_REMOVE_ROOT(&step_val);
                            step_ok = false;
                            break;
                        }

                        if (!append_to_list(&step_vals_list, &step_vals_tail_node, step_val)) {
                            result = sl_make_errorf("Eval: OOM building do step values");
                            SL_GC_REMOVE_ROOT(&step_val);
                            step_ok = false;
                            break;
                        }
                        SL_GC_REMOVE_ROOT(&step_val);  // Value safe in step_vals_list
                        current_step_expr_node = sl_cdr(current_step_expr_node);
                    }
                    SL_GC_REMOVE_ROOT(&current_step_expr_node);
                    SL_GC_REMOVE_ROOT(&v_iter_debug);  // <<< ADD THIS LINE
                    SL_GC_REMOVE_ROOT(&step_vals_tail_node);

                    if (!step_ok) { goto cleanup_do_loop; }  // Error during step eval

                    // e. Update Bindings Simultaneously
                    v_iter = vars_list;
                    sl_object *sv_iter = step_vals_list;
                    while (v_iter != SL_NIL) {  // Assumes lists are same length

                        if (!sl_env_set(loop_env, sl_car(v_iter), sl_car(sv_iter))) {
                            // Should not happen if vars were defined correctly
                            result = sl_make_errorf("Eval: Internal error - failed to set! do variable '%s'", sl_symbol_name(sl_car(v_iter)));
                            goto cleanup_do_loop;
                        }

                        v_iter = sl_cdr(v_iter);
                        sv_iter = sl_cdr(sv_iter);
                    }
                    // Loop continues (go back to step 4a)
                }  // End while(true) loop

            cleanup_do_loop:
                SL_GC_REMOVE_ROOT(&step_vals_list);
                SL_GC_REMOVE_ROOT(&loop_env);
            cleanup_do_state:
                SL_GC_REMOVE_ROOT(&init_vals_list);
                SL_GC_REMOVE_ROOT(&steps_list);
                SL_GC_REMOVE_ROOT(&vars_list);
                SL_GC_REMOVE_ROOT(&commands);
                SL_GC_REMOVE_ROOT(&result_exprs);
                SL_GC_REMOVE_ROOT(&test_expr);
                SL_GC_REMOVE_ROOT(&bindings);
            cleanup_do:
                SL_GC_REMOVE_ROOT(&args);
                break;  // Break from switch case 'do'
            }  // End DO block

            // --- END OF SPECIAL FORMS ---
            // If none of the above matched, it's not a special form we handle here.
            // Fall through to the function call logic below.

        }  // end if (sl_is_symbol(op_obj))

        // --- Generic Function Call ---
        // If op_obj wasn't a symbol or wasn't a recognized special form symbol.
        // SL_GC_ADD_ROOT(&op_obj);  // Root operator object itself, op_obj is car(obj) which is rooted already
        // SL_GC_ADD_ROOT(&args);    // Root arguments list

        sl_object *fn = sl_eval(op_obj, env);  // Evaluate the operator - MIGHT GC
        SL_GC_ADD_ROOT(&fn);                   // Root the resulting function object

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
        SL_GC_ADD_ROOT(&evaled_args);                      // Root the evaluated argument list

        // --- ADDED CHECK for errors from sl_eval_list ---
        if (evaled_args == SL_OUT_OF_MEMORY_ERROR || sl_is_error(evaled_args)) {
            result = evaled_args;  // Propagate the error from argument evaluation
            // Skip sl_apply, go directly to cleanup for evaled_args
            goto cleanup_args_call;
        }
        // --- END ADDED CHECK ---

        // Apply the function
        // Pass the main loop's state pointers (&obj, &env) to sl_apply
        result = sl_apply(fn, evaled_args, &obj, &env);  // <<< MODIFIED CALL

        // --- ADDED CHECK: Handle TCO signal from sl_apply ---
        if (result == SL_CONTINUE_EVAL) {
            // sl_apply signaled that a tail call is needed.
            // eval_sequence_with_defines (called by sl_apply) already updated obj and env.
            // Unroot locals specific to this path before jumping.
            SL_GC_REMOVE_ROOT(&evaled_args);  // removed with macro intro
            SL_GC_REMOVE_ROOT(&fn);
            // SL_GC_REMOVE_ROOT(&op_obj);
            // SL_GC_REMOVE_ROOT(&args);
            goto top_of_eval;  // Jump back to the main loop start
        }
        // --- END ADDED CHECK ---
        // Otherwise, result holds the final value or an error from sl_apply

    cleanup_args_call:
        SL_GC_REMOVE_ROOT(&evaled_args);
    cleanup_fn_call:
        SL_GC_REMOVE_ROOT(&fn);
        // Also unroot op_obj and args used in this path
        // SL_GC_REMOVE_ROOT(&op_obj);  // removed with macro intro
        // SL_GC_REMOVE_ROOT(&args);
        break;  // Break from switch case SL_TYPE_PAIR

    }  // end case SL_TYPE_PAIR

    default:
        result = sl_make_errorf("Eval: Unknown object type %d", obj->type);
        break;

    }  // end switch

cleanup:
    // --- Unroot locals before returning ---
    SL_GC_REMOVE_ROOT(&result);
    SL_GC_REMOVE_ROOT(&env);
    SL_GC_REMOVE_ROOT(&obj);
    return result;
}

// Helper to evaluate a sequence of expressions (like in begin or function body)
// Returns the result of the *last* expression, or an error.
// Handles TCO by jumping to top_of_eval in the caller (sl_eval).
// NOTE: This helper is NOT used in the current 'let' implementation above,
// the logic was integrated directly for TCO via goto top_of_eval.
// Keeping the signature here for potential future refactoring.
// static sl_object *eval_sequence(sl_object *seq, sl_object *env) { ... }

// Helper to evaluate a list of arguments
sl_object *sl_eval_list(sl_object *list, sl_object *env) {
    if (list == SL_NIL) return SL_NIL;
    if (!sl_is_pair(list)) return sl_make_errorf("Eval: Invalid argument list structure (not a pair)");

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    sl_object *current_expr_node = list;  // Traversal node
    sl_object *evaled_arg = SL_NIL;       // Slot for evaluated arg
    sl_object *new_pair = SL_NIL;         // Slot for newly created pair
    sl_object *return_value = SL_NIL;     // Final return

    // Root key variables
    SL_GC_ADD_ROOT(&head);               // Protect the list being built
    SL_GC_ADD_ROOT(&env);                // Protect the environment
    SL_GC_ADD_ROOT(&current_expr_node);  // Protect traversal position
    SL_GC_ADD_ROOT(&evaled_arg);         // Protect intermediate eval result
    SL_GC_ADD_ROOT(&new_pair);           // Protect intermediate allocation

    while (current_expr_node != SL_NIL) {
        if (!sl_is_pair(current_expr_node)) {
            return_value = sl_make_errorf("Eval: Improper argument list (dotted list?)");
            goto cleanup_eval_list;
        }
        sl_object *arg_expr = sl_car(current_expr_node);

        // Evaluate argument - Overwrites rooted evaled_arg slot
        evaled_arg = sl_eval(arg_expr, env);
        if (evaled_arg == SL_OUT_OF_MEMORY_ERROR || sl_is_error(evaled_arg)) {
            return_value = evaled_arg;  // Propagate error
            goto cleanup_eval_list;
        }

        // Create new pair - Overwrites rooted new_pair slot
        new_pair = sl_make_pair(evaled_arg, SL_NIL);
        if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
            return_value = new_pair;  // Propagate error
            goto cleanup_eval_list;
        }

        // Append to list (new_pair is protected by its root)
        *tail_ptr = new_pair;
        tail_ptr = &new_pair->data.pair.cdr;

        // Advance
        current_expr_node = sl_cdr(current_expr_node);
    }

    return_value = head;  // Success

cleanup_eval_list:
    // Unroot all local roots
    SL_GC_REMOVE_ROOT(&new_pair);
    SL_GC_REMOVE_ROOT(&evaled_arg);
    SL_GC_REMOVE_ROOT(&current_expr_node);
    SL_GC_REMOVE_ROOT(&env);
    SL_GC_REMOVE_ROOT(&head);
    return return_value;
}

// sl_object *sl_apply(sl_object *fn, sl_object *args) {
sl_object *sl_apply(sl_object *fn, sl_object *args, sl_object **obj_ptr, sl_object **env_ptr) {
    // --- Check for error during argument evaluation ---
    if (args == SL_OUT_OF_MEMORY_ERROR || sl_is_error(args)) {
        // fprintf(stderr, "[DEBUG sl_apply] Error detected in evaluated arguments, propagating directly.\n"); // Optional debug
        return args;  // Propagate the error returned by sl_eval_list
    }

    if (!sl_is_function(fn)) {
        char *fn_str = sl_object_to_string(fn);
        sl_object *err = sl_make_errorf("Apply: Not a function: %s", fn_str ? fn_str : "<?>");
        free(fn_str);
        return err;
    }
    if (!sl_is_list(args)) {
        return sl_make_errorf("Apply: Internal error - args is not a list (type: %s)", sl_type_name(args ? args->type : -1));
    }

    sl_object *result = SL_NIL;
    // --- Root key variables ---
    SL_GC_ADD_ROOT(&fn);
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&result);

    if (fn->data.function.is_builtin) {
        // Builtins cannot be tail called in this scheme. They execute and return.
        result = fn->data.function.def.builtin.func_ptr(args);
    } else {
        // Apply a user-defined closure
        sl_object *params = fn->data.function.def.closure.params;
        sl_object *body_list = fn->data.function.def.closure.body;
        sl_object *closure_env = fn->data.function.def.closure.env;

        // Root closure parts temporarily
        SL_GC_ADD_ROOT(&params);
        SL_GC_ADD_ROOT(&body_list);
        SL_GC_ADD_ROOT(&closure_env);

        // Create a new environment for the call
        sl_object *call_env = sl_env_create(closure_env);
        CHECK_ALLOC_GOTO(call_env, cleanup_apply_closure, result);
        SL_GC_ADD_ROOT(&call_env);

        // --- REVISED Binding Logic for Variadics ---
        sl_object *p = params;  // Current parameter specifier
        sl_object *a = args;    // Current argument list node
        bool bind_ok = true;
        SL_GC_ADD_ROOT(&p);  // Root traversal pointers
        SL_GC_ADD_ROOT(&a);

        if (sl_is_pair(p)) {  // Case 1 & 2: Proper or dotted list parameters (e.g., (a b) or (a b . rest))
            while (sl_is_pair(p) && sl_is_pair(a)) {
                // Bind one required parameter
                sl_object *param_sym = sl_car(p);
                if (!sl_is_symbol(param_sym)) {
                    result = sl_make_errorf("Apply: Parameter list contains non-symbol");
                    bind_ok = false;
                    break;
                }
                sl_env_define(call_env, param_sym, sl_car(a));
                // Check OOM?
                p = sl_cdr(p);
                a = sl_cdr(a);
            }

            if (!bind_ok) goto binding_done;  // Error occurred in loop

            if (sl_is_symbol(p)) {  // Case 2: Dotted list (e.g., (a b . rest)) - p is now the 'rest' symbol
                sl_object *rest_sym = p;
                // 'a' now points to the list node containing the first rest argument, or NIL if none.
                // The rest arguments are already evaluated and form a proper list.
                sl_env_define(call_env, rest_sym, a);  // Bind rest symbol to the remaining args list
                // Check OOM?
                // Arity check: Already consumed required args. Any number of rest args (>=0) is fine.
            } else if (p == SL_NIL && a == SL_NIL) {  // Case 1: Proper list, exact match
                // Correct number of arguments provided. Binding complete.
            } else {  // Case 1: Proper list, mismatch
                char *fn_str = sl_object_to_string(fn);
                char *arg_str = sl_object_to_string(args);
                result = sl_make_errorf("Apply %s on %s: Mismatched argument count (expected %s, got %s)",
                                        fn_str ? fn_str : "<?>",
                                        arg_str ? arg_str : "<?>",
                                        sl_is_nil(p) ? "fewer" : "more",
                                        sl_is_nil(a) ? "fewer" : "more");
                free(fn_str);
                free(arg_str);
                bind_ok = false;
            }
        } else if (sl_is_symbol(p)) {  // Case 3: Single symbol parameter (e.g., (lambda rest ...))
            sl_object *rest_sym = p;
            // Bind the symbol to the entire list of arguments
            sl_env_define(call_env, rest_sym, args);  // 'args' is the complete list
            // Check OOM?
            // Any number of arguments (>=0) is fine.
        } else if (p == SL_NIL) {  // Case: (lambda () ...)
            if (a != SL_NIL) {     // Provided arguments but expected none
                char *fn_str = sl_object_to_string(fn);
                result = sl_make_errorf("Apply %s: Mismatched argument count (expected 0)", fn_str ? fn_str : "<?>");
                free(fn_str);
                bind_ok = false;
            }
            // Else: p is NIL and a is NIL, correct arity (0). Binding complete.
        } else {  // Invalid parameter specification (should ideally be caught at lambda creation)
            result = sl_make_errorf("Apply: Invalid parameter specification object");
            bind_ok = false;
        }

    binding_done:
        SL_GC_REMOVE_ROOT(&p);  // Unroot traversal pointers
        SL_GC_REMOVE_ROOT(&a);
        // --- End REVISED Binding Logic ---

        if (bind_ok) {
            // --- Evaluate the body sequence using the helper ---
            // Pass the main loop's obj_ptr and env_ptr down.
            // <<< CORRECTED CALL to eval_sequence_with_defines >>>
            result = eval_sequence_with_defines(body_list, call_env, obj_ptr, env_ptr);

            // --- ADDED: Check for TCO signal and cleanup ---
            if (result == SL_CONTINUE_EVAL) {
                // TCO signaled. Clean up sl_apply's locals before returning signal.
                // Cleanup closure/call specific roots
                SL_GC_REMOVE_ROOT(&call_env);
                SL_GC_REMOVE_ROOT(&params);
                SL_GC_REMOVE_ROOT(&body_list);
                SL_GC_REMOVE_ROOT(&closure_env);
                // Also clean up the main roots for fn, args, result
                SL_GC_REMOVE_ROOT(&result);  // result holds SL_CONTINUE_EVAL, safe to unroot
                SL_GC_REMOVE_ROOT(&args);
                SL_GC_REMOVE_ROOT(&fn);
                return SL_CONTINUE_EVAL;  // Propagate signal
            }
        }
        // else: result already holds the binding error

    cleanup_apply_closure:  // Label for cleanup within closure apply path
        SL_GC_REMOVE_ROOT(&call_env);
        SL_GC_REMOVE_ROOT(&params);
        SL_GC_REMOVE_ROOT(&body_list);
        SL_GC_REMOVE_ROOT(&closure_env);
    }

cleanup_apply:
    SL_GC_REMOVE_ROOT(&result);
    SL_GC_REMOVE_ROOT(&args);
    SL_GC_REMOVE_ROOT(&fn);
    // Return the final value, an error, OR SL_CONTINUE_EVAL
    return result;
}

// Parses and evaluates all S-expressions from the given null-terminated string
sl_object *sl_eval_string(const char *input, sl_object *env) {
    sl_object *last_result = SL_NIL;  // Default result if string is empty/only whitespace
    sl_object *expr = NULL;
    const char *parse_ptr = input;
    const char *end_ptr = NULL;

    // Root environment and potentially changing last_result/expr
    SL_GC_ADD_ROOT(&env);
    SL_GC_ADD_ROOT(&last_result);
    SL_GC_ADD_ROOT(&expr);  // Root expr slot once

    while (true) {
        // --- Skip whitespace AND comments before parsing the next expression ---
        // while (isspace(*parse_ptr)) { // <<< OLD CODE
        //     parse_ptr++;
        // }
        skip_whitespace_and_comments(&parse_ptr);  // <<< CORRECTED: Use the function that handles comments

        // Check if we reached the end of the string after skipping
        if (*parse_ptr == '\0') {
            break;  // Done processing the string
        }

        // --- Parse ---
        // Store the start position in case of error
        const char *current_expr_start = parse_ptr;
        expr = sl_parse_string(parse_ptr, &end_ptr);
        // Note: expr is already rooted, sl_parse_string result overwrites the rooted slot

        if (expr == SL_NIL) {
            // Parsing failed (sl_parse_string should print details)
            // Or end of input was reached unexpectedly within parse attempt
            // Return a generic error, assuming sl_parse_string reported specifics
            last_result = sl_make_errorf("Error parsing expression starting near: %.*s", 30, current_expr_start);
            break;  // Stop processing on parse error
        }
        if (expr == SL_OUT_OF_MEMORY_ERROR) {
            last_result = expr;  // Propagate OOM
            break;
        }

        // --- Evaluate ---
        // Unroot previous result before eval potentially overwrites it via last_result
        SL_GC_REMOVE_ROOT(&last_result);
        last_result = sl_eval(expr, env);
        SL_GC_ADD_ROOT(&last_result);  // Re-root the new result

        // Check for evaluation errors
        if (last_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(last_result)) {
            // Evaluation error occurred, stop processing
            break;
        }

        // Update parse_ptr to continue after the parsed expression
        parse_ptr = end_ptr;

        // Optional: Trigger GC periodically?
        sl_gc();
    }

    // Clean up roots
    SL_GC_REMOVE_ROOT(&expr);
    SL_GC_REMOVE_ROOT(&last_result);
    SL_GC_REMOVE_ROOT(&env);

    return last_result;  // Return the result of the last evaluation or an error
}

// Reads and evaluates all S-expressions from a stream in the given environment.
// Simple implementation: Read whole stream to buffer, then call sl_eval_string.
// WARNING: Inefficient for large files. Consider true stream parsing later.
sl_object *sl_eval_stream(FILE *stream, sl_object *env) {
    // Determine file size
    fseek(stream, 0, SEEK_END);
    long file_size = ftell(stream);
    if (file_size < 0) {
        return sl_make_errorf("Error getting stream size");
    }
    fseek(stream, 0, SEEK_SET);  // Rewind

    // Allocate buffer (+1 for null terminator)
    // Check for potential overflow if file_size is huge
    if (file_size >= SIZE_MAX) {
        return sl_make_errorf("Stream too large to read into memory");
    }
    size_t buffer_size = (size_t)file_size + 1;
    char *buffer = malloc(buffer_size);
    if (!buffer) {
        return SL_OUT_OF_MEMORY_ERROR;  // Use the standard OOM error object
    }

    // Read the whole file
    size_t bytes_read = fread(buffer, 1, file_size, stream);
    if (bytes_read != (size_t)file_size) {
        free(buffer);
        // Check ferror(stream) vs feof(stream) if needed
        return sl_make_errorf("Error reading from stream");
    }
    buffer[bytes_read] = '\0';  // Null-terminate

    // Evaluate the buffer content
    sl_object *result = sl_eval_string(buffer, env);

    // Free the buffer
    free(buffer);

    return result;
}

// Loads all files ending in ".scm" from the specified directory path.
// Returns SL_TRUE on success (all files loaded without error),
// or an error object if directory cannot be opened or a file fails to load.
sl_object *sl_load_directory(const char *dir_path, sl_object *env) {
    DIR *dir;
    struct dirent *entry;
    sl_object *last_result = SL_TRUE;  // Assume success initially

    dir = opendir(dir_path);
    if (!dir) {
        return sl_make_errorf("load-directory: Cannot open directory '%s' (%s)", dir_path, strerror(errno));
    }

    // Root env and last_result
    SL_GC_ADD_ROOT(&env);
    SL_GC_ADD_ROOT(&last_result);

    while ((entry = readdir(dir)) != NULL) {
        const char *name = entry->d_name;
        size_t name_len = strlen(name);
        const char *ext = ".scm";
        size_t ext_len = strlen(ext);

        // Check if filename ends with ".scm"
        if (name_len > ext_len && strcmp(name + name_len - ext_len, ext) == 0) {
            // Construct full path
            // Need space for dir_path + '/' + name + '\0'
            size_t full_path_len = strlen(dir_path) + 1 + name_len + 1;
            char *full_path = malloc(full_path_len);
            if (!full_path) {
                last_result = SL_OUT_OF_MEMORY_ERROR;
                break;  // OOM error
            }
            // Ensure directory path doesn't end with '/' before appending
            if (dir_path[strlen(dir_path) - 1] == '/') {
                snprintf(full_path, full_path_len, "%s%s", dir_path, name);
            } else {
                snprintf(full_path, full_path_len, "%s/%s", dir_path, name);
            }

            // Check if it's actually a file (and not a directory ending in .scm)
            struct stat path_stat;
            if (stat(full_path, &path_stat) == 0 && S_ISREG(path_stat.st_mode)) {
                FILE *file = fopen(full_path, "r");
                if (!file) {
                    fprintf(stderr, "Warning: Could not open library file '%s' (%s), skipping.\n", full_path, strerror(errno));
                    // Optionally make this a hard error:
                    // last_result = sl_make_errorf("load-directory: Cannot open file '%s' (%s)", full_path, strerror(errno));
                    // free(full_path);
                    // break;
                } else {
                    printf("Loading library file: %s\n", full_path);  // Debug message
                    // Evaluate the file
                    sl_object *eval_res = sl_eval_stream(file, env);
                    fclose(file);

                    // Check for evaluation errors
                    if (eval_res == SL_OUT_OF_MEMORY_ERROR || sl_is_error(eval_res)) {
                        // Report error from file loading
                        char *err_str = sl_object_to_string(eval_res);
                        fprintf(stderr, "Error loading library file '%s': %s\n", full_path, err_str ? err_str : "Unknown error");
                        free(err_str);
                        last_result = eval_res;  // Store the error
                        free(full_path);
                        break;  // Stop loading on error
                    }
                    // Discard successful result of individual file loading
                }
            } else {
                // It's not a regular file or stat failed, skip it silently or add warning
                // fprintf(stderr, "Warning: Skipping non-regular file '%s'.\n", full_path);
            }
            free(full_path);
        }  // end if .scm
    }  // end while readdir

    closedir(dir);

    // Unroot locals
    SL_GC_REMOVE_ROOT(&last_result);
    SL_GC_REMOVE_ROOT(&env);

    // Return SL_TRUE if loop completed without break, otherwise return the error.
    return last_result;
}
