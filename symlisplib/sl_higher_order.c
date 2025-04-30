#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sl_higher_order.h"
#include "sl_core.h"
#include "sl_env.h"
#include "sl_eval.h"      // Need sl_apply
#include "sl_builtins.h"  // Need define_builtin and helpers like append_to_list

// --- Builtin Implementations ---

// (map proc list1 [list2 ...])
static sl_object *builtin_map(sl_object *args) {
    sl_object *proc = SL_NIL;
    sl_object *lists_arg = SL_NIL;  // The list containing list1, list2, ...
    sl_object *result_head = SL_NIL;
    sl_object *result_tail = SL_NIL;
    sl_object *current_nodes = SL_NIL;    // List to hold current nodes of input lists
    sl_object *apply_args_head = SL_NIL;  // List to hold args for each apply call
    sl_object *apply_args_tail = SL_NIL;
    sl_object *map_result = SL_NIL;
    sl_object *temp_item = SL_NIL;

    // Root everything
    sl_gc_add_root(&args);
    sl_gc_add_root(&proc);
    sl_gc_add_root(&lists_arg);
    sl_gc_add_root(&result_head);
    sl_gc_add_root(&result_tail);
    sl_gc_add_root(&current_nodes);
    sl_gc_add_root(&apply_args_head);
    sl_gc_add_root(&apply_args_tail);
    sl_gc_add_root(&map_result);
    sl_gc_add_root(&temp_item);

    // 1. Check arity (at least 2 args: proc + list1)
    if (!sl_is_pair(args) || sl_cdr(args) == SL_NIL) {
        map_result = sl_make_errorf("map: Expected at least 2 arguments (procedure and list)");
        goto cleanup_map;
    }

    // 2. Extract proc and validate
    proc = sl_car(args);
    if (!sl_is_function(proc)) {
        map_result = sl_make_errorf("map: First argument must be a procedure");
        goto cleanup_map;
    }
    lists_arg = sl_cdr(args);  // The rest are the lists

    // 3. Validate input lists and initialize current_nodes list
    current_nodes = SL_NIL;  // Build list of initial list nodes
    sl_object *tail_ptr = SL_NIL;
    sl_gc_add_root(&tail_ptr);  // Root the tail pointer for building current_nodes

    sl_object *list_iter = lists_arg;
    while (sl_is_pair(list_iter)) {
        sl_object *input_list = sl_car(list_iter);
        if (!sl_is_list(input_list)) {  // Check if it's a proper list
            map_result = sl_make_errorf("map: All list arguments must be proper lists");
            sl_gc_remove_root(&tail_ptr);
            goto cleanup_map;
        }
        // Add the list itself (start node) to current_nodes
        temp_item = input_list;
        if (append_to_list(&current_nodes, &tail_ptr, temp_item) == NULL) {
            map_result = sl_make_errorf("map: Failed to build internal list state (OOM?)");
            sl_gc_remove_root(&tail_ptr);
            goto cleanup_map;
        }
        list_iter = sl_cdr(list_iter);
    }
    if (list_iter != SL_NIL) {  // Check if lists_arg itself was proper
        map_result = sl_make_errorf("map: Internal error - improper argument list structure");
        sl_gc_remove_root(&tail_ptr);
        goto cleanup_map;
    }
    sl_gc_remove_root(&tail_ptr);  // Done building current_nodes

    // 4. Main mapping loop
    result_head = SL_NIL;
    result_tail = SL_NIL;

    while (true) {
        apply_args_head = SL_NIL;  // Reset for this iteration
        apply_args_tail = SL_NIL;
        bool stop_iteration = false;

        // Iterate through the current nodes of all input lists
        sl_object *node_iter = current_nodes;
        sl_object *next_nodes_head = SL_NIL;  // Build the list for the next iteration
        sl_object *next_nodes_tail = SL_NIL;
        sl_gc_add_root(&next_nodes_head);
        sl_gc_add_root(&next_nodes_tail);

        while (sl_is_pair(node_iter)) {
            sl_object *current_list_node = sl_car(node_iter);

            // Check if any list has ended
            if (!sl_is_pair(current_list_node)) {
                stop_iteration = true;
                sl_gc_remove_root(&next_nodes_tail);  // Break inner loop needs cleanup
                sl_gc_remove_root(&next_nodes_head);
                break;  // Stop processing this iteration
            }

            // Get element and add to apply_args
            temp_item = sl_car(current_list_node);
            if (append_to_list(&apply_args_head, &apply_args_tail, temp_item) == NULL) {
                map_result = sl_make_errorf("map: Failed to build apply arguments (OOM?)");
                sl_gc_remove_root(&next_nodes_tail);
                sl_gc_remove_root(&next_nodes_head);
                goto cleanup_map;
            }

            // Add the next node (cdr) to the list for the next iteration
            temp_item = sl_cdr(current_list_node);
            if (append_to_list(&next_nodes_head, &next_nodes_tail, temp_item) == NULL) {
                map_result = sl_make_errorf("map: Failed to build internal list state (OOM?)");
                sl_gc_remove_root(&next_nodes_tail);
                sl_gc_remove_root(&next_nodes_head);
                goto cleanup_map;
            }

            node_iter = sl_cdr(node_iter);
        }
        if (stop_iteration) {
            sl_gc_remove_root(&next_nodes_tail);  // Ensure cleanup if stopped early
            sl_gc_remove_root(&next_nodes_head);
            break;  // Exit the main while loop
        }

        // Update current_nodes for the next iteration
        current_nodes = next_nodes_head;      // Already rooted
        sl_gc_remove_root(&next_nodes_tail);  // Don't need tail pointer anymore
        sl_gc_remove_root(&next_nodes_head);  // Unroot head (now pointed to by current_nodes root)

        // Apply the procedure
        sl_object *apply_result = sl_apply(proc, apply_args_head, NULL, NULL);
        sl_gc_add_root(&apply_result);  // Root the result

        // Check for errors from apply
        if (apply_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(apply_result)) {
            map_result = apply_result;  // Propagate error
            sl_gc_remove_root(&apply_result);
            goto cleanup_map;
        }

        // Append result to the final result list
        temp_item = apply_result;
        if (append_to_list(&result_head, &result_tail, temp_item) == NULL) {
            map_result = sl_make_errorf("map: Failed to build result list (OOM?)");
            sl_gc_remove_root(&apply_result);
            goto cleanup_map;
        }
        sl_gc_remove_root(&apply_result);  // Unroot (now part of result_head)
    }

    map_result = result_head;  // Success

cleanup_map:
    // Unroot everything
    sl_gc_remove_root(&temp_item);
    sl_gc_remove_root(&map_result);  // Keep map_result rooted if it's the return value
    sl_gc_remove_root(&apply_args_tail);
    sl_gc_remove_root(&apply_args_head);
    sl_gc_remove_root(&current_nodes);
    sl_gc_remove_root(&result_tail);
    sl_gc_remove_root(&result_head);  // Keep result_head rooted if it's the return value
    sl_gc_remove_root(&lists_arg);
    sl_gc_remove_root(&proc);
    sl_gc_remove_root(&args);

    // Return either the result list or an error object
    return map_result == result_head ? result_head : map_result;
}

// (filter pred list)
static sl_object *builtin_filter(sl_object *args) {
    sl_object *pred = SL_NIL;
    sl_object *input_list = SL_NIL;
    sl_object *result_head = SL_NIL;
    sl_object *result_tail = SL_NIL;
    sl_object *current_node = SL_NIL;
    sl_object *apply_args = SL_NIL;  // List containing single element for apply
    sl_object *filter_result = SL_NIL;
    sl_object *temp_item = SL_NIL;

    // Root everything
    sl_gc_add_root(&args);
    sl_gc_add_root(&pred);
    sl_gc_add_root(&input_list);
    sl_gc_add_root(&result_head);
    sl_gc_add_root(&result_tail);
    sl_gc_add_root(&current_node);
    sl_gc_add_root(&apply_args);
    sl_gc_add_root(&filter_result);
    sl_gc_add_root(&temp_item);

    // 1. Check arity
    sl_object *arity_check = check_arity("filter", args, 2);
    if (arity_check != SL_TRUE) {
        filter_result = arity_check;
        goto cleanup_filter;
    }

    // 2. Extract and validate arguments
    pred = sl_car(args);
    input_list = sl_cadr(args);

    if (!sl_is_function(pred)) {
        filter_result = sl_make_errorf("filter: First argument must be a procedure");
        goto cleanup_filter;
    }
    if (!sl_is_list(input_list)) {
        filter_result = sl_make_errorf("filter: Second argument must be a proper list");
        goto cleanup_filter;
    }

    // 3. Main filtering loop
    result_head = SL_NIL;
    result_tail = SL_NIL;
    current_node = input_list;

    while (sl_is_pair(current_node)) {
        temp_item = sl_car(current_node);  // The element to test

        // Create the argument list for apply: (list element)
        apply_args = sl_make_pair(temp_item, SL_NIL);
        CHECK_ALLOC_GOTO(apply_args, cleanup_filter, filter_result);
        // apply_args is already rooted

        // Apply the predicate
        sl_object *pred_result = sl_apply(pred, apply_args, NULL, NULL);
        sl_gc_add_root(&pred_result);  // Root the result

        // Check for errors from apply
        if (pred_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(pred_result)) {
            filter_result = pred_result;  // Propagate error
            sl_gc_remove_root(&pred_result);
            goto cleanup_filter;
        }

        // If predicate result is not #f, add original element to result list
        if (pred_result != SL_FALSE) {
            // temp_item still holds the original element
            if (append_to_list(&result_head, &result_tail, temp_item) == NULL) {
                filter_result = sl_make_errorf("filter: Failed to build result list (OOM?)");
                sl_gc_remove_root(&pred_result);
                goto cleanup_filter;
            }
        }
        sl_gc_remove_root(&pred_result);  // Unroot predicate result

        current_node = sl_cdr(current_node);  // Move to next element
    }
    // Input list traversal finished (proper list checked earlier)

    filter_result = result_head;  // Success

cleanup_filter:
    // Unroot everything
    sl_gc_remove_root(&temp_item);
    sl_gc_remove_root(&filter_result);  // Keep if it's the return value
    sl_gc_remove_root(&apply_args);
    sl_gc_remove_root(&current_node);
    sl_gc_remove_root(&result_tail);
    sl_gc_remove_root(&result_head);  // Keep if it's the return value
    sl_gc_remove_root(&input_list);
    sl_gc_remove_root(&pred);
    sl_gc_remove_root(&args);

    // Return either the result list or an error object
    return filter_result == result_head ? result_head : filter_result;
}

// --- Registration ---

void sl_register_higher_order_primitives(sl_object *env) {
    define_builtin(env, "map", builtin_map);
    define_builtin(env, "filter", builtin_filter);
    // Add fold-left, fold-right, for-each etc. here later
}