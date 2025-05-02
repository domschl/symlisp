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
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&proc);
    SL_GC_ADD_ROOT(&lists_arg);
    SL_GC_ADD_ROOT(&result_head);
    SL_GC_ADD_ROOT(&result_tail);
    SL_GC_ADD_ROOT(&current_nodes);
    SL_GC_ADD_ROOT(&apply_args_head);
    SL_GC_ADD_ROOT(&apply_args_tail);
    SL_GC_ADD_ROOT(&map_result);
    SL_GC_ADD_ROOT(&temp_item);

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
    SL_GC_ADD_ROOT(&tail_ptr);  // Root the tail pointer for building current_nodes

    sl_object *list_iter = lists_arg;
    while (sl_is_pair(list_iter)) {
        sl_object *input_list = sl_car(list_iter);
        if (!sl_is_list(input_list)) {  // Check if it's a proper list
            map_result = sl_make_errorf("map: All list arguments must be proper lists");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_map;
        }
        // Add the list itself (start node) to current_nodes
        temp_item = input_list;
        if (append_to_list(&current_nodes, &tail_ptr, temp_item) == NULL) {
            map_result = sl_make_errorf("map: Failed to build internal list state (OOM?)");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_map;
        }
        list_iter = sl_cdr(list_iter);
    }
    if (list_iter != SL_NIL) {  // Check if lists_arg itself was proper
        map_result = sl_make_errorf("map: Internal error - improper argument list structure");
        SL_GC_REMOVE_ROOT(&tail_ptr);
        goto cleanup_map;
    }
    SL_GC_REMOVE_ROOT(&tail_ptr);  // Done building current_nodes

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
        SL_GC_ADD_ROOT(&next_nodes_head);
        SL_GC_ADD_ROOT(&next_nodes_tail);

        while (sl_is_pair(node_iter)) {
            sl_object *current_list_node = sl_car(node_iter);

            // Check if any list has ended
            if (!sl_is_pair(current_list_node)) {
                stop_iteration = true;
                // SL_GC_REMOVE_ROOT(&next_nodes_tail);  // Break inner loop needs cleanup
                // SL_GC_REMOVE_ROOT(&next_nodes_head);
                break;  // Stop processing this iteration
            }

            // Get element and add to apply_args
            temp_item = sl_car(current_list_node);
            if (append_to_list(&apply_args_head, &apply_args_tail, temp_item) == NULL) {
                map_result = sl_make_errorf("map: Failed to build apply arguments (OOM?)");
                SL_GC_REMOVE_ROOT(&next_nodes_tail);
                SL_GC_REMOVE_ROOT(&next_nodes_head);
                goto cleanup_map;
            }

            // Add the next node (cdr) to the list for the next iteration
            temp_item = sl_cdr(current_list_node);
            if (append_to_list(&next_nodes_head, &next_nodes_tail, temp_item) == NULL) {
                map_result = sl_make_errorf("map: Failed to build internal list state (OOM?)");
                SL_GC_REMOVE_ROOT(&next_nodes_tail);
                SL_GC_REMOVE_ROOT(&next_nodes_head);
                goto cleanup_map;
            }

            node_iter = sl_cdr(node_iter);
        }
        if (stop_iteration) {
            SL_GC_REMOVE_ROOT(&next_nodes_tail);  // Ensure cleanup if stopped early
            SL_GC_REMOVE_ROOT(&next_nodes_head);
            break;  // Exit the main while loop
        }

        // Update current_nodes for the next iteration
        current_nodes = next_nodes_head;      // Already rooted
        SL_GC_REMOVE_ROOT(&next_nodes_tail);  // Don't need tail pointer anymore
        SL_GC_REMOVE_ROOT(&next_nodes_head);  // Unroot head (now pointed to by current_nodes root)

        // Apply the procedure
        sl_object *apply_result = sl_apply(proc, apply_args_head, NULL, NULL);
        SL_GC_ADD_ROOT(&apply_result);  // Root the result

        // Check for errors from apply
        if (apply_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(apply_result)) {
            map_result = apply_result;  // Propagate error
            SL_GC_REMOVE_ROOT(&apply_result);
            goto cleanup_map;
        }

        // Append result to the final result list
        temp_item = apply_result;
        if (append_to_list(&result_head, &result_tail, temp_item) == NULL) {
            map_result = sl_make_errorf("map: Failed to build result list (OOM?)");
            SL_GC_REMOVE_ROOT(&apply_result);
            goto cleanup_map;
        }
        SL_GC_REMOVE_ROOT(&apply_result);  // Unroot (now part of result_head)
    }

    map_result = result_head;  // Success

cleanup_map:
    // Unroot everything
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&map_result);  // Keep map_result rooted if it's the return value
    SL_GC_REMOVE_ROOT(&apply_args_tail);
    SL_GC_REMOVE_ROOT(&apply_args_head);
    SL_GC_REMOVE_ROOT(&current_nodes);
    SL_GC_REMOVE_ROOT(&result_tail);
    SL_GC_REMOVE_ROOT(&result_head);  // Keep result_head rooted if it's the return value
    SL_GC_REMOVE_ROOT(&lists_arg);
    SL_GC_REMOVE_ROOT(&proc);
    SL_GC_REMOVE_ROOT(&args);

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
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&pred);
    SL_GC_ADD_ROOT(&input_list);
    SL_GC_ADD_ROOT(&result_head);
    SL_GC_ADD_ROOT(&result_tail);
    SL_GC_ADD_ROOT(&current_node);
    SL_GC_ADD_ROOT(&apply_args);
    SL_GC_ADD_ROOT(&filter_result);
    SL_GC_ADD_ROOT(&temp_item);

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
        SL_GC_ADD_ROOT(&pred_result);  // Root the result

        // Check for errors from apply
        if (pred_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(pred_result)) {
            filter_result = pred_result;  // Propagate error
            SL_GC_REMOVE_ROOT(&pred_result);
            goto cleanup_filter;
        }

        // If predicate result is not #f, add original element to result list
        if (pred_result != SL_FALSE) {
            // temp_item still holds the original element
            if (append_to_list(&result_head, &result_tail, temp_item) == NULL) {
                filter_result = sl_make_errorf("filter: Failed to build result list (OOM?)");
                SL_GC_REMOVE_ROOT(&pred_result);
                goto cleanup_filter;
            }
        }
        SL_GC_REMOVE_ROOT(&pred_result);  // Unroot predicate result

        current_node = sl_cdr(current_node);  // Move to next element
    }
    // Input list traversal finished (proper list checked earlier)

    filter_result = result_head;  // Success

cleanup_filter:
    // Unroot everything
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&filter_result);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&apply_args);
    SL_GC_REMOVE_ROOT(&current_node);
    SL_GC_REMOVE_ROOT(&result_tail);
    SL_GC_REMOVE_ROOT(&result_head);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&input_list);
    SL_GC_REMOVE_ROOT(&pred);
    SL_GC_REMOVE_ROOT(&args);

    // Return either the result list or an error object
    return filter_result == result_head ? result_head : filter_result;
}

// (for-each proc list1 [list2 ...])
static sl_object *builtin_for_each(sl_object *args) {
    sl_object *proc = SL_NIL;
    sl_object *lists_arg = SL_NIL;
    sl_object *current_nodes = SL_NIL;    // List to hold current nodes of input lists
    sl_object *apply_args_head = SL_NIL;  // List to hold args for each apply call
    sl_object *apply_args_tail = SL_NIL;
    sl_object *fe_result = SL_NIL;  // Final result (unspecified -> NIL)
    sl_object *temp_item = SL_NIL;

    // Root everything
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&proc);
    SL_GC_ADD_ROOT(&lists_arg);
    SL_GC_ADD_ROOT(&current_nodes);
    SL_GC_ADD_ROOT(&apply_args_head);
    SL_GC_ADD_ROOT(&apply_args_tail);
    SL_GC_ADD_ROOT(&fe_result);
    SL_GC_ADD_ROOT(&temp_item);

    // 1. Check arity (at least 2 args: proc + list1)
    if (!sl_is_pair(args) || sl_cdr(args) == SL_NIL) {
        fe_result = sl_make_errorf("for-each: Expected at least 2 arguments (procedure and list)");
        goto cleanup_for_each;
    }

    // 2. Extract proc and validate
    proc = sl_car(args);
    if (!sl_is_function(proc)) {
        fe_result = sl_make_errorf("for-each: First argument must be a procedure");
        goto cleanup_for_each;
    }
    lists_arg = sl_cdr(args);  // The rest are the lists

    // 3. Validate input lists and initialize current_nodes list
    current_nodes = SL_NIL;
    sl_object *tail_ptr = SL_NIL;
    SL_GC_ADD_ROOT(&tail_ptr);

    sl_object *list_iter = lists_arg;
    while (sl_is_pair(list_iter)) {
        sl_object *input_list = sl_car(list_iter);
        if (!sl_is_list(input_list)) {
            fe_result = sl_make_errorf("for-each: All list arguments must be proper lists");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_for_each;
        }
        temp_item = input_list;
        if (append_to_list(&current_nodes, &tail_ptr, temp_item) == NULL) {
            fe_result = sl_make_errorf("for-each: Failed to build internal list state (OOM?)");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_for_each;
        }
        list_iter = sl_cdr(list_iter);
    }
    if (list_iter != SL_NIL) {
        fe_result = sl_make_errorf("for-each: Internal error - improper argument list structure");
        SL_GC_REMOVE_ROOT(&tail_ptr);
        goto cleanup_for_each;
    }
    SL_GC_REMOVE_ROOT(&tail_ptr);

    // 4. Main loop
    while (true) {
        apply_args_head = SL_NIL;
        apply_args_tail = SL_NIL;
        bool stop_iteration = false;

        sl_object *node_iter = current_nodes;
        sl_object *next_nodes_head = SL_NIL;
        sl_object *next_nodes_tail = SL_NIL;
        SL_GC_ADD_ROOT(&next_nodes_head);
        SL_GC_ADD_ROOT(&next_nodes_tail);

        while (sl_is_pair(node_iter)) {
            sl_object *current_list_node = sl_car(node_iter);
            if (!sl_is_pair(current_list_node)) {
                stop_iteration = true;
                // SL_GC_REMOVE_ROOT(&next_nodes_tail);
                // SL_GC_REMOVE_ROOT(&next_nodes_head);
                break;
            }
            temp_item = sl_car(current_list_node);
            if (append_to_list(&apply_args_head, &apply_args_tail, temp_item) == NULL) {
                fe_result = sl_make_errorf("for-each: Failed to build apply arguments (OOM?)");
                SL_GC_REMOVE_ROOT(&next_nodes_tail);
                SL_GC_REMOVE_ROOT(&next_nodes_head);
                goto cleanup_for_each;
            }
            temp_item = sl_cdr(current_list_node);
            if (append_to_list(&next_nodes_head, &next_nodes_tail, temp_item) == NULL) {
                fe_result = sl_make_errorf("for-each: Failed to build internal list state (OOM?)");
                SL_GC_REMOVE_ROOT(&next_nodes_tail);
                SL_GC_REMOVE_ROOT(&next_nodes_head);
                goto cleanup_for_each;
            }
            node_iter = sl_cdr(node_iter);
        }
        if (stop_iteration) {
            SL_GC_REMOVE_ROOT(&next_nodes_tail);
            SL_GC_REMOVE_ROOT(&next_nodes_head);
            break;
        }

        current_nodes = next_nodes_head;
        SL_GC_REMOVE_ROOT(&next_nodes_tail);
        SL_GC_REMOVE_ROOT(&next_nodes_head);

        // Apply the procedure
        sl_object *apply_result = sl_apply(proc, apply_args_head, NULL, NULL);
        // Check for errors but discard the actual result
        if (apply_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(apply_result)) {
            fe_result = apply_result;  // Propagate error
            goto cleanup_for_each;
        }
        // Discard apply_result (GC will handle it if unreferenced)
    }

    fe_result = SL_NIL;  // Success, return unspecified value

cleanup_for_each:
    // Unroot everything
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&fe_result);
    SL_GC_REMOVE_ROOT(&apply_args_tail);
    SL_GC_REMOVE_ROOT(&apply_args_head);
    SL_GC_REMOVE_ROOT(&current_nodes);
    SL_GC_REMOVE_ROOT(&lists_arg);
    SL_GC_REMOVE_ROOT(&proc);
    SL_GC_REMOVE_ROOT(&args);
    return fe_result;
}

// (fold-left proc initial list1 [list2 ...])
// (reduce proc initial list1 [list2 ...])
static sl_object *builtin_fold_left(sl_object *args) {
    sl_object *proc = SL_NIL;
    sl_object *accumulator = SL_NIL;
    sl_object *lists_arg = SL_NIL;
    sl_object *current_nodes = SL_NIL;    // List to hold current nodes of input lists
    sl_object *apply_args_head = SL_NIL;  // List to hold args for each apply call
    sl_object *apply_args_tail = SL_NIL;
    sl_object *fold_result = SL_NIL;  // Holds final accumulator or error
    sl_object *temp_item = SL_NIL;

    // Root everything
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&proc);
    SL_GC_ADD_ROOT(&accumulator);
    SL_GC_ADD_ROOT(&lists_arg);
    SL_GC_ADD_ROOT(&current_nodes);
    SL_GC_ADD_ROOT(&apply_args_head);
    SL_GC_ADD_ROOT(&apply_args_tail);
    SL_GC_ADD_ROOT(&fold_result);
    SL_GC_ADD_ROOT(&temp_item);

    // 1. Check arity (at least 2 args: proc + initial)
    if (!sl_is_pair(args) || sl_cdr(args) == SL_NIL) {
        fold_result = sl_make_errorf("fold-left/reduce: Expected at least 2 arguments (procedure and initial value)");
        goto cleanup_fold_left;
    }

    // 2. Extract proc, initial, and lists
    proc = sl_car(args);
    accumulator = sl_cadr(args);  // Initial value
    lists_arg = sl_cddr(args);    // The rest are the lists

    if (!sl_is_function(proc)) {
        fold_result = sl_make_errorf("fold-left/reduce: First argument must be a procedure");
        goto cleanup_fold_left;
    }

    // If no lists provided, return initial value
    if (lists_arg == SL_NIL) {
        fold_result = accumulator;
        goto cleanup_fold_left;
    }

    // 3. Validate input lists and initialize current_nodes list
    current_nodes = SL_NIL;
    sl_object *tail_ptr = SL_NIL;
    SL_GC_ADD_ROOT(&tail_ptr);

    sl_object *list_iter = lists_arg;
    while (sl_is_pair(list_iter)) {
        sl_object *input_list = sl_car(list_iter);
        if (!sl_is_list(input_list)) {
            fold_result = sl_make_errorf("fold-left/reduce: All list arguments must be proper lists");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_fold_left;
        }
        temp_item = input_list;
        if (append_to_list(&current_nodes, &tail_ptr, temp_item) == NULL) {
            fold_result = sl_make_errorf("fold-left/reduce: Failed to build internal list state (OOM?)");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_fold_left;
        }
        list_iter = sl_cdr(list_iter);
    }
    if (list_iter != SL_NIL) {
        fold_result = sl_make_errorf("fold-left/reduce: Internal error - improper argument list structure");
        SL_GC_REMOVE_ROOT(&tail_ptr);
        goto cleanup_fold_left;
    }
    SL_GC_REMOVE_ROOT(&tail_ptr);

    // 4. Main folding loop
    while (true) {
        apply_args_head = SL_NIL;  // Reset for this iteration
        apply_args_tail = SL_NIL;
        bool stop_iteration = false;

        // Build argument list for apply: (accumulator element1 element2 ...)
        // Start with accumulator
        temp_item = accumulator;  // Accumulator is already rooted
        if (append_to_list(&apply_args_head, &apply_args_tail, temp_item) == NULL) {
            fold_result = sl_make_errorf("fold-left/reduce: Failed to build apply arguments (OOM?)");
            goto cleanup_fold_left;
        }

        // Iterate through the current nodes of all input lists to get elements
        sl_object *node_iter = current_nodes;
        sl_object *next_nodes_head = SL_NIL;  // Build the list for the next iteration
        sl_object *next_nodes_tail = SL_NIL;
        SL_GC_ADD_ROOT(&next_nodes_head);
        SL_GC_ADD_ROOT(&next_nodes_tail);

        while (sl_is_pair(node_iter)) {
            sl_object *current_list_node = sl_car(node_iter);

            // Check if any list has ended
            if (!sl_is_pair(current_list_node)) {
                stop_iteration = true;
                // SL_GC_REMOVE_ROOT(&next_nodes_tail);
                // SL_GC_REMOVE_ROOT(&next_nodes_head);
                break;  // Stop processing this iteration
            }

            // Get element and add to apply_args
            temp_item = sl_car(current_list_node);
            if (append_to_list(&apply_args_head, &apply_args_tail, temp_item) == NULL) {
                fold_result = sl_make_errorf("fold-left/reduce: Failed to build apply arguments (OOM?)");
                SL_GC_REMOVE_ROOT(&next_nodes_tail);
                SL_GC_REMOVE_ROOT(&next_nodes_head);
                goto cleanup_fold_left;
            }

            // Add the next node (cdr) to the list for the next iteration
            temp_item = sl_cdr(current_list_node);
            if (append_to_list(&next_nodes_head, &next_nodes_tail, temp_item) == NULL) {
                fold_result = sl_make_errorf("fold-left/reduce: Failed to build internal list state (OOM?)");
                SL_GC_REMOVE_ROOT(&next_nodes_tail);
                SL_GC_REMOVE_ROOT(&next_nodes_head);
                goto cleanup_fold_left;
            }

            node_iter = sl_cdr(node_iter);
        }
        if (stop_iteration) {
            SL_GC_REMOVE_ROOT(&next_nodes_tail);
            SL_GC_REMOVE_ROOT(&next_nodes_head);
            break;  // Exit the main while loop
        }

        // Update current_nodes for the next iteration
        current_nodes = next_nodes_head;
        SL_GC_REMOVE_ROOT(&next_nodes_tail);
        SL_GC_REMOVE_ROOT(&next_nodes_head);

        // Apply the procedure
        sl_object *apply_result = sl_apply(proc, apply_args_head, NULL, NULL);
        SL_GC_ADD_ROOT(&apply_result);  // Root the result

        // Check for errors from apply
        if (apply_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(apply_result)) {
            fold_result = apply_result;  // Propagate error
            SL_GC_REMOVE_ROOT(&apply_result);
            goto cleanup_fold_left;
        }

        // Update accumulator
        SL_GC_REMOVE_ROOT(&accumulator);   // Unroot old accumulator
        accumulator = apply_result;        // Update accumulator pointer
        SL_GC_ADD_ROOT(&accumulator);      // Re-root the new accumulator
        SL_GC_REMOVE_ROOT(&apply_result);  // Unroot apply_result (now pointed to by accumulator root)
    }

    fold_result = accumulator;  // Success, return final accumulator

cleanup_fold_left:
    // Unroot everything
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&fold_result);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&apply_args_tail);
    SL_GC_REMOVE_ROOT(&apply_args_head);
    SL_GC_REMOVE_ROOT(&current_nodes);
    SL_GC_REMOVE_ROOT(&lists_arg);
    SL_GC_REMOVE_ROOT(&accumulator);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&proc);
    SL_GC_REMOVE_ROOT(&args);

    // Return either the final accumulator or an error object
    return fold_result == accumulator ? accumulator : fold_result;
}

// Recursive helper for fold-right.
// current_nodes is a list of the current positions in each input list.
static sl_object *builtin_fold_right_recursive_helper(sl_object *proc, sl_object *initial, sl_object *current_nodes) {
    sl_object *result = SL_NIL;
    sl_object *elements_head = SL_NIL;  // List of elements for this level
    sl_object *elements_tail = SL_NIL;
    sl_object *next_nodes_head = SL_NIL;  // List of next nodes for recursive call
    sl_object *next_nodes_tail = SL_NIL;
    sl_object *temp_item = SL_NIL;
    bool stop_recursion = false;

    // Root arguments and locals for this call frame
    SL_GC_ADD_ROOT(&proc);
    SL_GC_ADD_ROOT(&initial);
    SL_GC_ADD_ROOT(&current_nodes);
    SL_GC_ADD_ROOT(&result);
    SL_GC_ADD_ROOT(&elements_head);
    SL_GC_ADD_ROOT(&elements_tail);
    SL_GC_ADD_ROOT(&next_nodes_head);
    SL_GC_ADD_ROOT(&next_nodes_tail);
    SL_GC_ADD_ROOT(&temp_item);

    // Iterate through current nodes to check for end and gather elements/next_nodes
    sl_object *node_iter = current_nodes;
    while (sl_is_pair(node_iter)) {
        sl_object *current_list_node = sl_car(node_iter);

        // Base case check: If any list is empty, return initial
        if (!sl_is_pair(current_list_node)) {
            stop_recursion = true;
            break;
        }

        // Get element for this level
        temp_item = sl_car(current_list_node);
        if (append_to_list(&elements_head, &elements_tail, temp_item) == NULL) {
            result = sl_make_errorf("fold-right: Failed to build element list (OOM?)");
            goto cleanup_fold_right_helper;
        }

        // Get next node for recursive call
        temp_item = sl_cdr(current_list_node);
        if (append_to_list(&next_nodes_head, &next_nodes_tail, temp_item) == NULL) {
            result = sl_make_errorf("fold-right: Failed to build next node list (OOM?)");
            goto cleanup_fold_right_helper;
        }

        node_iter = sl_cdr(node_iter);
    }

    if (stop_recursion) {
        result = initial;  // Base case: return initial value
        goto cleanup_fold_right_helper;
    }

    // --- Recursive Step ---
    // Call helper recursively with the cdr's of the lists
    sl_object *recursive_result = builtin_fold_right_recursive_helper(proc, initial, next_nodes_head);
    SL_GC_ADD_ROOT(&recursive_result);  // Root the result of the recursive call

    if (recursive_result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(recursive_result)) {
        result = recursive_result;  // Propagate error
        SL_GC_REMOVE_ROOT(&recursive_result);
        goto cleanup_fold_right_helper;
    }

    // Build argument list for apply: (element1 element2 ... recursive_result)
    // elements_head already contains element1, element2...
    // Append recursive_result to the end of elements_head list
    if (elements_head == SL_NIL) {  // Should not happen if lists weren't empty
        result = sl_make_errorf("fold-right: Internal error - empty elements list");
        SL_GC_REMOVE_ROOT(&recursive_result);
        goto cleanup_fold_right_helper;
    }
    // Find the tail of elements_head (which is elements_tail) and append recursive_result
    temp_item = recursive_result;
    if (append_to_list(&elements_head, &elements_tail, temp_item) == NULL) {
        result = sl_make_errorf("fold-right: Failed to build apply arguments (OOM?)");
        SL_GC_REMOVE_ROOT(&recursive_result);
        goto cleanup_fold_right_helper;
    }
    // Note: elements_head now contains (element1 ... recursive_result)

    // Apply the procedure
    result = sl_apply(proc, elements_head, NULL, NULL);
    // Result is already rooted

    // Check for errors from apply
    if (result == SL_OUT_OF_MEMORY_ERROR || sl_is_error(result)) {
        // Error already in result
    }

    SL_GC_REMOVE_ROOT(&recursive_result);  // Unroot intermediate recursive result

cleanup_fold_right_helper:
    // Unroot locals for this frame
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&next_nodes_tail);
    SL_GC_REMOVE_ROOT(&next_nodes_head);
    SL_GC_REMOVE_ROOT(&elements_tail);
    SL_GC_REMOVE_ROOT(&elements_head);
    SL_GC_REMOVE_ROOT(&result);
    SL_GC_REMOVE_ROOT(&current_nodes);
    SL_GC_REMOVE_ROOT(&initial);
    SL_GC_REMOVE_ROOT(&proc);
    return result;
}

// (fold-right proc initial list1 [list2 ...])
static sl_object *builtin_fold_right(sl_object *args) {
    sl_object *proc = SL_NIL;
    sl_object *initial = SL_NIL;
    sl_object *lists_arg = SL_NIL;
    sl_object *current_nodes = SL_NIL;  // List to hold the *start* nodes of input lists
    sl_object *fold_result = SL_NIL;
    sl_object *temp_item = SL_NIL;

    // Root arguments
    SL_GC_ADD_ROOT(&args);
    SL_GC_ADD_ROOT(&proc);
    SL_GC_ADD_ROOT(&initial);
    SL_GC_ADD_ROOT(&lists_arg);
    SL_GC_ADD_ROOT(&current_nodes);
    SL_GC_ADD_ROOT(&fold_result);
    SL_GC_ADD_ROOT(&temp_item);

    // 1. Check arity (at least 2 args: proc + initial)
    if (!sl_is_pair(args) || sl_cdr(args) == SL_NIL) {
        fold_result = sl_make_errorf("fold-right: Expected at least 2 arguments (procedure and initial value)");
        goto cleanup_fold_right;
    }

    // 2. Extract proc, initial, and lists
    proc = sl_car(args);
    initial = sl_cadr(args);    // Initial value
    lists_arg = sl_cddr(args);  // The rest are the lists

    if (!sl_is_function(proc)) {
        fold_result = sl_make_errorf("fold-right: First argument must be a procedure");
        goto cleanup_fold_right;
    }

    // If no lists provided, return initial value
    if (lists_arg == SL_NIL) {
        fold_result = initial;
        goto cleanup_fold_right;
    }

    // 3. Validate input lists and build the initial 'current_nodes' list
    current_nodes = SL_NIL;
    sl_object *tail_ptr = SL_NIL;
    SL_GC_ADD_ROOT(&tail_ptr);

    sl_object *list_iter = lists_arg;
    while (sl_is_pair(list_iter)) {
        sl_object *input_list = sl_car(list_iter);
        if (!sl_is_list(input_list)) {
            fold_result = sl_make_errorf("fold-right: All list arguments must be proper lists");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_fold_right;
        }
        temp_item = input_list;  // Add the start of the list
        if (append_to_list(&current_nodes, &tail_ptr, temp_item) == NULL) {
            fold_result = sl_make_errorf("fold-right: Failed to build internal list state (OOM?)");
            SL_GC_REMOVE_ROOT(&tail_ptr);
            goto cleanup_fold_right;
        }
        list_iter = sl_cdr(list_iter);
    }
    if (list_iter != SL_NIL) {
        fold_result = sl_make_errorf("fold-right: Internal error - improper argument list structure");
        SL_GC_REMOVE_ROOT(&tail_ptr);
        goto cleanup_fold_right;
    }
    SL_GC_REMOVE_ROOT(&tail_ptr);

    // 4. Call the recursive helper
    fold_result = builtin_fold_right_recursive_helper(proc, initial, current_nodes);
    // fold_result is already managed by the helper's rooting

cleanup_fold_right:
    // Unroot arguments
    SL_GC_REMOVE_ROOT(&temp_item);
    SL_GC_REMOVE_ROOT(&fold_result);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&current_nodes);
    SL_GC_REMOVE_ROOT(&lists_arg);
    SL_GC_REMOVE_ROOT(&initial);  // Keep if it's the return value
    SL_GC_REMOVE_ROOT(&proc);
    SL_GC_REMOVE_ROOT(&args);

    // Return the result from the helper (value or error)
    return fold_result == initial ? initial : fold_result;
}

// --- Registration ---

void sl_register_higher_order_primitives(sl_object *env) {
    define_builtin(env, "map", builtin_map);
    define_builtin(env, "filter", builtin_filter);
    define_builtin(env, "for-each", builtin_for_each);      // <<< ADDED
    define_builtin(env, "fold-left", builtin_fold_left);    // <<< ADDED
    define_builtin(env, "reduce", builtin_fold_left);       // <<< ADDED (alias)
    define_builtin(env, "fold-right", builtin_fold_right);  // <<< ADDED
}
