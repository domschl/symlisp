#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "sl_core.h"
#include "sl_env.h"
#include "sl_builtins.h"
#include "sl_parse.h"
#include "sl_eval.h"

// Default path for standard library relative to executable location
// Should match the one used in the REPL or be set via CMake
#define DEFAULT_STDLIB_PATH "../stdsymlisp"

// Helper to look up a symbol and return its value, printing errors
static sl_object *get_global_var(const char *name) {
    sl_object *sym = sl_make_symbol(name);
    // --- ADDED: More robust check after sl_make_symbol ---

    if (!sym) {
        fprintf(stderr, "Tester Error: sl_make_symbol('%s') returned NULL.\n", name);
        return NULL;  // Indicate failure
    }
    if (sym == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Tester Error: Out of memory creating symbol '%s'.\n", name);
        return NULL;  // Indicate failure
    }
    // Explicitly check if it's a symbol BEFORE passing to lookup
    if (!sl_is_symbol(sym)) {
        fprintf(stderr, "Tester Error: sl_make_symbol('%s') did not return a symbol object (type: %d).\n", name, sym->type);
        return NULL;  // Indicate failure
    }
    // --- End Added Checks ---

    // Root symbol temporarily during lookup
    sl_gc_add_root(&sym);
    sl_object *binding_pair = sl_env_lookup(sl_global_env, sym);  // Lookup returns the pair
    sl_gc_remove_root(&sym);

    if (binding_pair == SL_NIL) {
        fprintf(stderr, "Tester Error: Global variable '%s' not found.\n", name);
        return NULL;  // Indicate failure (not found)
    }
    if (!sl_is_pair(binding_pair)) {
        // Should not happen with correct lookup, but check defensively
        fprintf(stderr, "Tester Error: Internal error - lookup for '%s' did not return a pair.\n", name);
        return NULL;  // Indicate failure (internal error)
    }
    if (sl_is_error(binding_pair)) {  // Check if the lookup itself returned an error object
        char *err_str = sl_object_to_string(binding_pair);
        fprintf(stderr, "Tester Error: Error looking up global variable '%s': %s\n", name, err_str ? err_str : "Unknown error");
        free(err_str);
        return NULL;  // Indicate failure (lookup error)
    }

    // <<< ADDED: Extract the value (cdr) from the binding pair >>>
    sl_object *value = sl_cdr(binding_pair);

    // Optional: Check if the extracted value is an error (shouldn't be if define worked)
    if (sl_is_error(value)) {
        char *err_str = sl_object_to_string(value);
        fprintf(stderr, "Tester Error: Value associated with '%s' is an error: %s\n", name, err_str ? err_str : "Unknown error");
        free(err_str);
        return NULL;  // Indicate failure (value is error)
    }

    return value;  // Return the actual value
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <test_file.scm>\n", argv[0]);
        return 2;  // Indicate usage error
    }
    const char *test_filename = argv[1];

    // 1. Initialize SymLisp
    sl_mem_init(0);
    sl_global_env = sl_env_create(NULL);
    if (!sl_global_env || sl_global_env == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Fatal: Failed to create global environment.\n");
        sl_mem_shutdown();
        return 1;
    }
    sl_gc_add_root(&sl_global_env);
    sl_builtins_init(sl_global_env);

    // 2. Load Standard Library (including testing.scm)
    printf("Loading standard library from: %s\n", DEFAULT_STDLIB_PATH);
    sl_object *load_lib_result = sl_load_directory(DEFAULT_STDLIB_PATH, sl_global_env);
    if (load_lib_result != SL_TRUE) {
        fprintf(stderr, "Error loading standard library:\n");
        char *err_str = sl_object_to_string(load_lib_result);
        fprintf(stderr, "  %s\n", err_str ? err_str : "Unknown error");
        free(err_str);
        sl_mem_shutdown();
        return 1;
    }
    printf("Standard library loaded.\n");

    // 3. Load and Run Test File
    printf("Loading test file: %s\n", test_filename);
    FILE *test_file = fopen(test_filename, "r");
    if (!test_file) {
        perror("Error opening test file");
        sl_mem_shutdown();
        return 1;
    }

    sl_object *eval_res = sl_eval_stream(test_file, sl_global_env);
    fclose(test_file);

    if (eval_res == SL_OUT_OF_MEMORY_ERROR || sl_is_error(eval_res)) {
        fprintf(stderr, "Error executing test file '%s':\n", test_filename);
        char *err_str = sl_object_to_string(eval_res);
        fprintf(stderr, "  %s\n", err_str ? err_str : "Unknown error");
        free(err_str);
        sl_mem_shutdown();
        return 1;
    }
    printf("Test file execution finished.\n");

    // 4. Retrieve Statistics
    printf("\n--- Test Results ---\n");

    sl_object *all_passed_obj = get_global_var("tests-all-passed");
    sl_object *run_count_obj = get_global_var("tests-run-count");
    sl_object *passed_count_obj = get_global_var("tests-passed-count");
    sl_object *failed_count_obj = get_global_var("tests-failed-count");

    bool all_passed = false;
    long long run_count = -1;
    long long passed_count = -1;
    long long failed_count = -1;
    int exit_code = 1;  // Default to failure

    // Temporary variables for sl_number_get_si
    int64_t num, den;

    // Extract values and check types
    if (all_passed_obj && sl_is_boolean(all_passed_obj)) {
        all_passed = (all_passed_obj == SL_TRUE);
    } else {
        fprintf(stderr, "Tester Warning: Could not read or invalid type for 'tests-all-passed'.\n");
    }

    if (run_count_obj && sl_is_number(run_count_obj)) {
        // Call sl_number_get_si correctly
        if (sl_number_get_si(run_count_obj, &num, &den) && den == 1) {
            run_count = num;
        } else {
            fprintf(stderr, "Tester Warning: 'tests-run-count' is not a valid integer.\n");
        }
    } else {
        fprintf(stderr, "Tester Warning: Could not read or invalid type for 'tests-run-count'.\n");
    }

    if (passed_count_obj && sl_is_number(passed_count_obj)) {
        // Call sl_number_get_si correctly
        if (sl_number_get_si(passed_count_obj, &num, &den) && den == 1) {
            passed_count = num;
        } else {
            fprintf(stderr, "Tester Warning: 'tests-passed-count' is not a valid integer.\n");
        }
    } else {
        fprintf(stderr, "Tester Warning: Could not read or invalid type for 'tests-passed-count'.\n");
    }

    if (failed_count_obj && sl_is_number(failed_count_obj)) {
        // Call sl_number_get_si correctly
        if (sl_number_get_si(failed_count_obj, &num, &den) && den == 1) {
            failed_count = num;
        } else {
            fprintf(stderr, "Tester Warning: 'tests-failed-count' is not a valid integer.\n");
        }
    } else {
        fprintf(stderr, "Tester Warning: Could not read or invalid type for 'tests-failed-count'.\n");
    }

    // 5. Print Summary
    if (run_count >= 0 && passed_count >= 0 && failed_count >= 0) {
        printf("Total tests run:    %lld\n", run_count);
        printf("Tests passed:       %lld\n", passed_count);
        printf("Tests failed:       %lld\n", failed_count);
        printf("Overall result:     %s\n", all_passed ? "PASSED" : "FAILED");
    } else {
        printf("Could not retrieve complete test statistics.\n");
        // Keep exit_code as 1 (failure) if stats are incomplete
    }

    // 6. Determine Exit Code based *only* on all_passed_obj status
    // We trust the Scheme code's tests-all-passed variable as the final verdict.
    if (all_passed_obj && sl_is_boolean(all_passed_obj) && all_passed_obj == SL_TRUE) {
        exit_code = 0;  // Success
    } else {
        // If the variable wasn't found, wasn't a boolean, or was #f
        exit_code = 1;  // Failure
    }

    // 7. Shutdown
    sl_mem_shutdown();

    // 8. Return the final exit code
    return exit_code;
}