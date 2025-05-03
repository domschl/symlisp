#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <sys/stat.h>
#include <unistd.h>  // For getopt
#include <getopt.h>

#include "sl_core.h"
#include "sl_env.h"
#include "sl_builtins.h"
#include "sl_eval.h"

// Default path for standard library relative to executable location (adjust as needed)
// This is a placeholder; using CMake to define this is better.
#define DEFAULT_STDLIB_PATH "../stdsymlisp"

// Simple parenthesis balance checker (replace with more robust later if needed)
int check_balance(const char *str) {
    int balance = 0;
    bool in_string = 0;  // false; VSCode doesn't get false!
    while (*str) {
        if (*str == '"') {
            in_string = !in_string;
        } else if (!in_string) {
            if (*str == '(') {
                balance++;
            } else if (*str == ')') {
                balance--;
            }
        }
        // Basic escape handling (just skip next char) - needs improvement
        if (*str == '\\' && str[1] != '\0') {
            str++;
        }
        str++;
    }
    return balance;
}

void run_repl() {
    char *home_dir = getenv("HOME");
    char history_dir[1024] = "";
    char history_file[1024] = "";

    if (home_dir) {
        // Create directory path
        snprintf(history_dir, sizeof(history_dir), "%s/.config/symlisp", home_dir);
        snprintf(history_file, sizeof(history_file), "%s/history", history_dir);

        // Create directory if it doesn't exist
        struct stat st = {0};
        if (stat(history_dir, &st) == -1) {
            // Directory doesn't exist, create it (mode 0700 = rwx for user only)
            mkdir(history_dir, 0700);
        }

        // Load history if the file exists
        read_history(history_file);

        // Limit history size to 1000 entries
        stifle_history(1000);
    }

    printf("SymLisp REPL (Ctrl+D to exit)\n");

    // Use dynamic allocation instead of fixed buffer
    size_t buffer_size = 1024;
    char *buffer = malloc(buffer_size);
    if (!buffer) {
        perror("Memory allocation failed for input buffer");
        sl_mem_shutdown();  // Clean up library memory
        return;
    }
    buffer[0] = '\0';

    char prompt[32] = "symlisp> ";

    while (1) {
        char *line = readline(prompt);
        if (!line) break;  // Ctrl+D

        // --- ADDED: Strip comment from the line ---
        char *comment_start = NULL;
        bool in_string_repl = false;  // Use a different name
        for (char *p = line; *p; ++p) {
            if (*p == '"') {
                in_string_repl = !in_string_repl;
            } else if (*p == '\\' && p[1] != '\0') {
                // Simple escape handling: skip the next character
                p++;
            } else if (*p == ';' && !in_string_repl) {
                comment_start = p;
                break;  // Found the start of a comment
            }
        }
        if (comment_start) {
            *comment_start = '\0';  // Truncate the line string here
        }
        // --- END ADDED ---

        // Handle empty line
        if (strlen(line) == 0) {
            free(line);
            continue;
        }

        // Resize buffer if needed
        size_t current_len = strlen(buffer);
        size_t line_len = strlen(line);
        size_t required_size = current_len + line_len + 2;  // +2 for space and null terminator

        if (required_size > buffer_size) {
            // Double the buffer size or increase to required size, whichever is larger
            size_t new_size = buffer_size * 2;
            if (new_size < required_size) {
                new_size = required_size;
            }

            char *new_buffer = realloc(buffer, new_size);
            if (!new_buffer) {
                perror("Memory reallocation failed for input buffer");
                free(buffer);
                free(line);
                sl_mem_shutdown();  // Clean up library memory
                return;
            }

            buffer = new_buffer;
            buffer_size = new_size;
        }

        // Append to the buffer
        strcat(buffer, line);
        strcat(buffer, " ");
        free(line);

        // Check balance
        int paren_balance = check_balance(buffer);

        // If balanced, process the expression(s)
        if (paren_balance == 0) {
            if (strlen(buffer) > 0) {
                add_history(buffer);  // Add complete input block to history

                // --- Evaluate the entire buffer ---
                sl_object *eval_result = sl_eval_string(buffer, sl_global_env);
                // ---------------------------------

                // Print result (only if not NIL and not an error, or if it's an error)
                // Note: sl_eval_string returns NIL for empty/whitespace input.
                // It also returns the result of the *last* expression.
                // Builtins like display, newline, define return NIL or unspecified.
                // We only print if the final result is significant or an error.
                if (eval_result && eval_result != SL_NIL) {
                    char *result_str = sl_object_to_string(eval_result);
                    if (result_str) {
                        // Check if it's an error object to print to stderr
                        if (sl_is_error(eval_result)) {
                            fprintf(stderr, "%s\n", result_str);
                        } else {
                            printf("=> %s\n", result_str);
                        }
                        free(result_str);
                    } else {
                        // Error converting the result itself (likely OOM)
                        if (eval_result == SL_OUT_OF_MEMORY_ERROR) {
                            fprintf(stderr, "Error: Out of memory during evaluation or result conversion.\n");
                        } else {
                            fprintf(stderr, "Error: Could not convert final result to string.\n");
                        }
                    }
                } else if (eval_result == SL_NIL) {
                    // Don't print anything if the final result is NIL (e.g. from display, define)
                }

                // Trigger GC after processing the buffer
                sl_gc();
            }

            // Clear buffer for next input cycle
            buffer[0] = '\0';
            strcpy(prompt, "symlisp> ");

        } else if (paren_balance < 0) {
            fprintf(stderr, "Error: Unexpected closing parenthesis.\n");
            buffer[0] = '\0';  // Clear buffer on syntax error
            strcpy(prompt, "symlisp> ");
        } else {
            // Unbalanced, wait for more input
            strcpy(prompt, "...... ");
        }
    }  // End while(1) REPL loop

    // Clean up
    free(buffer);

    // Save history before exiting
    if (history_file[0] != '\0') {
        write_history(history_file);
    }

    printf("\nGoodbye!\n");

    // --- Shutdown SymLisp Memory ---
    sl_mem_shutdown();
}

int main(int argc, char *argv[]) {
    bool load_stdlib = true;  // Default to loading stdlib
    int opt;

    // Simple argument parsing using getopt
    while ((opt = getopt(argc, argv, "n")) != -1) {
        switch (opt) {
        case 'n':
            load_stdlib = false;
            break;
        case '?':  // Unknown option or missing argument
            fprintf(stderr, "Usage: %s [-n]\n", argv[0]);
            fprintf(stderr, "  -n: Do not load standard library\n");
            return 1;
        default:
            // Should not happen with this simple option string
            break;
        }
    }

    // Initialize memory, GC, core objects (NIL, TRUE, FALSE, OOM)
    sl_mem_init(0);  // Use default heap size for now

    // Create the global environment
    printf("Creating global environment...\n");
    sl_global_env = sl_env_create(NULL);  // Top-level env has no parent
    if (!sl_global_env || sl_global_env == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Fatal: Failed to create global environment.\n");
        sl_mem_shutdown();  // Use shutdown before exit
        return 1;
    }
    printf("Global environment created.\n");
    sl_gc_add_root(&sl_global_env);  // Keep global env rooted (NOW SAFE)

    // Initialize built-in functions and add them to the global environment
    sl_builtins_init(sl_global_env);  // (NOW SAFE)

    // --- Load Standard Library (conditionally) ---
    if (load_stdlib) {
        printf("Loading standard library from: %s\n", DEFAULT_STDLIB_PATH);
        sl_object *load_lib_result = sl_load_directory(DEFAULT_STDLIB_PATH, sl_global_env);
        if (load_lib_result != SL_TRUE) {
            fprintf(stderr, "Error loading standard library:\n");
            // Print the error object returned by sl_load_directory
            char *err_str = sl_object_to_string(load_lib_result);
            if (err_str) {
                fprintf(stderr, "  %s\n", err_str);
                free(err_str);
            } else {
                fprintf(stderr, "  Unknown error during library loading.\n");
            }
            sl_mem_shutdown();  // Use shutdown before exit
            return 1;
        }
        printf("Standard library loaded.\n");
    } else {
        printf("Skipping standard library loading (-n specified).\n");
    }
    // ---------------------------

    // Load history
    char *home_dir = getenv("HOME");
    char history_dir[1024] = "";
    char history_file[1024] = "";

    if (home_dir) {
        // Create directory path
        snprintf(history_dir, sizeof(history_dir), "%s/.config/symlisp", home_dir);
        snprintf(history_file, sizeof(history_file), "%s/history", history_dir);

        // Create directory if it doesn't exist
        struct stat st = {0};
        if (stat(history_dir, &st) == -1) {
            mkdir(history_dir, 0700);
        }

        // Load history if the file exists
        read_history(history_file);

        // Limit history size
        stifle_history(1000);
    }

    // Run the REPL (which now contains the shutdown call at its end)
    run_repl();

    return 0;  // Normal exit
}
