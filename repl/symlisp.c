#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <readline/readline.h>
#include <readline/history.h>

// Include the SymLisp library headers
#include "sl_core.h"
#include "sl_parse.h"
#include "sl_env.h"  // Include for sl_global_env access (though it's extern in sl_core.h)
#include "sl_eval.h"
#include "sl_builtins.h"  // Include builtins header

// Simple parenthesis balance checker (replace with more robust later if needed)
int check_balance(const char *str) {
    int balance = 0;
    bool in_string = false;
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
    // --- Initialize SymLisp Memory & Builtins ---
    sl_mem_init(0);
    sl_builtins_init(sl_global_env);  // Initialize builtins after memory/global env
    // -------------------------------------------

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
    size_t buffer_size = 1024;  // Initial size
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

        if (strlen(line) == 0) {
            free(line);
            continue;
        }

        // Handle comments (simple version: only if ';' is first non-space char)
        const char *temp_line = line;
        while (isspace(*temp_line))
            temp_line++;
        if (*temp_line == ';') {
            free(line);
            continue;
        }
        // More robust comment handling (anywhere on line) could be added here if needed

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
        strcat(buffer, " ");  // Add space for readability

        free(line);

        // Check balance using the simple checker
        int paren_balance = check_balance(buffer);

        // If balanced, process the expression
        if (paren_balance == 0) {
            if (strlen(buffer) > 0) {
                const char *parse_ptr = buffer;
                const char *end_ptr = NULL;
                sl_object *parse_result = sl_parse_string(parse_ptr, &end_ptr);

                if (parse_result != SL_NIL) {
                    const char *check_end = end_ptr;
                    while (isspace(*check_end))
                        check_end++;

                    if (*check_end == '\0') {
                        // Successfully parsed the whole buffer
                        add_history(buffer);  // Add valid input to history

                        // --- Evaluate ---
                        sl_object *eval_result = sl_eval(parse_result, sl_global_env);
                        // ----------------

                        // --- Print Result/Error ---
                        if (sl_is_error(eval_result)) {
                            fprintf(stderr, "Error: %s\n", sl_error_message(eval_result));
                        } else {
                            printf("=> ");  // Indicate standard result
                            sl_write_to_stream(eval_result, stdout);
                            printf("\n");
                        }
                        // --------------------------

                        // --- GC ---
                        sl_gc();
                        // --------
                    } else {
                        fprintf(stderr, "Error: Could not parse entire input. Remaining: '%s'\n", end_ptr);
                    }
                } else {
                    // Parsing failed (sl_parse_string prints errors internally for now)
                    fprintf(stderr, "Error during parsing.\n");
                }
            }
            buffer[0] = '\0';
            strcpy(prompt, "symlisp> ");
        } else if (paren_balance < 0) {
            fprintf(stderr, "Error: Unexpected closing parenthesis.\n");
            buffer[0] = '\0';
            strcpy(prompt, "symlisp> ");
        } else {
            strcpy(prompt, "...... ");
        }
    }

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
    run_repl();
    return 0;
}