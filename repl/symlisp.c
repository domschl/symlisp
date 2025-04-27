#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <readline/readline.h>
#include <readline/history.h>

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

    printf("symlisp REPL (Ctrl+D to exit)\n");

    // Use dynamic allocation instead of fixed buffer
    size_t buffer_size = 1024;  // Initial size
    char *buffer = malloc(buffer_size);
    if (!buffer) {
        printf("Memory allocation failed\n");
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

        // Handle comments
        char *comment = strchr(line, ';');
        if (comment) *comment = '\0';

        // Skip empty lines
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
                printf("Memory reallocation failed\n");
                free(buffer);
                free(line);
                return;
            }

            buffer = new_buffer;
            buffer_size = new_size;
        }

        // Append to the buffer
        strcat(buffer, line);
        strcat(buffer, " ");  // Add space for readability

        free(line);

        // Check balance using the library function
        int error_pos = -1;
        int paren_balance = 0;  // symlisp_validate_parentheses(buffer, &error_pos);

        // If balanced, process the expression
        if (paren_balance == 0) {
            if (strlen(buffer) > 0) {
                // symlispValue *result = symlisp_parse_eval_multi(buffer, global_env);

                // Print the result using the output system
                // symlisp_write_value(result);
                printf("\n");

                // symlisp_free(result);
                add_history(buffer);  // Add to history only if balanced
            }
            buffer[0] = '\0';  // Reset buffer but keep allocated memory

            strcpy(prompt, "symlisp> ");
        } else if (paren_balance < 0) {
            // Unbalanced closing parenthesis - syntax error
            printf("Error: Unexpected closing bracket at position %d\n", error_pos);
            buffer[0] = '\0';  // Reset buffer
            strcpy(prompt, "symlisp> ");
        } else {
            // Change prompt to show we're awaiting more input
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
}

int main(int argc, char *argv[]) {
    run_repl();
    return 0;
}