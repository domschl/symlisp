#include <stdio.h>
#include <string.h>
#include "sl_output.h"

// Global variables for output redirection
int sl_output_redirected = 0;
char *sl_output_buffer = NULL;
int sl_output_buffer_size = 0;
int sl_output_buffer_pos = 0;

// Function to enable/disable output redirection
int sl_redirect_output(int enable, char *buffer, int buffer_size) {
    if (enable) {
        if (!buffer || buffer_size <= 0) {
            return 0;  // Error: Invalid buffer
        }
        sl_output_redirected = 1;
        sl_output_buffer = buffer;
        sl_output_buffer_size = buffer_size;
        sl_output_buffer_pos = 0;
        sl_output_buffer[0] = '\0';  // Initialize buffer as empty string
    } else {
        sl_output_redirected = 0;
        sl_output_buffer = NULL;
        sl_output_buffer_size = 0;
        sl_output_buffer_pos = 0;
    }
    return 1;  // Success
}

// Custom output function to replace standard output functions
int sl_output_char(int c) {
    if (sl_output_redirected) {
        if (sl_output_buffer_pos < sl_output_buffer_size - 1) {
            sl_output_buffer[sl_output_buffer_pos++] = (char)c;
            sl_output_buffer[sl_output_buffer_pos] = '\0';  // Keep buffer null-terminated
            return c;
        }
        return EOF;  // Buffer full
    } else {
        return putchar(c);  // Direct to stdout
    }
}

int sl_output_string(const char *s) {
    if (sl_output_redirected) {
        int len = strlen(s);
        int remaining = sl_output_buffer_size - sl_output_buffer_pos - 1;
        int to_copy = (len < remaining) ? len : remaining;

        if (to_copy > 0) {
            memcpy(sl_output_buffer + sl_output_buffer_pos, s, to_copy);
            sl_output_buffer_pos += to_copy;
            sl_output_buffer[sl_output_buffer_pos] = '\0';  // Keep buffer null-terminated
        }
        return to_copy;
    } else {
        return fputs(s, stdout);
    }
}