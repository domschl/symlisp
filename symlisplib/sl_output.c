#include <stdio.h>
#include <string.h>
#include "sl_output.h"

// Global variables for output redirection
int sl_output_redirected = 0;
char *sl_output_buffer = NULL;
int sl_output_buffer_size = 0;
int sl_output_buffer_pos = 0;

// Global variables for input redirection
int sl_input_redirected = 0;
const char *sl_input_buffer = NULL;
int sl_input_buffer_size = 0;
int sl_input_buffer_pos = 0;

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

// Function to enable/disable input redirection
int sl_redirect_input(int enable, const char *buffer, int buffer_size) {
    if (enable) {
        if (!buffer) {
            return 0;  // Error: Invalid buffer
        }
        sl_input_redirected = 1;
        sl_input_buffer = buffer;
        sl_input_buffer_size = buffer_size;
        sl_input_buffer_pos = 0;
    } else {
        sl_input_redirected = 0;
        sl_input_buffer = NULL;
        sl_input_buffer_size = 0;
        sl_input_buffer_pos = 0;
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

// Custom input function to replace standard input functions
int sl_input_char(void) {
    if (sl_input_redirected) {
        if (sl_input_buffer_pos < sl_input_buffer_size) {
            return (unsigned char)sl_input_buffer[sl_input_buffer_pos++];
        }
        return EOF;  // End of buffer
    } else {
        return getchar();  // Direct from stdin
    }
}

char *sl_input_line(char *buffer, int size) {
    if (sl_input_redirected) {
        int i = 0;
        while (i < size - 1 && sl_input_buffer_pos < sl_input_buffer_size) {
            char c = sl_input_buffer[sl_input_buffer_pos++];
            buffer[i++] = c;
            if (c == '\n') break;
        }
        if (i == 0 && sl_input_buffer_pos >= sl_input_buffer_size) {
            return NULL;  // EOF
        }
        buffer[i] = '\0';
        return buffer;
    } else {
        return fgets(buffer, size, stdin);  // Direct from stdin
    }
}