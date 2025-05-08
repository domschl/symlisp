#ifndef SL_OUTPUT_H
#define SL_OUTPUT_H

#include <stdio.h>

// Global variables for output redirection
extern int sl_output_redirected;
extern char *sl_output_buffer;
extern int sl_output_buffer_size;
extern int sl_output_buffer_pos;

// Function to enable/disable output redirection
int sl_redirect_output(int enable, char *buffer, int buffer_size);

// Custom output functions
int sl_output_char(int c);
int sl_output_string(const char *s);

#endif  // SL_OUTPUT_H