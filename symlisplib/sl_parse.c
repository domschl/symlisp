#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <math.h>
#include <gmp.h>

#include "sl_core.h"
#include "sl_parse.h"  // Include its own header

// --- Buffer Management ---

typedef struct {
    char *buffer;
    size_t length;
    size_t capacity;
} sl_string_buffer;

// Initializes a string buffer
static void sbuf_init(sl_string_buffer *sbuf) {
    sbuf->buffer = NULL;
    sbuf->length = 0;
    sbuf->capacity = 0;
}

// Frees the memory used by a string buffer
static void sbuf_free(sl_string_buffer *sbuf) {
    free(sbuf->buffer);
    sbuf_init(sbuf);  // Reset
}

// Ensures buffer has enough capacity, reallocating if necessary
static bool sbuf_ensure_capacity(sl_string_buffer *sbuf, size_t additional_needed) {
    if (sbuf->length + additional_needed + 1 > sbuf->capacity) {
        size_t new_capacity = (sbuf->capacity == 0) ? 64 : sbuf->capacity * 2;
        while (new_capacity < sbuf->length + additional_needed + 1) {
            new_capacity *= 2;
        }
        char *new_buffer = (char *)realloc(sbuf->buffer, new_capacity);
        if (!new_buffer) {
            fprintf(stderr, "Error: Failed to reallocate string buffer\n");
            return false;  // Allocation failure
        }
        sbuf->buffer = new_buffer;
        sbuf->capacity = new_capacity;
    }
    return true;
}

// Appends a character to the buffer
static bool sbuf_append_char(sl_string_buffer *sbuf, char c) {
    if (!sbuf_ensure_capacity(sbuf, 1)) return false;
    sbuf->buffer[sbuf->length++] = c;
    sbuf->buffer[sbuf->length] = '\0';  // Keep null-terminated
    return true;
}

// Add a helper to append multiple bytes if needed
static bool sbuf_append_bytes(sl_string_buffer *sbuf, const char *bytes, size_t len) {
    // Implementation depends on your sl_string_buffer details
    // Ensure buffer has space, realloc if needed, then memcpy
    // Return true on success, false on memory allocation failure
    // Placeholder implementation:
    if (sbuf->capacity < sbuf->length + len) {
        size_t new_capacity = (sbuf->capacity == 0) ? 8 : sbuf->capacity * 2;
        while (new_capacity < sbuf->length + len) {
            new_capacity *= 2;
        }
        char *new_buffer = (char *)realloc(sbuf->buffer, new_capacity);
        if (!new_buffer) return false;
        sbuf->buffer = new_buffer;
        sbuf->capacity = new_capacity;
    }
    memcpy(sbuf->buffer + sbuf->length, bytes, len);
    sbuf->length += len;
    return true;
}

// Appends a null-terminated string to the buffer
static bool sbuf_append_str(sl_string_buffer *sbuf, const char *str) {
    if (!str) return true;  // Nothing to append
    size_t len = strlen(str);
    if (!sbuf_ensure_capacity(sbuf, len)) return false;
    memcpy(sbuf->buffer + sbuf->length, str, len);
    sbuf->length += len;
    sbuf->buffer[sbuf->length] = '\0';  // Keep null-terminated
    return true;
}

// --- Forward Declarations ---
// Parsing helpers
static sl_object *parse_expression(const char **input);
static sl_object *parse_list(const char **input);
static sl_object *parse_atom(const char **input);
static sl_object *parse_string_literal(const char **input);  // Renamed to avoid conflict
static sl_object *parse_number(const char **input_ptr, const char *start, size_t len);
static sl_object *parse_symbol_or_bool(const char **input_ptr, const char *start, size_t len);

static bool is_delimiter(char c);

// Writing helpers
static bool sl_write_recursive(sl_object *obj, sl_string_buffer *sbuf);
static bool sl_write_pair_recursive(sl_object *pair, sl_string_buffer *sbuf);
static bool sl_write_stream_recursive(sl_object *obj, FILE *stream);
static bool sl_write_pair_stream_recursive(sl_object *pair, FILE *stream);

// --- Forward Declarations (Stream Parsing) ---
static sl_object *parse_stream_expression(FILE *stream);
static sl_object *parse_stream_list(FILE *stream);
static sl_object *parse_stream_atom(FILE *stream);
static sl_object *parse_stream_string_literal(FILE *stream);
static sl_object *parse_stream_character_literal(FILE *stream);
static int peek_char(FILE *stream);  // Helper to peek next char

// --- Public Parsing Functions ---

// Helper to check if char is delimiter (adjust as needed for stream parsing)
static int is_stream_delimiter(int c) {
    return isspace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == EOF;
}

// Read an atom (symbol, number, #t, #f) from a stream
// This replaces the previous fixed-buffer version.
static sl_object *read_atom(FILE *stream) {
    char *token_buffer = NULL;
    size_t buffer_size = 0;
    size_t buffer_capacity = 0;
    const size_t initial_capacity = 32;
    int c;

    while (!is_stream_delimiter(c = fgetc(stream))) {
        // Check if buffer needs resizing
        if (buffer_size + 1 >= buffer_capacity) {  // +1 for the char to add
            size_t new_capacity = (buffer_capacity == 0) ? initial_capacity : buffer_capacity * 2;
            char *new_buffer = realloc(token_buffer, new_capacity);
            if (!new_buffer) {
                fprintf(stderr, "Parser Error: Out of memory reading atom.\n");
                free(token_buffer);
                // Need to handle the character 'c' that was read but not processed.
                // Maybe ungetc(c, stream)? Or just signal error.
                if (c != EOF) ungetc(c, stream);  // Try to put back delimiter
                return SL_NIL;                    // Signal error
            }
            token_buffer = new_buffer;
            buffer_capacity = new_capacity;
        }
        token_buffer[buffer_size++] = (char)c;
    }

    // Put back the delimiter character that stopped the loop
    if (c != EOF) {
        ungetc(c, stream);
    }

    if (buffer_size == 0) {  // Should not happen if called correctly
        free(token_buffer);
        return SL_NIL;  // Indicate error or EOF right at start
    }

    // Null-terminate the buffer
    // Ensure capacity for null terminator
    if (buffer_size + 1 > buffer_capacity) {
        // Resize one last time if exactly full
        size_t new_capacity = buffer_capacity + 1;  // Just need one more byte
        char *new_buffer = realloc(token_buffer, new_capacity);
        if (!new_buffer) {
            fprintf(stderr, "Parser Error: Out of memory finalizing atom buffer.\n");
            free(token_buffer);
            return SL_NIL;
        }
        token_buffer = new_buffer;
        buffer_capacity = new_capacity;
    }
    token_buffer[buffer_size] = '\0';

    // --- Try parsing token_buffer as #t, #f, number, or symbol ---
    sl_object *result = NULL;

    if (strcmp(token_buffer, "#t") == 0) {
        result = SL_TRUE;
    } else if (strcmp(token_buffer, "#f") == 0) {
        result = SL_FALSE;
    } else {
        // Try parsing as number
        mpq_t num_q;
        mpq_init(num_q);
        bool is_num = (mpq_set_str(num_q, token_buffer, 10) == 0);

        if (is_num) {
            mpq_canonicalize(num_q);
            mpz_t num_z, den_z;
            mpz_inits(num_z, den_z, NULL);
            mpq_get_num(num_z, num_q);
            mpq_get_den(den_z, num_q);

            if (mpz_cmp_si(den_z, 1) == 0 && fits_int64(num_z)) {
                result = sl_make_number_si(mpz_get_si(num_z), 1);
            } else if (fits_int64(num_z) && fits_int64(den_z)) {
                result = sl_make_number_si(mpz_get_si(num_z), mpz_get_si(den_z));
            } else {
                result = sl_make_number_q(num_q);  // Copies num_q
            }
            mpz_clears(num_z, den_z, NULL);
        }
        mpq_clear(num_q);  // Clear temp GMP num

        if (!is_num) {
            // If not a number, assume it's a symbol
            result = sl_make_symbol(token_buffer);
        }
    }

    free(token_buffer);  // Free the dynamically allocated buffer

    // Check if object creation failed due to memory
    if (result == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Parser Error: Out of memory creating object for atom.\n");
        return SL_NIL;  // Signal error
    }

    return result;  // Return the parsed object (#t, #f, number, symbol) or SL_NIL on error
}

// --- Parsing Implementation ---

// Parses an integer (base 2-62), fraction N/D (base 10), decimal (base 10),
// or scientific (N.MeP, base 10) string.
// On success: Initializes result_q, sets end_ptr past the parsed number, returns true.
// On failure: Returns false, end_ptr is undefined, result_q is undefined.
// Caller MUST initialize result_q before calling.
bool parse_rational_from_string(const char *start, int base, mpq_t result_q, const char **end_ptr) {
    const char *ptr = start;
    bool negative = false;

    // 1. Handle Sign
    if (*ptr == '+') {
        ptr++;
    } else if (*ptr == '-') {
        negative = true;
        ptr++;
    }
    const char *num_start = ptr;

    // Check for empty string after sign, but allow "0"
    if (*num_start == '\0' && *(start) != '0' && (*(start + 1) != '0' || (*start != '+' && *start != '-'))) {
        *end_ptr = start;
        return false;
    }

    // --- Non-Base-10 Path (Integers Only) ---
    if (base != 10) {
        mpz_t temp_z;
        mpz_init(temp_z);
        int gmp_ret = mpz_set_str(temp_z, start, base);
        bool success = false;
        const char *final_end_ptr = start;
        if (gmp_ret == 0) {
            const char *scan_ptr = num_start;
            bool found_exponent_char = false;
            while (*scan_ptr) {
                char c = *scan_ptr;
                int digit_val = -1;
                if (c >= '0' && c <= '9')
                    digit_val = c - '0';
                else if (base > 10 && c >= 'a' && c <= 'z')
                    digit_val = c - 'a' + 10;
                else if (base > 10 && c >= 'A' && c <= 'Z')
                    digit_val = c - 'A' + 10;

                // if (c == 'e' || c == 'E') { found_exponent_char = true; }

                if (digit_val != -1 && digit_val < base) {
                    scan_ptr++;
                } else {
                    break;
                }
            }
            if (!found_exponent_char && (scan_ptr > num_start || (mpz_sgn(temp_z) == 0 && scan_ptr == num_start && *num_start == '0'))) {
                mpq_set_z(result_q, temp_z);
                final_end_ptr = scan_ptr;
                success = true;
            }
        }
        mpz_clear(temp_z);
        *end_ptr = final_end_ptr;
        return success;
    }

    // --- Base-10 Path ---
    // First, try mpq_set_str for integer or N/D format
    mpq_t temp_q;  // Local temp for mpq_set_str result
    mpq_init(temp_q);
    int gmp_ret = mpq_set_str(temp_q, start, 10);
    const char *gmp_end_ptr = NULL;

    if (gmp_ret == 0) {
        if (mpz_sgn(mpq_denref(temp_q)) == 0) {
            // Division by zero, treat as failure for this path
            gmp_ret = -1;  // Mark as failed
        } else {
            // mpq_set_str succeeded, find where it stopped.
            const char *scan_ptr = num_start;
            bool seen_slash = false;
            bool after_slash = false;
            bool sign_after_slash_done = false;
            while (*scan_ptr) {
                char c = *scan_ptr;
                int digit_val = -1;
                if (c >= '0' && c <= '9')
                    digit_val = c - '0';
                else
                    digit_val = -1;

                if (digit_val != -1) {
                    scan_ptr++;
                    if (after_slash) sign_after_slash_done = true;
                } else if (c == '/' && !seen_slash && scan_ptr > num_start) {
                    seen_slash = true;
                    after_slash = true;
                    scan_ptr++;
                } else if ((c == '+' || c == '-') && after_slash && !sign_after_slash_done) {
                    sign_after_slash_done = true;
                    scan_ptr++;
                    if (!(*scan_ptr >= '0' && *scan_ptr <= '9')) {
                        scan_ptr--;
                        break;
                    }
                } else {
                    break;
                }
            }
            // Set the end pointer to where the scan stopped
            gmp_end_ptr = scan_ptr;
        }
    }
    // gmp_end_ptr is now set if mpq_set_str succeeded and parsed something valid

    // Now, always try the full decimal/scientific parser
    mpq_t significand_q;      // Use a separate rational for this path
    mpq_init(significand_q);  // <<< Initialize significand_q
    const char *decimal_sci_end_ptr = NULL;
    bool decimal_sci_parsed = false;

    // Reset ptr to start after sign for this attempt
    ptr = num_start;
    const char *significand_start = ptr;
    const char *significand_end = ptr;
    const char *decimal_point = NULL;
    bool has_exponent = false;

    // --- BEGIN Significand Scan ---
    while (*significand_end) {
        if (*significand_end >= '0' && *significand_end <= '9') {
            significand_end++;
        } else if (*significand_end == '.' && decimal_point == NULL) {
            decimal_point = significand_end;
            significand_end++;
        } else if ((*significand_end == 'e' || *significand_end == 'E') && (significand_end > significand_start)) {
            if ((*(significand_end - 1) >= '0' && *(significand_end - 1) <= '9') ||
                (*(significand_end - 1) == '.' && (significand_end - 1) > significand_start)) {
                has_exponent = true;
                break;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    // --- END Significand Scan ---

    // Check if we actually parsed any digits or just "."
    if (significand_end == significand_start && !decimal_point) {                                                   /* No digits */
    } else if (decimal_point && significand_end == decimal_point + 1 && significand_end == significand_start + 1) { /* Just "." */
    } else {
        // --- BEGIN Significand Construction ---
        mpz_t sig_num_z, sig_den_z;
        mpz_inits(sig_num_z, sig_den_z, NULL);
        bool sig_ok = false;

        size_t sig_len = significand_end - significand_start;
        char *sig_num_str = (char *)malloc(sig_len + 1);
        if (!sig_num_str) { /* OOM */
        } else {
            char *dst = sig_num_str;
            const char *src = significand_start;
            while (src < significand_end) {
                if (*src != '.') { *dst++ = *src; }
                src++;
            }
            *dst = '\0';

            if (mpz_set_str(sig_num_z, sig_num_str, 10) == 0) {
                if (decimal_point) {
                    size_t fractional_digits = significand_end - decimal_point - 1;
                    if (fractional_digits > 0) {
                        mpz_ui_pow_ui(sig_den_z, 10, fractional_digits);
                    } else {
                        mpz_set_ui(sig_den_z, 1);
                    }
                } else {
                    mpz_set_ui(sig_den_z, 1);
                }

                mpq_set_num(significand_q, sig_num_z);
                mpq_set_den(significand_q, sig_den_z);
                mpq_canonicalize(significand_q);
                sig_ok = true;
            }
            free(sig_num_str);
        }
        mpz_clears(sig_num_z, sig_den_z, NULL);
        // --- END Significand Construction ---

        if (sig_ok) {  // Proceed only if significand was parsed correctly
            if (has_exponent) {
                // --- BEGIN Exponent Handling ---
                const char *exp_ptr = significand_end + 1;
                bool exp_negative = false;
                if (*exp_ptr == '+') {
                    exp_ptr++;
                } else if (*exp_ptr == '-') {
                    exp_negative = true;
                    exp_ptr++;
                }

                if (*exp_ptr == '\0' || !(*exp_ptr >= '0' && *exp_ptr <= '9')) {
                    sig_ok = false;  // Invalid exponent
                } else {
                    const char *exp_start = exp_ptr;
                    while (*exp_ptr >= '0' && *exp_ptr <= '9') {
                        exp_ptr++;
                    }

                    mpz_t exp_magnitude_z;
                    mpz_init(exp_magnitude_z);
                    bool exp_mag_ok = false;
                    size_t exp_len = exp_ptr - exp_start;
                    char *exp_str = (char *)malloc(exp_len + 1);
                    if (!exp_str) {
                        sig_ok = false; /* OOM */
                    } else {
                        memcpy(exp_str, exp_start, exp_len);
                        exp_str[exp_len] = '\0';
                        if (mpz_set_str(exp_magnitude_z, exp_str, 10) == 0) { exp_mag_ok = true; }
                        free(exp_str);
                    }

                    if (exp_mag_ok) {
                        mpq_t factor_q;
                        mpq_init(factor_q);
                        bool factor_ok = false;

                        if (!mpz_fits_ulong_p(exp_magnitude_z)) { /* Exponent too large */
                        } else {
                            unsigned long exp_magnitude_ul = mpz_get_ui(exp_magnitude_z);
                            if (exp_magnitude_ul == 0) {
                                mpq_set_ui(factor_q, 1, 1);
                                factor_ok = true;
                            } else {
                                mpz_t power_of_10_z;
                                mpz_init(power_of_10_z);
                                mpz_ui_pow_ui(power_of_10_z, 10, exp_magnitude_ul);
                                mpq_set_z(factor_q, power_of_10_z);
                                mpz_clear(power_of_10_z);
                                factor_ok = true;
                            }
                        }

                        if (factor_ok) {
                            if (exp_negative) {
                                if (mpz_sgn(mpq_numref(factor_q)) == 0) {
                                    sig_ok = false; /* Div by zero */
                                } else {
                                    mpq_div(significand_q, significand_q, factor_q);
                                }
                            } else {
                                mpq_mul(significand_q, significand_q, factor_q);
                            }
                            if (sig_ok) {  // Only set if division/multiplication succeeded
                                decimal_sci_end_ptr = exp_ptr;
                                decimal_sci_parsed = true;
                            }
                        } else {
                            sig_ok = false;
                        }  // Factor calculation failed
                        mpq_clear(factor_q);
                    } else {
                        sig_ok = false;
                    }  // Exponent magnitude parsing failed
                    mpz_clear(exp_magnitude_z);
                }
                // --- END Exponent Handling ---
            } else {  // No exponent
                decimal_sci_end_ptr = significand_end;
                decimal_sci_parsed = true;
            }
        }  // End if(sig_ok)
    }  // End else (parsed digits or '.')

    // --- Decide which result to use (Base 10 only) ---
    bool success = false;
    const char *final_end_ptr = start;  // Default end pointer

    if (decimal_sci_parsed && (!gmp_end_ptr || decimal_sci_end_ptr > gmp_end_ptr)) {
        // Decimal/scientific parser succeeded and consumed more (or mpq_set_str failed/consumed less)
        if (negative) { mpq_neg(significand_q, significand_q); }
        mpq_canonicalize(significand_q);
        mpq_set(result_q, significand_q);  // Copy result
        final_end_ptr = decimal_sci_end_ptr;
        success = true;
    } else if (gmp_end_ptr) {
        // mpq_set_str succeeded and consumed as much or more
        // Value is already in temp_q (no sign applied yet by mpq_set_str)
        mpq_set(result_q, temp_q);  // Copy result
        final_end_ptr = gmp_end_ptr;
        success = true;
    } else {
        // Neither succeeded for base 10
        success = false;
    }

    mpq_clear(temp_q);
    mpq_clear(significand_q);  // <<< Clear significand_q

    *end_ptr = final_end_ptr;
    return success;
}

static sl_object *parse_character_literal(const char **input) {
    const char *start = *input;  // Points right after #\

    // Check for named characters first
    if (strncmp(start, "newline", 7) == 0 && !isalnum((unsigned char)start[7])) {
        *input += 7;
        return sl_make_char(0x0A);  // Use code point for newline
    } else if (strncmp(start, "space", 5) == 0 && !isalnum((unsigned char)start[5])) {
        *input += 5;
        return sl_make_char(0x20);  // Use code point for space
    } else if (strncmp(start, "tab", 3) == 0 && !isalnum((unsigned char)start[3])) {
        *input += 3;
        return sl_make_char(0x09);  // Use code point for tab
    }
    // TODO: Add other named characters (#\return, #\alarm etc.)
    // TODO: Add #\uXXXX and #\UXXXXXXXX parsing
    // TODO: Add direct UTF-8 character parsing (e.g., #\é)

    // Check for hex escape #\xHH
    if (start[0] == 'x' && isxdigit((unsigned char)start[1])) {
        char hex_str[3] = {0};
        hex_str[0] = start[1];
        if (isxdigit((unsigned char)start[2])) {  // Two hex digits
            hex_str[1] = start[2];
            *input += 3;  // Consumed xHH
        } else {          // Only one hex digit
            *input += 2;  // Consumed xH
        }
        long char_code = strtol(hex_str, NULL, 16);
        // Basic check for valid Unicode range (up to 0x10FFFF)
        if (char_code >= 0 && char_code <= 0x10FFFF &&
            !(char_code >= 0xD800 && char_code <= 0xDFFF)) {  // Exclude surrogates
            return sl_make_char((uint32_t)char_code);
        } else {
            fprintf(stderr, "Error: Invalid hex character code #\\x%s\n", hex_str);  // <<< REPLACE
            return SL_NIL;                                                           // Indicate parse error
        }
    }

    // Otherwise, it must be a single ASCII character literal for now
    // (UTF-8 decoding needed for multi-byte chars like #\é)
    if (start[0] == '\0') {
        fprintf(stderr, "Error: Unexpected end of input after #\\\n");  // <<< REPLACE
        return SL_NIL;
    }

    // Create a temporary pointer to pass to the decoder
    const char *char_ptr = start;
    uint32_t code_point = decode_utf8(&char_ptr);  // decode_utf8 advances char_ptr

    if (code_point == 0 && char_ptr == start) {  // Check if decode_utf8 hit EOF immediately
        fprintf(stderr, "Error: Unexpected end of input after #\\\n");
        return SL_NIL;
    }
    if (code_point == UTF8_REPLACEMENT_CHAR) {
        // Decoder encountered an error
        fprintf(stderr, "Error: Invalid UTF-8 sequence for character literal starting with byte 0x%02X\n", (unsigned char)*start);
        *input = char_ptr;  // Update main pointer past the invalid byte
        return SL_NIL;
    }
    *input = char_ptr;                // Update main pointer past the consumed bytes
    return sl_make_char(code_point);  // Cast ASCII char to code point
}

// Skips whitespace characters and Scheme comments (;) in the input string.
// Modifies the input pointer to point after the skipped characters.
void skip_whitespace_and_comments(const char **input) {
    const char *ptr = *input;
    while (true) {
        // Skip standard whitespace
        while (isspace(*ptr)) {
            ptr++;
        }

        // Check for comment start
        if (*ptr == ';') {
            // Consume comment until newline or end of string
            while (*ptr != '\0' && *ptr != '\n') {
                ptr++;
            }
            // If we stopped at newline, consume it too and continue skipping
            // If we stopped at null terminator, the outer loop will handle it.
            if (*ptr == '\n') {
                ptr++;
                continue;  // Go back to check for more whitespace/comments after newline
            } else {
                // End of string reached after comment
                break;
            }
        }

        // If it's not whitespace and not a comment, stop skipping.
        break;
    }
    *input = ptr;  // Update the caller's pointer
}

// Parses the next S-expression from the input stream
static sl_object *parse_expression(const char **input) {
    skip_whitespace_and_comments(input);  // <<< Call skip here

    char current_char = **input;
    const char *start_ptr = *input;  // <<< Store position before parsing

    if (current_char == '\0') {
        // End of input
        return SL_NIL;  // Indicate end of input or nothing found
    } else if (current_char == '(') {
        // Start of a list
        return parse_list(input);
    } else if (current_char == ')') {
        // Unexpected closing parenthesis
        fprintf(stderr, "Error: Unexpected ')'.\n");
        return SL_NIL;  // Indicate error
    } else if (current_char == '\'') {
        // Quote reader macro: '<datum> -> (quote <datum>)
        (*input)++;                            // Consume the quote character
        const char *datum_start_ptr = *input;  // <<< Store position before parsing datum

        sl_object *datum = parse_expression(input);  // Parse the datum

        // --- CORRECTED ERROR/EOF CHECK ---
        if (!datum || datum == SL_OUT_OF_MEMORY_ERROR || sl_is_error(datum)) {
            // Error parsing the datum after quote, or OOM
            return datum;  // Propagate error or OOM
        }
        // Check specifically for EOF/failure *before* datum parsing started
        if (datum == SL_NIL && *input == datum_start_ptr) {
            fprintf(stderr, "Error: Unexpected end of input after quote (').\n");
            return SL_NIL;  // Return NIL indicating parse failure
        }
        // --- END CORRECTION ---
        // If we reach here, 'datum' is a valid object (including SL_NIL if '()' was parsed)

        sl_object *quote_sym = sl_make_symbol("quote");
        sl_gc_add_root(&quote_sym);  // Protect symbol
        sl_object *quoted_list = sl_make_pair(datum, SL_NIL);
        sl_gc_add_root(&quoted_list);  // Protect list element pair
        sl_object *result = sl_make_pair(quote_sym, quoted_list);
        sl_gc_remove_root(&quoted_list);
        sl_gc_remove_root(&quote_sym);
        // Check allocation result for the final pair
        CHECK_ALLOC(result);  // Use CHECK_ALLOC which returns on failure
        return result;
    } else if (current_char == '"') {
        // Start of a string literal
        return parse_string_literal(input);
    } else {
        // Assume it's an atom (symbol, number, boolean)
        return parse_atom(input);
    }
}

// --- Other Parsing Helpers ---

// Checks if a character is a delimiter for atoms
static bool is_delimiter(char c) {
    return isspace(c) || c == '(' || c == ')' || c == '"' || c == ';' || c == '\0';
}

// Parses a list starting after the opening '('
static sl_object *parse_list(const char **input) {
    (*input)++;  // Consume '('

    sl_object *head = SL_NIL;
    sl_object *tail = SL_NIL;
    sl_gc_add_root(&head);  // Protect list head during construction

    while (true) {
        skip_whitespace_and_comments(input);  // <<< Skip before checking element/end
        char current_char = **input;

        if (current_char == ')') {
            // End of list
            (*input)++;  // Consume ')'
            sl_gc_remove_root(&head);
            return head;
        }

        if (current_char == '\0') {
            fprintf(stderr, "Error: Unterminated list (reached end of input).\n");
            sl_gc_remove_root(&head);
            return SL_NIL;  // Indicate error
        }

        // Check for dotted pair syntax: . <datum> )
        // Need to check that '.' is followed by whitespace or delimiter
        if (current_char == '.' && is_delimiter((*input)[1])) {
            (*input)++;                           // Consume '.'
            skip_whitespace_and_comments(input);  // <<< Skip space after dot

            sl_object *cdr_val = parse_expression(input);
            if (!cdr_val || cdr_val == SL_OUT_OF_MEMORY_ERROR || sl_is_error(cdr_val)) {
                fprintf(stderr, "Error: Failed to parse datum after '.' in list.\n");
                sl_gc_remove_root(&head);
                return cdr_val;  // Propagate error or OOM
            }
            if (cdr_val == SL_NIL) {  // Check if cdr parsing hit EOF unexpectedly
                fprintf(stderr, "Error: Unexpected end of input after '.' in list.\n");
                sl_gc_remove_root(&head);
                return SL_NIL;
            }

            skip_whitespace_and_comments(input);  // <<< Skip before checking for ')'
            if (**input != ')') {
                fprintf(stderr, "Error: Expected ')' after datum following '.' in list, got '%c'.\n", **input);
                sl_gc_remove_root(&head);
                return SL_NIL;  // Missing closing parenthesis
            }
            (*input)++;  // Consume ')'

            if (head == SL_NIL) {
                fprintf(stderr, "Error: Dot notation '.' cannot appear at the beginning of a list.\n");
                sl_gc_remove_root(&head);
                return SL_NIL;
            }

            // Set the cdr of the last pair
            sl_set_cdr(tail, cdr_val);  // Assumes sl_set_cdr exists and handles GC if needed
            sl_gc_remove_root(&head);
            return head;  // Return the head of the now dotted list
        }

        // Parse the next element in the list
        // No need to skip here, parse_expression handles its own leading skip
        sl_object *element = parse_expression(input);
        if (!element || element == SL_OUT_OF_MEMORY_ERROR || sl_is_error(element)) {
            // Error parsing element, or OOM
            sl_gc_remove_root(&head);
            return element;  // Propagate error or OOM
        }
        if (element == SL_NIL && 0) {  // Check if element parsing hit EOF unexpectedly
            fprintf(stderr, "Error: Unexpected end of input while parsing list element.\n");
            sl_gc_remove_root(&head);
            return SL_NIL;
        }

        // Append the element to the list
        sl_object *new_pair = sl_make_pair(element, SL_NIL);
        CHECK_ALLOC(new_pair);  // Check allocation failure

        if (head == SL_NIL) {
            // First element
            head = new_pair;
            tail = new_pair;
        } else {
            // Subsequent elements
            sl_set_cdr(tail, new_pair);  // Assumes sl_set_cdr exists
            tail = new_pair;
        }
        // Rooting 'tail' isn't strictly necessary if sl_set_cdr doesn't GC
        // and head is already rooted.
    }
}

// Parses an atom (symbol, number, boolean)
static sl_object *parse_atom(const char **input) {
    const char *start = *input;
    // --- Handle # literals first ---
    if (start[0] == '#') {
        if (start[1] == 't' && !is_delimiter(start[2])) {  // Check delimiter
            *input += 2;
            return SL_TRUE;
        } else if (start[1] == 'f' && !is_delimiter(start[2])) {  // Check delimiter
            *input += 2;
            return SL_FALSE;
        } else if (start[1] == '\\') {
            *input += 2;                            // Consume #\
            // --- CALL PARSE_CHARACTER_LITERAL ---
            return parse_character_literal(input);  // <<< CORRECTED CALL
        }
        // Add other # literals here (#;, #! etc.) if needed

        // If # is followed by something else unexpected for a literal,
        // it might be part of a symbol (though unusual). Let it fall through.
    }

    // --- If not a # literal, proceed to read the atom token ---
    const char *ptr = start;

    // Read until a delimiter is found
    // Note: is_delimiter now implicitly handles ';' via isspace check in caller (skip_whitespace...)
    // but we should explicitly stop at ';' too, just in case.
    while (!is_delimiter(*ptr)) {
        ptr++;
    }

    size_t len = ptr - start;
    if (len == 0) {
        // This shouldn't happen if called correctly after checking non-delimiter start
        fprintf(stderr, "Internal Error: Tried to parse empty atom.\n");
        return SL_NIL;
    }

    // --- Create a null-terminated copy for parsing ---
    char *token = (char *)malloc(len + 1);
    if (!token) { /* ... OOM error handling ... */
        return SL_OUT_OF_MEMORY_ERROR;
    }
    memcpy(token, start, len);
    token[len] = '\0';

    sl_object *result = NULL;

    // --- Try parsing as number using the new helper ---
    mpq_t temp_q;
    mpq_init(temp_q);
    const char *num_end_ptr = NULL;
    // Use base 10 for general atom parsing
    if (parse_rational_from_string(token, 10, temp_q, &num_end_ptr)) {
        // Check if the entire token was consumed
        if (*num_end_ptr == '\0') {
            // Successfully parsed the whole token as a number
            result = make_number_from_mpq(temp_q);  // Use existing helper to simplify if possible
        }
        // If *num_end_ptr != '\0', it means there were trailing chars, so not a valid number token.
    }
    mpq_clear(temp_q);

    // --- If not a valid number, try symbol/boolean ---
    if (!result) {
        if (strcmp(token, "#t") == 0) {  // Check booleans again (should be caught by # prefix, but safe)
            result = SL_TRUE;
        } else if (strcmp(token, "#f") == 0) {
            result = SL_FALSE;
        } else {
            // Assume symbol
            result = sl_make_symbol(token);
        }
    }

    free(token);  // Free the temporary token string

    if (result == SL_OUT_OF_MEMORY_ERROR) return result;  // Propagate OOM

    if (result) {
        *input = ptr;  // Consume the atom characters if parsing succeeded
        return result;
    }

    // If neither worked, it's an error
    *input = ptr;  // Consume the characters anyway to avoid loops
    fprintf(stderr, "Error: Could not parse '%.*s' as a number, symbol, or boolean.\n", (int)len, start);
    return SL_NIL;  // Indicate error
}

// Parses a string literal starting after the opening '"'
static sl_object *parse_string_literal(const char **input) {
    (*input)++;  // Consume opening '"'
    const char *ptr = *input;
    sl_string_buffer sbuf;
    sbuf_init(&sbuf);
    bool success = true;

    while (success) {  // Loop while successful
        char current_char = *ptr;

        if (current_char == '"') {
            ptr++;  // Consume closing '"'
            break;  // End of string
        }

        if (current_char == '\0') {
            fprintf(stderr, "Error: Unterminated string literal.\n");
            success = false;
            break;  // Unterminated
        }

        if (current_char == '\\') {
            // --- Handle Escape Sequence ---
            ptr++;  // Consume '\'
            char escaped_char = *ptr;
            if (escaped_char == '\0') {
                fprintf(stderr, "Error: Unterminated escape sequence in string literal.\n");
                success = false;
                break;
            }
            ptr++;  // Consume the character after '\'

            char char_to_append = 0;  // Will be set if simple escape
            bool simple_escape = true;

            switch (escaped_char) {
            case 'n':
                char_to_append = '\n';
                break;
            case 't':
                char_to_append = '\t';
                break;
            case '"':
                char_to_append = '"';
                break;
            case '\\':
                char_to_append = '\\';
                break;
            // Add \r etc. if needed
            case 'x': {                 // Hex escape \xHH
                simple_escape = false;  // Not a simple char append
                char hex_str[3] = {0};
                // Check if the *next two* chars are hex digits
                if (isxdigit((unsigned char)ptr[0]) && isxdigit((unsigned char)ptr[1])) {
                    hex_str[0] = ptr[0];
                    hex_str[1] = ptr[1];
                    ptr += 2;  // Consume HH *after* the 'x'
                    long byte_val = strtol(hex_str, NULL, 16);
                    if (!sbuf_append_char(&sbuf, (char)byte_val)) { success = false; }
                } else {
                    fprintf(stderr, "Error: Invalid hex escape sequence \\x%.*s\n",
                            isxdigit((unsigned char)ptr[0]) ? 2 : (ptr[0] ? 1 : 0), ptr);
                    success = false;
                    // Consume the bad chars after \x to avoid loops
                    if (isxdigit((unsigned char)ptr[0])) ptr++;
                    if (isxdigit((unsigned char)ptr[1])) ptr++;  // Might consume too much if only 1 hex digit
                }
                break;  // Break from switch
            }
            default:
                // Invalid escape sequence
                simple_escape = false;  // Not a simple char append
                fprintf(stderr, "Error: Invalid escape sequence '\\%c' in string literal.\n", escaped_char);
                success = false;
                break;  // Break from switch
            }

            // Append character for simple escapes if successful so far
            if (simple_escape && success) {
                if (!sbuf_append_char(&sbuf, char_to_append)) {
                    success = false;
                }
            }
            // --- End Handle Escape Sequence ---

        } else {
            // --- Handle Regular Character ---
            // Append the byte directly (handles UTF-8 implicitly)
            if (!sbuf_append_char(&sbuf, current_char)) {
                success = false;
            }
            ptr++;  // Consume the regular character
            // --- End Handle Regular Character ---
        }
    }  // End while(success)

    // No goto needed

    *input = ptr;  // Update the main input pointer

    sl_object *str_obj = SL_NIL;
    if (success) {
        // Null-terminate buffer before creating object
        if (!sbuf_append_char(&sbuf, '\0')) {
            fprintf(stderr, "Error: Failed to null-terminate string buffer.\n");
            success = false;
        } else {
            // Adjust length back as null terminator isn't part of the Scheme string value
            sbuf.length--;
            const char *string_to_make = (sbuf.buffer != NULL) ? sbuf.buffer : "";
            str_obj = sl_make_string(string_to_make);
            if (str_obj == SL_OUT_OF_MEMORY_ERROR) {
                fprintf(stderr, "Error: Out of memory creating string object.\n");
                success = false;   // Mark as failure
                str_obj = SL_NIL;  // Ensure NIL is returned
            }
        }
    }

    sbuf_free(&sbuf);
    return success ? str_obj : SL_NIL;  // Return object or NIL on failure
}

// --- parse_number and parse_symbol_or_bool implementations ---

// Parses a token as a number (integer or rational)
// Returns a number object or NULL if parsing fails.
// Does NOT consume input; parse_atom handles that.
static sl_object *parse_number(const char **input_ptr, const char *start, size_t len) {
    // Create a null-terminated copy for C string functions
    char *token = (char *)malloc(len + 1);
    if (!token) {
        perror("malloc failed for number token");
        return NULL;
    }
    memcpy(token, start, len);
    token[len] = '\0';

    sl_object *result = NULL;

    // --- Try GMP parsing directly ---
    mpq_t temp_q;
    mpq_init(temp_q);

    // mpq_set_str returns 0 on success, -1 on failure
    if (mpq_set_str(temp_q, token, 10) == 0) {
        // Successfully parsed by GMP
        // Canonicalize the fraction (e.g., 4/2 -> 2/1)
        mpq_canonicalize(temp_q);

        // Check if it fits into a small int representation
        mpz_t num_z, den_z;
        mpz_inits(num_z, den_z, NULL);
        mpq_get_num(num_z, temp_q);
        mpq_get_den(den_z, temp_q);

        // Check if denominator is 1 and numerator fits int64_t
        if (mpz_cmp_si(den_z, 1) == 0 && fits_int64(num_z)) {
            result = sl_make_number_si(mpz_get_si(num_z), 1);
        }
        // Check if both numerator and denominator fit int64_t
        else if (fits_int64(num_z) && fits_int64(den_z)) {
            result = sl_make_number_si(mpz_get_si(num_z), mpz_get_si(den_z));
        } else {
            // Doesn't fit small int, use the bignum directly
            result = sl_make_number_q(temp_q);  // This copies temp_q
        }

        mpz_clears(num_z, den_z, NULL);

    } else {
        // mpq_set_str failed, so it's not a valid number format for GMP
        result = NULL;
    }

    mpq_clear(temp_q);  // Clear the temporary GMP rational
    free(token);        // Free the temporary token string
    return result;      // NULL if parsing failed
}

// Parses a token as a symbol or boolean (#t, #f)
// Returns a symbol or boolean object, or NULL on allocation failure.
// Does NOT consume input; parse_atom handles that.
static sl_object *parse_symbol_or_bool(const char **input_ptr, const char *start, size_t len) {
    // Create a null-terminated copy for strcmp
    char *token = (char *)malloc(len + 1);
    if (!token) {
        perror("malloc failed for symbol token");
        return NULL;
    }
    memcpy(token, start, len);
    token[len] = '\0';

    sl_object *result = NULL;

    // Check for booleans
    if (strcmp(token, "#t") == 0) {
        result = SL_TRUE;
    } else if (strcmp(token, "#f") == 0) {
        result = SL_FALSE;
    } else {
        // Not a boolean, assume it's a symbol
        // TODO: Validate symbol characters? (e.g., cannot start with number if not a number)
        // The current structure relies on parse_number failing first.
        result = sl_make_symbol(token);  // sl_make_symbol handles copying/interning
    }

    free(token);    // Free the temporary token string
    return result;  // SL_NIL if sl_make_symbol fails allocation
}

// Helper to peek at the next character without consuming it
static int peek_char(FILE *stream) {
    int c = fgetc(stream);
    if (c != EOF) {
        ungetc(c, stream);
    }
    return c;
}

// Skips whitespace and comments in a stream
void skip_stream_whitespace_and_comments(FILE *stream) {
    int c;
    while (true) {
        c = fgetc(stream);
        // Skip standard whitespace
        while (isspace(c)) {
            c = fgetc(stream);
        }

        // Check for comment start
        if (c == ';') {
            // Consume comment until newline or EOF
            while ((c = fgetc(stream)) != EOF && c != '\n') {
                // Consume
            }
            // If we stopped at newline, continue skipping after it
            // If we stopped at EOF, the outer loop will handle it.
            if (c == '\n') {
                continue;  // Go back to check for more whitespace/comments
            } else {
                // EOF reached after comment
                break;
            }
        }

        // If it's not whitespace and not a comment, put it back and stop.
        if (c != EOF) {
            ungetc(c, stream);
        }
        break;
    }
}

// Parses the next S-expression from the input stream
static sl_object *parse_stream_expression(FILE *stream) {
    skip_stream_whitespace_and_comments(stream);

    int current_char = peek_char(stream);

    if (current_char == EOF) {
        return SL_EOF_OBJECT;  // Use the dedicated EOF object
    } else if (current_char == '(') {
        return parse_stream_list(stream);
    } else if (current_char == ')') {
        fprintf(stderr, "Error: Unexpected ')'.\n");
        fgetc(stream);          // Consume the ')' to avoid loops
        return SL_PARSE_ERROR;  // Use the dedicated parse error object
    } else if (current_char == '\'') {
        fgetc(stream);  // Consume the quote character

        sl_object *datum = parse_stream_expression(stream);  // Parse the datum

        if (!datum || datum == SL_OUT_OF_MEMORY_ERROR || datum == SL_PARSE_ERROR || datum == SL_EOF_OBJECT) {
            if (datum == SL_EOF_OBJECT) {
                fprintf(stderr, "Error: Unexpected end of input after quote (').\n");
            }
            // Error parsing the datum after quote, or OOM, or EOF
            return datum;  // Propagate error, OOM, or EOF
        }

        sl_object *quote_sym = sl_make_symbol("quote");
        CHECK_ALLOC(quote_sym);
        sl_gc_add_root(&quote_sym);
        sl_object *quoted_list = sl_make_pair(datum, SL_NIL);
        CHECK_ALLOC(quoted_list);
        sl_gc_add_root(&quoted_list);
        sl_object *result = sl_make_pair(quote_sym, quoted_list);
        sl_gc_remove_root(&quoted_list);
        sl_gc_remove_root(&quote_sym);
        CHECK_ALLOC(result);
        return result;
    } else if (current_char == '"') {
        return parse_stream_string_literal(stream);
    } else {
        // Assume it's an atom (symbol, number, boolean, character)
        return parse_stream_atom(stream);
    }
}

// Parses a list from a stream starting after the opening '('
static sl_object *parse_stream_list(FILE *stream) {
    fgetc(stream);  // Consume '('

    sl_object *head = SL_NIL;
    sl_object *tail = SL_NIL;
    sl_gc_add_root(&head);  // Protect list head

    while (true) {
        skip_stream_whitespace_and_comments(stream);
        int current_char = peek_char(stream);

        if (current_char == ')') {
            fgetc(stream);  // Consume ')'
            sl_gc_remove_root(&head);
            return head;
        }

        if (current_char == EOF) {
            fprintf(stderr, "Error: Unterminated list (reached end of input).\n");
            sl_gc_remove_root(&head);
            return SL_PARSE_ERROR;
        }

        // Check for dotted pair syntax: . <datum> )
        if (current_char == '.') {
            fgetc(stream);  // Consume '.'
            int next_char = peek_char(stream);
            if (!is_stream_delimiter(next_char)) {
                // It's not '. ' or similar, maybe part of a symbol like ".."? Put '.' back.
                ungetc('.', stream);
                // Fall through to parse as atom/element
            } else {
                // It is dot notation
                skip_stream_whitespace_and_comments(stream);  // Skip space after dot

                sl_object *cdr_val = parse_stream_expression(stream);
                if (!cdr_val || cdr_val == SL_OUT_OF_MEMORY_ERROR || cdr_val == SL_PARSE_ERROR || cdr_val == SL_EOF_OBJECT) {
                    fprintf(stderr, "Error: Failed to parse datum after '.' in list.\n");
                    sl_gc_remove_root(&head);
                    return cdr_val;  // Propagate error, OOM, or EOF
                }

                skip_stream_whitespace_and_comments(stream);
                if (peek_char(stream) != ')') {
                    fprintf(stderr, "Error: Expected ')' after datum following '.' in list.\n");
                    sl_gc_remove_root(&head);
                    return SL_PARSE_ERROR;
                }
                fgetc(stream);  // Consume ')'

                if (head == SL_NIL) {
                    fprintf(stderr, "Error: Dot notation '.' cannot appear at the beginning of a list.\n");
                    sl_gc_remove_root(&head);
                    return SL_PARSE_ERROR;
                }

                sl_set_cdr(tail, cdr_val);
                sl_gc_remove_root(&head);
                return head;
            }
        }

        // Parse the next element in the list
        sl_object *element = parse_stream_expression(stream);
        if (!element || element == SL_OUT_OF_MEMORY_ERROR || element == SL_PARSE_ERROR || element == SL_EOF_OBJECT) {
            if (element == SL_EOF_OBJECT) {
                fprintf(stderr, "Error: Unexpected end of input while parsing list element.\n");
            }
            sl_gc_remove_root(&head);
            return element;  // Propagate error, OOM, or EOF
        }

        // Append the element to the list
        sl_object *new_pair = sl_make_pair(element, SL_NIL);
        CHECK_ALLOC(new_pair);

        if (head == SL_NIL) {
            head = new_pair;
            tail = new_pair;
        } else {
            sl_set_cdr(tail, new_pair);
            tail = new_pair;
        }
    }
}

// Parses an atom (symbol, number, boolean, character) from a stream
static sl_object *parse_stream_atom(FILE *stream) {
    int first_char = peek_char(stream);

    // --- Handle # literals first ---
    if (first_char == '#') {
        fgetc(stream);  // Consume '#'
        int second_char = peek_char(stream);
        if (second_char == 't') {
            fgetc(stream);                                  // Consume 't'
            if (!is_stream_delimiter(peek_char(stream))) {  // Check delimiter
                ungetc('t', stream);                        // Put back 't'
                ungetc('#', stream);                        // Put back '#' - treat as symbol starting with #
            } else {
                return SL_TRUE;
            }
        } else if (second_char == 'f') {
            fgetc(stream);                                  // Consume 'f'
            if (!is_stream_delimiter(peek_char(stream))) {  // Check delimiter
                ungetc('f', stream);                        // Put back 'f'
                ungetc('#', stream);                        // Put back '#' - treat as symbol starting with #
            } else {
                return SL_FALSE;
            }
        } else if (second_char == '\\') {
            fgetc(stream);  // Consume '\'
            return parse_stream_character_literal(stream);
        } else {
            // Unknown # sequence, treat as symbol starting with #
            ungetc('#', stream);  // Put '#' back
        }
    }

    // --- If not a # literal, read the atom token ---
    sl_string_buffer sbuf;
    sbuf_init(&sbuf);
    int c;
    while (!is_stream_delimiter(c = fgetc(stream))) {
        if (!sbuf_append_char(&sbuf, (char)c)) {
            sbuf_free(&sbuf);
            return SL_OUT_OF_MEMORY_ERROR;
        }
    }
    if (c != EOF) {
        ungetc(c, stream);  // Put back the delimiter
    }

    if (sbuf.length == 0) {  // Should not happen if called correctly
        sbuf_free(&sbuf);
        fprintf(stderr, "Internal Error: Tried to parse empty atom from stream.\n");
        return SL_PARSE_ERROR;
    }

    // Null-terminate the buffer
    if (!sbuf_append_char(&sbuf, '\0')) {  // Ensure space for null terminator
        sbuf_free(&sbuf);
        return SL_OUT_OF_MEMORY_ERROR;
    }

    // --- Try parsing token as number or symbol ---
    sl_object *result = NULL;
    const char *token = sbuf.buffer;

    // --- Try parsing token as number using the new helper ---
    mpq_t temp_q;
    mpq_init(temp_q);
    const char *num_end_ptr = NULL;
    // Use base 10 for general atom parsing
    if (parse_rational_from_string(token, 10, temp_q, &num_end_ptr)) {
        // Check if the entire token was consumed
        if (*num_end_ptr == '\0') {
            // Successfully parsed the whole token as a number
            result = make_number_from_mpq(temp_q);  // Use existing helper to simplify if possible
        }
    }
    mpq_clear(temp_q);

    // --- If not a valid number, try symbol/boolean ---
    if (!result) {
        if (strcmp(token, "#t") == 0) {  // Check booleans again
            result = SL_TRUE;
        } else if (strcmp(token, "#f") == 0) {
            result = SL_FALSE;
        } else {
            // Assume symbol
            result = sl_make_symbol(token);
        }
    }

    sbuf_free(&sbuf);

    // Check if object creation failed due to memory
    if (result == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Parser Error: Out of memory creating object for atom.\n");
        return SL_OUT_OF_MEMORY_ERROR;
    }
    if (!result) {  // Should only happen if sl_make_symbol fails for non-OOM reasons (unlikely)
        fprintf(stderr, "Parser Error: Failed to create object for atom '%s'.\n", token);
        return SL_PARSE_ERROR;
    }

    return result;
}

// Parses a string literal from stream starting after the opening '"'
static sl_object *parse_stream_string_literal(FILE *stream) {
    fgetc(stream);  // Consume opening '"'
    sl_string_buffer sbuf;
    sbuf_init(&sbuf);
    bool success = true;
    int current_char;

    while (success) {
        current_char = fgetc(stream);

        if (current_char == '"') {
            break;  // End of string
        }

        if (current_char == EOF) {
            fprintf(stderr, "Error: Unterminated string literal (EOF).\n");
            success = false;
            break;
        }

        if (current_char == '\\') {
            // Handle Escape Sequence
            int escaped_char = fgetc(stream);
            if (escaped_char == EOF) {
                fprintf(stderr, "Error: Unterminated escape sequence in string literal (EOF).\n");
                success = false;
                break;
            }

            char char_to_append = 0;
            bool simple_escape = true;

            switch (escaped_char) {
            case 'n':
                char_to_append = '\n';
                break;
            case 't':
                char_to_append = '\t';
                break;
            case '"':
                char_to_append = '"';
                break;
            case '\\':
                char_to_append = '\\';
                break;
            case 'x': {  // Hex escape \xHH
                simple_escape = false;
                int d1 = fgetc(stream);
                int d2 = fgetc(stream);
                if (isxdigit(d1) && isxdigit(d2)) {
                    char hex_str[3] = {(char)d1, (char)d2, '\0'};
                    long byte_val = strtol(hex_str, NULL, 16);
                    if (!sbuf_append_char(&sbuf, (char)byte_val)) { success = false; }
                } else {
                    fprintf(stderr, "Error: Invalid hex escape sequence \\x%c%c\n", d1, d2);
                    success = false;
                    // Put back non-hex chars if possible
                    if (d2 != EOF) ungetc(d2, stream);
                    if (d1 != EOF) ungetc(d1, stream);
                }
                break;
            }
            default:
                simple_escape = false;
                fprintf(stderr, "Error: Invalid escape sequence '\\%c' in string literal.\n", escaped_char);
                success = false;
                break;
            }
            if (simple_escape && success) {
                if (!sbuf_append_char(&sbuf, char_to_append)) { success = false; }
            }
        } else {
            // Handle Regular Character
            if (!sbuf_append_char(&sbuf, (char)current_char)) { success = false; }
        }
    }  // End while(success)

    sl_object *str_obj = SL_NIL;  // Use NIL as error indicator for now
    if (success) {
        if (!sbuf_append_char(&sbuf, '\0')) {  // Null terminate
            success = false;
            str_obj = SL_OUT_OF_MEMORY_ERROR;
        } else {
            sbuf.length--;  // Don't include null in Scheme string value
            const char *string_to_make = (sbuf.buffer != NULL) ? sbuf.buffer : "";
            str_obj = sl_make_string(string_to_make);
            if (str_obj == SL_OUT_OF_MEMORY_ERROR) {
                success = false;  // Mark as failure
            }
        }
    }

    sbuf_free(&sbuf);
    return success ? str_obj : (str_obj == SL_OUT_OF_MEMORY_ERROR ? str_obj : SL_PARSE_ERROR);
}

// Parses a character literal from stream starting after #\ character
static sl_object *parse_stream_character_literal(FILE *stream) {
    int c1 = fgetc(stream);
    if (c1 == EOF) {
        fprintf(stderr, "Error: Unexpected EOF after #\\\n");
        return SL_PARSE_ERROR;
    }

    // Buffer to read potential multi-character name like "newline" or "xHH"
    char name_buf[10];  // Max length for "newline" + safety
    name_buf[0] = (char)c1;
    size_t name_len = 1;

    // Read subsequent characters if they are not delimiters
    while (name_len < sizeof(name_buf) - 1) {
        int next_c = peek_char(stream);
        if (is_stream_delimiter(next_c)) {
            break;  // End of character name/code
        }
        name_buf[name_len++] = (char)fgetc(stream);  // Consume the char
    }
    name_buf[name_len] = '\0';

    // Check named characters
    if (strcmp(name_buf, "newline") == 0) return sl_make_char('\n');
    if (strcmp(name_buf, "space") == 0) return sl_make_char(' ');
    if (strcmp(name_buf, "tab") == 0) return sl_make_char('\t');
    // Add others...

    // Check hex escape #\xHH
    if (name_buf[0] == 'x' && name_len > 1) {
        long char_code = strtol(name_buf + 1, NULL, 16);  // Parse hex part
        // Basic validation
        if (char_code >= 0 && char_code <= 0x10FFFF && !(char_code >= 0xD800 && char_code <= 0xDFFF)) {
            return sl_make_char((uint32_t)char_code);
        } else {
            fprintf(stderr, "Error: Invalid hex character code #\\%s\n", name_buf);
            return SL_PARSE_ERROR;
        }
    }

    // If only one character was read, treat it as a literal character
    if (name_len == 1) {
        // Need to handle UTF-8 if c1 is the start of a multi-byte sequence.
        // For simplicity now, assume ASCII or single byte from UTF-8.
        // A full solution would involve putting c1 back and using a UTF-8 decoder.
        return sl_make_char((uint32_t)(unsigned char)c1);
    }

    // If none of the above matched
    fprintf(stderr, "Error: Unknown character literal #\\%s\n", name_buf);
    return SL_PARSE_ERROR;
}

// --- Public Parsing Function (Stream) ---
// This is the top-level function called by sl_builtin_read
sl_object *sl_parse_stream(FILE *stream) {
    if (!stream) {
        return SL_NIL;  // Or error?
    }
    return parse_stream_expression(stream);
}

// --- Public Parsing Function (String) ---
sl_object *sl_parse_string(const char *input, const char **end_ptr) {
    if (!input) {
        return SL_NIL;
    }
    const char *current_ptr = input;
    sl_object *result = parse_expression(&current_ptr);  // parse_expression handles initial skip

    if (end_ptr) {
        *end_ptr = current_ptr;  // Point end_ptr after the parsed expression
    }

    // Check if parse_expression returned NIL because of EOF at start
    if (result == SL_NIL && current_ptr == input) {
        // Check if input was just whitespace/comments
        const char *check_ptr = input;
        skip_whitespace_and_comments(&check_ptr);
        if (*check_ptr == '\0') {
            // Input was effectively empty, return NIL is correct.
        } else {
            // Input was not empty, but parsing failed immediately.
            // parse_expression should have printed an error.
        }
    }

    return result;  // Can be SL_NIL or an error object on failure
}

/* obsoleted:
// TODO: Implement robust stream parsing handling EOF, errors, etc.
sl_object *sl_parse_stream(FILE *stream) {
    // Very basic implementation: read into a buffer first.
    // A real implementation would parse directly from the stream.
    char buffer[4096];  // Limited buffer size
    char *ptr = buffer;
    int c;
    int balance = 0;  // Parenthesis balance
    bool in_string = false;

    // Read one S-expression (simplistic approach)
    while ((c = fgetc(stream)) != EOF) {
        if (ptr >= buffer + sizeof(buffer) - 1) {
            fprintf(stderr, "Error: Input expression too long for buffer.\n");
            return SL_NIL;
        }

        *ptr++ = (char)c;

        if (c == '"') {  // Basic string detection, no escape handling here
            // Find matching quote or end of buffer/line? This is tricky.
            // For simplicity, assume strings don't contain critical chars for now.
            // A better stream parser is needed.
            in_string = !in_string;
        }

        if (!in_string) {
            if (c == '(')
                balance++;
            else if (c == ')')
                balance--;
            else if (c == ';') {  // Skip comment line
                while ((c = fgetc(stream)) != EOF && c != '\n')
                    ;
                if (c == EOF) break;
                *(ptr - 1) = ' ';  // Replace semicolon with space
                continue;
            }

            // If balanced and we encounter whitespace after a complete expression
            if (balance == 0 && isspace(c)) {
                // Check if the buffer actually contains more than just whitespace
                const char *temp = buffer;
                skip_whitespace_and_comments(&temp);
                if (temp < ptr && temp[0] != ')') {  // If there was non-whitespace content and not just ending a list
                    break;                           // Found end of one expression
                }
            }
        }
        // If balance drops below zero, it's an error, but we read until whitespace/EOF anyway for simplicity here
    }
    *ptr = '\0';

    if (ptr == buffer) return SL_NIL;  // Nothing read or EOF immediately

    const char *read_ptr = buffer;
    const char *end_ptr_internal = NULL;  // Use internal end_ptr
    sl_object *result = sl_parse_string(read_ptr, &end_ptr_internal);

    // Check if the whole buffer was consumed (ignoring trailing whitespace)
    if (result != SL_NIL && end_ptr_internal != NULL) {
        const char *check_end = end_ptr_internal;
        skip_whitespace_and_comments(&check_end);
        if (*check_end != '\0') {
            fprintf(stderr, "Error: Trailing characters after expression in stream buffer: '%s'\n", check_end);
            // Potentially free 'result' if GC isn't immediate? For now, rely on GC.
            return SL_NIL;
        }
    }
    if (balance != 0 && result != SL_NIL) {
        fprintf(stderr, "Error: Unbalanced parentheses in stream input.\n");
        return SL_NIL;
    }

    return result;
}
*/

// --- Public Write Functions ---

char *sl_write_to_string(sl_object *obj) {
    if (!obj) return NULL;

    sl_string_buffer sbuf;
    sbuf_init(&sbuf);

    if (sl_write_recursive(obj, &sbuf)) {
        // Success, return the buffer (caller owns it now)
        return sbuf.buffer;
    } else {
        // Failure during writing (likely allocation error)
        sbuf_free(&sbuf);
        return NULL;
    }
}

// Helper for stream writing
static bool sl_write_stream_recursive(sl_object *obj, FILE *stream);
static bool sl_write_pair_stream_recursive(sl_object *pair, FILE *stream);

int sl_write_to_stream(sl_object *obj, FILE *stream) {
    if (!obj || !stream) return EOF;
    if (sl_write_stream_recursive(obj, stream)) {
        return 0;  // Indicate success (could return chars written, but 0 is simple)
    } else {
        return EOF;  // Indicate failure
    }
}

// --- Recursive Writing Implementation (String Buffer) ---

static bool sl_write_recursive(sl_object *obj, sl_string_buffer *sbuf) {
    if (!obj) {
        // This case might occur if printing a malformed structure, treat as error?
        // Or print something like #<invalid>? For now, append "NULL_OBJ_ERROR"
        return sbuf_append_str(sbuf, "#<NULL_OBJ_ERROR>");
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
        return sbuf_append_str(sbuf, "()");
    case SL_TYPE_BOOLEAN:
        return sbuf_append_str(sbuf, obj->data.boolean ? "#t" : "#f");
    case SL_TYPE_SYMBOL:
        // Use the accessor macro sl_symbol_name(obj)
        return sbuf_append_str(sbuf, sl_symbol_name(obj));  // Corrected
    case SL_TYPE_STRING: {
        // Use the accessor macro sl_string_value(obj)
        char *str = sl_string_value(obj);  // Corrected
        if (!sbuf_append_char(sbuf, '"')) return false;
        while (*str) {
            char c = *str++;
            const char *escape = NULL;
            switch (c) {
            case '"':
                escape = "\\\"";
                break;
            case '\\':
                escape = "\\\\";
                break;
            case '\n':
                escape = "\\n";
                break;
            case '\t':
                escape = "\\t";
                break;
                // Add other escapes as needed (e.g., \r)
            }
            if (escape) {
                if (!sbuf_append_str(sbuf, escape)) return false;
            } else {
                if (!sbuf_append_char(sbuf, c)) return false;
            }
        }
        return sbuf_append_char(sbuf, '"');
    }
    case SL_TYPE_NUMBER: {
        char num_buffer[256];  // Temporary buffer for number conversion
        if (obj->data.number.is_bignum) {
            // Use gmp_sprintf for direct formatting if possible, or mpq_get_str
            char *gmp_str = mpq_get_str(NULL, 10, obj->data.number.value.big_num);
            if (!gmp_str) return false;  // GMP allocation failed
            bool ok = sbuf_append_str(sbuf, gmp_str);
            // IMPORTANT: mpq_get_str allocates memory using GMP's allocator
            // We need to free it using GMP's free function (or the one configured)
            void (*freefunc)(void *, size_t);
            mp_get_memory_functions(NULL, NULL, &freefunc);
            if (freefunc) {
                freefunc(gmp_str, strlen(gmp_str) + 1);
            } else {
                free(gmp_str);  // Fallback to standard free if GMP uses it
            }
            return ok;
        } else {
            // Small number
            int64_t num = obj->data.number.value.small_num.num;
            int64_t den = obj->data.number.value.small_num.den;
            if (den == 1) {
                snprintf(num_buffer, sizeof(num_buffer), "%" PRId64, num);
            } else {
                snprintf(num_buffer, sizeof(num_buffer), "%" PRId64 "/%" PRId64, num, den);
            }
            return sbuf_append_str(sbuf, num_buffer);
        }
    }
    case SL_TYPE_PAIR:
        return sl_write_pair_recursive(obj, sbuf);
    case SL_TYPE_FUNCTION:
        if (obj->data.function.is_builtin) {
            // Maybe include name: snprintf(buf, size, "#<builtin:%s>", obj->data.function.def.builtin.name);
            return sbuf_append_str(sbuf, "#<builtin>");
        } else {
            return sbuf_append_str(sbuf, "#<procedure>");
        }
    case SL_TYPE_FREE:
        return sbuf_append_str(sbuf, "#<FREE_SLOT>");  // Should not normally be printed
    default:
        return sbuf_append_str(sbuf, "#<UNKNOWN_TYPE>");
    }
    return false;  // Should not be reached
}

static bool sl_write_pair_recursive(sl_object *pair, sl_string_buffer *sbuf) {
    if (!sbuf_append_char(sbuf, '(')) return false;

    sl_object *current = pair;
    bool first = true;

    while (true) {
        if (!first) {
            if (!sbuf_append_char(sbuf, ' ')) return false;
        }
        first = false;

        // Write car
        if (!sl_write_recursive(sl_car(current), sbuf)) return false;

        // Check cdr
        sl_object *next = sl_cdr(current);
        if (sl_is_nil(next)) {
            // End of proper list
            break;
        } else if (sl_is_pair(next)) {
            // Continue list
            current = next;
        } else {
            // Dotted pair
            if (!sbuf_append_str(sbuf, " . ")) return false;
            if (!sl_write_recursive(next, sbuf)) return false;
            break;
        }
    }

    return sbuf_append_char(sbuf, ')');
}

// --- Recursive Writing Implementation (Stream) ---

static bool sl_write_stream_recursive(sl_object *obj, FILE *stream) {
    if (!obj) {
        return fprintf(stream, "#<NULL_OBJ_ERROR>") >= 0;
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
        return fprintf(stream, "()") >= 0;
    case SL_TYPE_BOOLEAN:
        return fprintf(stream, obj->data.boolean ? "#t" : "#f") >= 0;
    case SL_TYPE_SYMBOL:
        // Use the accessor macro sl_symbol_name(obj)
        return fprintf(stream, "%s", sl_symbol_name(obj)) >= 0;  // Corrected
    case SL_TYPE_STRING: {
        // Use the accessor macro sl_string_value(obj)
        char *str = sl_string_value(obj);  // Corrected
        if (fputc('"', stream) == EOF) return false;
        while (*str) {
            char c = *str++;
            const char *escape = NULL;
            switch (c) {
            case '"':
                escape = "\\\"";
                break;
            case '\\':
                escape = "\\\\";
                break;
            case '\n':
                escape = "\\n";
                break;
            case '\t':
                escape = "\\t";
                break;
            }
            if (escape) {
                if (fprintf(stream, "%s", escape) < 0) return false;
            } else {
                if (fputc(c, stream) == EOF) return false;
            }
        }
        return fputc('"', stream) != EOF;
    }
    case SL_TYPE_NUMBER: {
        if (obj->data.number.is_bignum) {
            // Use mpq_out_str for direct stream output
            if (mpq_out_str(stream, 10, obj->data.number.value.big_num) == 0) {
                // Error during output
                return false;
            }
            return true;  // mpq_out_str returns non-zero on success
        } else {
            // Small number
            int64_t num = obj->data.number.value.small_num.num;
            int64_t den = obj->data.number.value.small_num.den;
            if (den == 1) {
                return fprintf(stream, "%" PRId64, num) >= 0;
            } else {
                return fprintf(stream, "%" PRId64 "/%" PRId64, num, den) >= 0;
            }
        }
    }
    case SL_TYPE_PAIR:
        return sl_write_pair_stream_recursive(obj, stream);
    case SL_TYPE_FUNCTION:
        if (obj->data.function.is_builtin) {
            return fprintf(stream, "#<builtin>") >= 0;
        } else {
            return fprintf(stream, "#<procedure>") >= 0;
        }
    case SL_TYPE_FREE:
        return fprintf(stream, "#<FREE_SLOT>") >= 0;
    default:
        return fprintf(stream, "#<UNKNOWN_TYPE>") >= 0;
    }
    return false;  // Should not be reached
}

static bool sl_write_pair_stream_recursive(sl_object *pair, FILE *stream) {
    if (fputc('(', stream) == EOF) return false;

    sl_object *current = pair;
    bool first = true;

    while (true) {
        if (!first) {
            if (fputc(' ', stream) == EOF) return false;
        }
        first = false;

        // Write car
        if (!sl_write_stream_recursive(sl_car(current), stream)) return false;

        // Check cdr
        sl_object *next = sl_cdr(current);
        if (sl_is_nil(next)) {
            // End of proper list
            break;
        } else if (sl_is_pair(next)) {
            // Continue list
            current = next;
        } else {
            // Dotted pair
            if (fprintf(stream, " . ") < 0) return false;
            if (!sl_write_stream_recursive(next, stream)) return false;
            break;
        }
    }

    return fputc(')', stream) != EOF;
}

// --- Debug Printing Implementation ---

// Helper to print indentation
static void print_indent(FILE *stream, int indent) {
    for (int i = 0; i < indent; ++i) {
        fprintf(stream, "  ");
    }
}

void sl_debug_print_object(sl_object *obj, FILE *stream, int indent) {
    print_indent(stream, indent);

    if (!obj) {
        fprintf(stream, "(null object)\n");
        return;
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
        fprintf(stream, "[NIL] ()\n");
        break;
    case SL_TYPE_BOOLEAN:
        fprintf(stream, "[BOOLEAN] %s\n", obj->data.boolean ? "#t" : "#f");
        break;
    case SL_TYPE_SYMBOL:
        // Use the accessor macro sl_symbol_name(obj)
        fprintf(stream, "[SYMBOL] '%s'\n", sl_symbol_name(obj) ? sl_symbol_name(obj) : "(null!)");  // Corrected x2
        break;
    case SL_TYPE_STRING:
        // Use the accessor macro sl_string_value(obj)
        fprintf(stream, "[STRING] \"%s\"\n", sl_string_value(obj) ? sl_string_value(obj) : "(null!)");  // Corrected x2
        break;
    case SL_TYPE_NUMBER:
        if (obj->data.number.is_bignum) {
            char *gmp_str = mpq_get_str(NULL, 10, obj->data.number.value.big_num);
            fprintf(stream, "[NUMBER:BIG] %s\n", gmp_str ? gmp_str : "(gmp error)");
            // Free the string allocated by mpq_get_str
            void (*freefunc)(void *, size_t);
            mp_get_memory_functions(NULL, NULL, &freefunc);
            if (freefunc && gmp_str) {
                freefunc(gmp_str, strlen(gmp_str) + 1);
            } else if (gmp_str) {
                free(gmp_str);
            }
        } else {
            fprintf(stream, "[NUMBER:SMALL] %" PRId64 "/%" PRId64 "\n",
                    obj->data.number.value.small_num.num,
                    obj->data.number.value.small_num.den);
        }
        break;
    case SL_TYPE_PAIR:
        fprintf(stream, "[PAIR]\n");
        // Recursively print car and cdr
        print_indent(stream, indent + 1);
        fprintf(stream, "CAR:\n");
        sl_debug_print_object(sl_car(obj), stream, indent + 2);

        print_indent(stream, indent + 1);
        fprintf(stream, "CDR:\n");
        sl_debug_print_object(sl_cdr(obj), stream, indent + 2);
        break;
    case SL_TYPE_FUNCTION:
        if (obj->data.function.is_builtin) {
            fprintf(stream, "[FUNCTION:BUILTIN] %s\n",
                    obj->data.function.def.builtin.name ? obj->data.function.def.builtin.name : "<unknown>");
        } else {
            fprintf(stream, "[FUNCTION:CLOSURE]\n");
            // Optionally print params/body structure if needed later
            print_indent(stream, indent + 1);
            fprintf(stream, "PARAMS:\n");
            sl_debug_print_object(sl_closure_params(obj), stream, indent + 2);
            print_indent(stream, indent + 1);
            fprintf(stream, "BODY:\n");
            sl_debug_print_object(sl_closure_body(obj), stream, indent + 2);
            // Avoid printing env directly unless you have a representation for it
            print_indent(stream, indent + 1);
            fprintf(stream, "ENV: #<environment>\n");
        }
        break;
    case SL_TYPE_FREE:
        fprintf(stream, "[FREE SLOT]\n");
        break;
    default:
        fprintf(stream, "[UNKNOWN TYPE: %d]\n", obj->type);
        break;
    }
}

// --- Minimal String Representation for Errors ---

// Helper to write object to string buffer (minimal representation)
void sl_object_to_string_buf(sl_object *obj, char *buffer, size_t size) {
    if (size == 0) return;  // No space

    if (!obj) {
        strncpy(buffer, "#<NULL>", size - 1);
        buffer[size - 1] = '\0';
        return;
    }
    switch (obj->type) {
    case SL_TYPE_NIL:
        strncpy(buffer, "()", size - 1);
        break;
    case SL_TYPE_BOOLEAN:
        strncpy(buffer, obj->data.boolean ? "#t" : "#f", size - 1);
        break;
    case SL_TYPE_SYMBOL:
        snprintf(buffer, size, "%s", sl_symbol_name(obj));
        break;
    case SL_TYPE_NUMBER:
        snprintf(buffer, size, "#<number>");
        break;  // Simple placeholder
    case SL_TYPE_STRING:
        // Use the accessor macro sl_string_value(obj)
        snprintf(buffer, size, "\"%s\"", sl_string_value(obj) ? sl_string_value(obj) : "");  // Corrected x2
        break;                                                                               // Basic
    case SL_TYPE_PAIR:
        snprintf(buffer, size, "(...)");
        break;  // Simple placeholder
    case SL_TYPE_FUNCTION:
        if (obj->data.function.is_builtin) {
            snprintf(buffer, size, "#<builtin:%s>", obj->data.function.def.builtin.name ? obj->data.function.def.builtin.name : "???");
        } else {
            snprintf(buffer, size, "#<closure>");
        }
        break;
    case SL_TYPE_ENV:
        snprintf(buffer, size, "#<environment>");
        break;
    case SL_TYPE_ERROR:
        // Use the accessor macro sl_error_message(obj)
        snprintf(buffer, size, "#<error:%s>", sl_error_message(obj) ? sl_error_message(obj) : "");  // Corrected x2
        break;
    case SL_TYPE_FREE:
        strncpy(buffer, "#<FREE>", size - 1);
        break;
    default:
        snprintf(buffer, size, "#<type:%d>", obj->type);
        break;
    }
    buffer[size - 1] = '\0';  // Ensure null termination
}
