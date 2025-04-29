#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
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

    // Try parsing as a number first
    sl_object *num_result = parse_number(input, start, len);  // Pass start and len
    if (num_result) {
        if (num_result == SL_OUT_OF_MEMORY_ERROR) return num_result;  // Propagate OOM
        *input = ptr;                                                 // Consume the atom characters if number parsing succeeded
        return num_result;
    }

    // If not a number, try parsing as a symbol or boolean
    sl_object *sym_result = parse_symbol_or_bool(input, start, len);  // Pass start and len
    if (sym_result) {
        if (sym_result == SL_OUT_OF_MEMORY_ERROR) return sym_result;  // Propagate OOM
        *input = ptr;                                                 // Consume the atom characters if symbol parsing succeeded
        return sym_result;
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

    while (true) {
        char current_char = *ptr;

        if (current_char == '"') {
            // End of string
            ptr++;  // Consume closing '"'
            break;
        }

        if (current_char == '\0') {
            fprintf(stderr, "Error: Unterminated string literal.\n");
            success = false;
            break;
        }

        if (current_char == '\\') {
            // Escape sequence
            ptr++;  // Consume '\'
            char escaped_char = *ptr;
            char char_to_append;
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
            // Add other escapes like \r if needed
            case '\0':  // Unterminated escape sequence
                fprintf(stderr, "Error: Unterminated escape sequence in string literal.\n");
                success = false;
                goto end_loop;  // Use goto to break out of nested structure cleanly
            default:
                // Invalid escape sequence, treat as literal backslash followed by char?
                // Or signal error? Let's signal error for now.
                fprintf(stderr, "Error: Invalid escape sequence '\\%c' in string literal.\n", escaped_char);
                success = false;
                goto end_loop;
            }
            if (!sbuf_append_char(&sbuf, char_to_append)) {
                success = false;  // Allocation error
                break;
            }
            ptr++;  // Consume the character after '\'
        } else {
            // Regular character
            if (!sbuf_append_char(&sbuf, current_char)) {
                success = false;  // Allocation error
                break;
            }
            ptr++;  // Consume the regular character
        }
    }

end_loop:  // Label for goto

    *input = ptr;  // Update the main input pointer

    if (success) {
        // Create the string object. sl_make_string copies the buffer.
        // Create the string object. sl_make_string copies the buffer.
        // If sbuf.buffer is NULL (empty string parsed), pass "" literal instead.
        const char *string_to_make = (sbuf.buffer != NULL) ? sbuf.buffer : "";  // <<< FIX
        sl_object *str_obj = sl_make_string(string_to_make);
        sbuf_free(&sbuf);  // Free the temporary buffer
        return str_obj;    // Returns SL_NIL on allocation failure within sl_make_string
    } else {
        sbuf_free(&sbuf);  // Free buffer even on failure
        return SL_NIL;
    }
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
