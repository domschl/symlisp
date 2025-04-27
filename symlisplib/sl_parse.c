#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>  // For PRId64
#include <stdarg.h>    // For variadic functions if needed later
#include <gmp.h>       // For GMP types and functions

#include "sl_core.h"  // Make sure this is included
#include "sl_parse.h"

// --- Buffer Management for String Building ---

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

// --- Forward Declarations for Static Helper Functions ---

// Parsing helpers
static sl_object *parse_expression(const char **input);
static sl_object *parse_list(const char **input);
static sl_object *parse_atom(const char **input);
static sl_object *parse_string_literal(const char **input);  // Renamed to avoid conflict
static sl_object *parse_number(const char **input_ptr, const char *start, size_t len);
static sl_object *parse_symbol_or_bool(const char **input_ptr, const char *start, size_t len);
static void skip_whitespace_and_comments(const char **input);
static bool is_delimiter(char c);

// Writing helpers
static bool sl_write_recursive(sl_object *obj, sl_string_buffer *sbuf);
static bool sl_write_pair_recursive(sl_object *pair, sl_string_buffer *sbuf);
static bool sl_write_stream_recursive(sl_object *obj, FILE *stream);
static bool sl_write_pair_stream_recursive(sl_object *pair, FILE *stream);

// --- Public Parsing Functions ---

sl_object *sl_parse_string(const char *input, const char **end_ptr) {
    if (!input) {
        return SL_NIL;  // Or handle error appropriately
    }
    const char *current = input;
    sl_object *result = parse_expression(&current);
    if (end_ptr) {
        *end_ptr = current;  // Update end pointer for caller
    }
    return result;  // SL_NIL on failure
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
        return sbuf_append_str(sbuf, obj->data.symbol);
    case SL_TYPE_STRING: {
        if (!sbuf_append_char(sbuf, '"')) return false;
        char *str = obj->data.string;
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
            int64_t num = obj->data.number.value.small.num;
            int64_t den = obj->data.number.value.small.den;
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
        return fprintf(stream, "%s", obj->data.symbol) >= 0;
    case SL_TYPE_STRING: {
        if (fputc('"', stream) == EOF) return false;
        char *str = obj->data.string;
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
            int64_t num = obj->data.number.value.small.num;
            int64_t den = obj->data.number.value.small.den;
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
        fprintf(stream, "[SYMBOL] '%s'\n", obj->data.symbol ? obj->data.symbol : "(null!)");
        break;
    case SL_TYPE_STRING:
        // Print string with quotes, maybe limit length for very long strings
        fprintf(stream, "[STRING] \"%s\"\n", obj->data.string ? obj->data.string : "(null!)");
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
                    obj->data.number.value.small.num,
                    obj->data.number.value.small.den);
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
                    obj->data.function.def.builtin.name ? obj->data.function.def.builtin.name : "<???>");
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

// --- Parsing Helper Functions ---

static void skip_whitespace_and_comments(const char **input) {
    while (**input) {
        if (isspace(**input)) {
            (*input)++;
        } else if (**input == ';') {  // Scheme comment
            (*input)++;               // Skip ';'
            while (**input && **input != '\n') {
                (*input)++;  // Skip until newline
            }
            if (**input == '\n') {
                (*input)++;  // Skip newline itself
            }
        } else {
            break;  // Not whitespace or comment start
        }
    }
}

// Check if a character can delimit an atom
static bool is_delimiter(char c) {
    return isspace(c) || c == '(' || c == ')' || c == '"' || c == '\'' || c == ';' || c == '\0';
}

// Main recursive parsing function
static sl_object *parse_expression(const char **input) {
    skip_whitespace_and_comments(input);

    char current_char = **input;

    if (current_char == '\0') {
        // This is okay if called recursively at the end, but not initially.
        // sl_parse_string handles the initial empty string case.
        // fprintf(stderr, "Error: Unexpected end of input.\n");
        return SL_NIL;  // Indicate end or error depending on context
    } else if (current_char == '(') {
        (*input)++;  // Consume '('
        return parse_list(input);
    } else if (current_char == ')') {
        fprintf(stderr, "Error: Unexpected ')'.\n");
        return SL_NIL;
    } else if (current_char == '\'') {
        (*input)++;  // Consume '''
        sl_object *quoted_expr = parse_expression(input);
        if (quoted_expr == SL_NIL && **input != ')') {  // Check if NIL was due to error or end
            fprintf(stderr, "Error: Expression expected after quote.\n");
            return SL_NIL;  // Propagate error
        }
        // Build (quote <expr>)
        sl_object *quote_sym = sl_make_symbol("quote");
        // Error check make_symbol?
        return sl_make_pair(quote_sym, sl_make_pair(quoted_expr, SL_NIL));
    } else if (current_char == '"') {
        (*input)++;                          // Consume '"'
        return parse_string_literal(input);  // Use renamed function
    } else {
        return parse_atom(input);
    }
}

// Parses the contents of a list after '(' has been consumed
static sl_object *parse_list(const char **input) {
    skip_whitespace_and_comments(input);

    if (**input == ')') {
        (*input)++;     // Consume ')'
        return SL_NIL;  // Empty list
    }

    // Use sentinel head for easier list building
    sl_object *head = sl_make_pair(SL_NIL, SL_NIL);  // Dummy head
    sl_object *tail = head;

    while (true) {
        skip_whitespace_and_comments(input);
        char current_char = **input;

        if (current_char == ')') {
            (*input)++;  // Consume ')'
            break;       // End of list
        }

        if (current_char == '\0') {
            fprintf(stderr, "Error: Unterminated list.\n");
            // Need to potentially free partially built list if not using GC immediately
            return SL_NIL;
        }

        // Check for dotted pair syntax: . <expr> )
        if (current_char == '.' && is_delimiter((*input)[1])) {
            (*input)++;  // Consume '.'
            skip_whitespace_and_comments(input);
            sl_object *cdr_expr = parse_expression(input);
            if (cdr_expr == SL_NIL && **input != ')') {  // Check if NIL was due to error
                fprintf(stderr, "Error: Expression expected after dot in list.\n");
                return SL_NIL;  // Propagate error
            }

            skip_whitespace_and_comments(input);
            if (**input != ')') {
                fprintf(stderr, "Error: Expected ')' after dot in list.\n");
                return SL_NIL;
            }
            (*input)++;                  // Consume ')'
            sl_set_cdr(tail, cdr_expr);  // Set the cdr of the last *real* element
            return head->data.pair.cdr;  // Return the actual list start
        }

        // Regular list element
        sl_object *element = parse_expression(input);
        if (element == SL_NIL && **input != ')') {  // Check if NIL was due to error
            // Need to potentially free partially built list
            fprintf(stderr, "Error parsing list element.\n");
            return SL_NIL;  // Propagate error
        }

        sl_object *new_pair = sl_make_pair(element, SL_NIL);
        sl_set_cdr(tail, new_pair);
        tail = new_pair;
    }

    return head->data.pair.cdr;  // Skip dummy head
}

// Parses an atom (number, boolean, symbol)
static sl_object *parse_atom(const char **input) {
    const char *start = *input;
    while (**input && !is_delimiter(**input)) {
        (*input)++;
    }
    size_t len = *input - start;
    if (len == 0) {
        // This can happen if input is just "( )" - parse_expression returns NIL
        // which is correct. Only an error if not immediately followed by ')'.
        // Let the caller (parse_list/parse_expression) handle unexpected delimiters.
        // fprintf(stderr, "Error: Expected an atom, found delimiter '%c'.\n", **input);
        return SL_NIL;  // Indicate no atom found here
    }

    // Try parsing as number first
    // Pass a copy of start, parse_number shouldn't modify input pointer directly
    const char *temp_input = start;
    sl_object *num_obj = parse_number(&temp_input, start, len);
    if (num_obj) {
        // *input is already advanced by the loop above, just return the object
        return num_obj;
    }

    // If not a number, try boolean or symbol
    // Pass a copy of start, parse_symbol_or_bool shouldn't modify input pointer directly
    temp_input = start;
    return parse_symbol_or_bool(&temp_input, start, len);
}

// Parses a string literal after '"' has been consumed
static sl_object *parse_string_literal(const char **input) {  // Renamed
    const char *start = *input;
    // Use dynamic buffer instead of fixed size
    sl_string_buffer sbuf;
    sbuf_init(&sbuf);

    while (**input != '"') {
        if (**input == '\0') {
            fprintf(stderr, "Error: Unterminated string literal.\n");
            sbuf_free(&sbuf);
            return SL_NIL;
        }

        char char_to_add;
        if (**input == '\\') {  // Handle escape sequences
            (*input)++;
            switch (**input) {
            case 'n':
                char_to_add = '\n';
                break;
            case 't':
                char_to_add = '\t';
                break;
            case '\\':
                char_to_add = '\\';
                break;
            case '"':
                char_to_add = '"';
                break;
            case '\0':  // Unterminated escape at end of input
                fprintf(stderr, "Error: Unterminated escape sequence in string.\n");
                sbuf_free(&sbuf);
                return SL_NIL;
            default:  // Unknown escape sequence, just insert the char itself
                char_to_add = **input;
                break;
            }
        } else {
            char_to_add = **input;
        }

        if (!sbuf_append_char(&sbuf, char_to_add)) {
            // Error appending (likely memory allocation)
            sbuf_free(&sbuf);
            return SL_NIL;
        }

        (*input)++;
    }

    (*input)++;  // Consume closing '"'

    sl_object *result = sl_make_string(sbuf.buffer ? sbuf.buffer : "");  // Handle empty string case
    sbuf_free(&sbuf);                                                    // Free the temporary buffer
    return result;
}

// Tries to parse a token as a number (integer, rational, or float converted to rational)
static sl_object *parse_number(const char **input_ptr, const char *start, size_t len) {
    // input_ptr is not used here, only start and len
    char *buffer = (char *)malloc(len + 1);
    if (!buffer) return NULL;  // Allocation failure
    memcpy(buffer, start, len);
    buffer[len] = '\0';

    mpq_t value_q;
    mpq_init(value_q);
    int base = 10;  // Assume base 10 for now
    sl_object *result = NULL;

    // 1. Try parsing directly as an integer or rational (e.g., "123", "-45", "1/2", "-3/4")
    if (mpq_set_str(value_q, buffer, base) == 0) {
        // Successfully parsed by mpq_set_str
        mpq_canonicalize(value_q);           // Ensure lowest terms, positive denominator
        result = sl_make_number_q(value_q);  // Let sl_make_number_q handle smallnum check
        // Fall through to cleanup and return
    } else {
        // 2. mpq_set_str failed, try parsing as a float (e.g., "3.14", "-0.5", "1e3")
        mpf_t value_f;
        mpf_init(value_f);  // Initialize a multi-precision float

        if (mpf_set_str(value_f, buffer, base) == 0) {
            // Successfully parsed as a float by mpf_set_str
            mpq_set_f(value_q, value_f);  // Convert the float to a rational
            // mpq_set_f result is already canonical if the float is finite.
            // mpq_canonicalize(value_q); // Usually not needed after mpq_set_f

            result = sl_make_number_q(value_q);  // Create the number object
        }
        // else: mpf_set_str also failed, result remains NULL

        mpf_clear(value_f);  // Clean up the temporary float
    }

    // Cleanup and return
    mpq_clear(value_q);
    free(buffer);
    return result;  // Returns NULL if both mpq_set_str and mpf_set_str failed
}

// Parses an atom as a boolean or symbol
static sl_object *parse_symbol_or_bool(const char **input_ptr, const char *start, size_t len) {
    // input_ptr is not used here, only start and len
    char *buffer = (char *)malloc(len + 1);
    if (!buffer) return SL_NIL;  // Allocation failure
    memcpy(buffer, start, len);
    buffer[len] = '\0';

    sl_object *result = SL_NIL;
    if (len == 2 && strcmp(buffer, "#t") == 0) {
        result = SL_TRUE;
    } else if (len == 2 && strcmp(buffer, "#f") == 0) {
        result = SL_FALSE;
    } else {
        // Assume it's a symbol
        // TODO: Add validation for valid symbol characters?
        result = sl_make_symbol(buffer);
    }

    free(buffer);
    return result;
}