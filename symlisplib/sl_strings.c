#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>
#include <stdlib.h>  // For malloc/free if needed later

#include "sl_strings.h"
#include "sl_core.h"
#include "sl_parse.h"
#include "sl_builtins.h"
#include "sl_unicode_case.h"

// --- String Functions ---

/**
 * @brief Builtin function (string char ...)
 * Creates a new string whose characters are the arguments.
 * @param args A list containing zero or more character arguments.
 * @return A new string object, or an error object.
 */
sl_object *sl_builtin_string(sl_object *args) {
    // Use realloc approach for building the string buffer
    char *buffer = NULL;
    size_t capacity = 0;
    size_t length = 0;
    sl_object *current = args;
    sl_gc_add_root(&current);  // Protect argument list iterator

    while (sl_is_pair(current)) {
        sl_object *char_obj = sl_car(current);
        if (!sl_is_char(char_obj)) {
            free(buffer);
            sl_gc_remove_root(&current);
            return sl_make_errorf("string: Expected a character argument, got %s", sl_type_name(char_obj ? char_obj->type : -1));
        }

        uint32_t cp = char_obj->data.code_point;
        char utf8_bytes[5];
        size_t bytes_written = encode_utf8(cp, utf8_bytes);

        // Ensure buffer has enough space (+1 for null terminator)
        if (length + bytes_written + 1 > capacity) {
            size_t new_capacity = capacity == 0 ? 16 : capacity * 2;
            if (new_capacity < length + bytes_written + 1) {
                new_capacity = length + bytes_written + 1;
            }
            // Use standard realloc
            char *new_buffer = realloc(buffer, new_capacity);
            if (!new_buffer) {
                free(buffer);
                sl_gc_remove_root(&current);
                return SL_OUT_OF_MEMORY_ERROR;
            }
            buffer = new_buffer;
            capacity = new_capacity;
        }

        // Append encoded bytes
        memcpy(buffer + length, utf8_bytes, bytes_written);
        length += bytes_written;

        current = sl_cdr(current);
    }

    // Check if args was a proper list (ends in NIL)
    if (!sl_is_nil(current)) {
        free(buffer);
        sl_gc_remove_root(&current);
        return sl_make_errorf("string: Internal error - improper argument list");
    }

    sl_gc_remove_root(&current);

    // Finalize buffer
    if (!buffer) {  // Handle zero arguments case
        buffer = malloc(1);
        if (!buffer) return SL_OUT_OF_MEMORY_ERROR;
        capacity = 1;
    } else if (length + 1 > capacity) {
        char *new_buffer = realloc(buffer, length + 1);
        if (!new_buffer) {
            free(buffer);
            return SL_OUT_OF_MEMORY_ERROR;
        }
        buffer = new_buffer;
    }
    buffer[length] = '\0';

    // Create string object
    sl_object *result = sl_make_string(buffer);
    free(buffer);

    return result;
}

// (string-length string) -> integer (number of Unicode code points)
static sl_object *sl_builtin_string_length(sl_object *args) {
    sl_object *arity_check = check_arity("string-length", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    if (!sl_is_string(str_obj)) {
        return sl_make_errorf("Error (string-length): Expected a string, got %s.", sl_type_name(str_obj->type));
    }

    const char *utf8_str = sl_string_value(str_obj);
    if (!utf8_str) {  // Should not happen for valid string objects
        return sl_make_errorf("Error (string-length): String object has NULL value.");
    }

    size_t code_point_count = 0;
    const char *ptr = utf8_str;

    while (*ptr != '\0') {
        uint32_t code_point = decode_utf8(&ptr);     // decode_utf8 advances ptr
        if (code_point == 0 && *ptr == '\0') break;  // Normal EOF
        // We count even replacement characters as characters for length
        code_point_count++;
        if (code_point == UTF8_REPLACEMENT_CHAR && *ptr == '\0') break;  // EOF after error byte
    }

    // Return the count as a SymLisp number
    // For now, assume length fits in int64_t
    // TODO: Handle potential overflow into bignum if lengths can exceed INT64_MAX
    return sl_make_number_si((int64_t)code_point_count, 1);
}

// (string-ref string k) -> character at index k (0-based)
static sl_object *sl_builtin_string_ref(sl_object *args) {
    sl_object *arity_check = check_arity("string-ref", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    sl_object *k_obj = sl_cadr(args);

    if (!sl_is_string(str_obj)) {
        return sl_make_errorf("Error (string-ref): Expected a string as the first argument, got %s.", sl_type_name(str_obj->type));
    }
    if (!sl_is_number(k_obj) || !sl_number_is_integer(k_obj)) {
        return sl_make_errorf("Error (string-ref): Expected an exact integer index as the second argument, got %s.", sl_type_name(k_obj->type));
    }

    const char *utf8_str = sl_string_value(str_obj);
    if (!utf8_str) {
        return sl_make_errorf("Error (string-ref): String object has NULL value.");
    }

    // Get the index k
    int64_t k;
    if (!get_number_as_int64(k_obj, &k, "string-ref")) {
        // Error message already generated by get_number_as_int64
        return SL_NIL;  // Indicate error
    }

    if (k < 0) {
        return sl_make_errorf("Error (string-ref): Index %" PRId64 " must be non-negative.", k);
    }

    // Iterate k code points into the string
    const char *ptr = utf8_str;
    uint32_t target_code_point = UTF8_REPLACEMENT_CHAR;  // Default if index is out of bounds
    bool found = false;
    int64_t current_index = 0;

    while (*ptr != '\0') {
        const char *start_of_char = ptr;
        uint32_t code_point = decode_utf8(&ptr);  // Advances ptr

        if (code_point == 0 && *ptr == '\0') break;  // Normal EOF

        if (current_index == k) {
            target_code_point = code_point;  // Could be UTF8_REPLACEMENT_CHAR if decode failed
            found = true;
            break;
        }

        current_index++;
        if (code_point == UTF8_REPLACEMENT_CHAR && *ptr == '\0') break;  // EOF after error byte
    }

    if (!found) {
        // Index k is out of bounds
        // Calculate actual length for a better error message
        size_t len = 0;
        const char *len_ptr = utf8_str;
        while (*len_ptr != '\0') {
            decode_utf8(&len_ptr);
            len++;
            if (*len_ptr == '\0' && len_ptr > utf8_str && *(len_ptr - 1) == '\0') break;  // Avoid infinite loop on decode error at end
        }
        return sl_make_errorf("Error (string-ref): Index %" PRId64 " out of bounds for string of length %zu.", k, len);
    }

    // Return the character object
    return sl_make_char(target_code_point);
}

// (string-append str1 str2 ...) -> string
static sl_object *sl_builtin_string_append(sl_object *args) {
    sl_gc_add_root(&args);
    size_t total_byte_len = 0;
    sl_object *current_arg = args;

    // First pass: Calculate total byte length and check types
    while (sl_is_pair(current_arg)) {
        sl_object *str = sl_car(current_arg);
        if (!sl_is_string(str)) {
            sl_gc_remove_root(&args);
            return sl_make_errorf("string-append: Expected a string, got %s", sl_type_name(str ? str->type : -1));
        }
        const char *str_val = str->data.string_val;
        if (str_val) {
            total_byte_len += strlen(str_val);
        }
        current_arg = sl_cdr(current_arg);
    }
    if (!sl_is_nil(current_arg)) {
        sl_gc_remove_root(&args);
        return sl_make_errorf("string-append: Internal error - improper argument list");
    }

    // Allocate buffer using standard malloc
    char *buffer = malloc(total_byte_len + 1);  // <<< USE malloc
    if (!buffer) {
        sl_gc_remove_root(&args);
        return SL_OUT_OF_MEMORY_ERROR;
    }
    buffer[0] = '\0';

    // Second pass: Concatenate strings
    current_arg = args;
    char *current_pos = buffer;
    while (sl_is_pair(current_arg)) {
        sl_object *str = sl_car(current_arg);
        const char *str_val = str->data.string_val;
        if (str_val) {
            size_t len = strlen(str_val);
            memcpy(current_pos, str_val, len);
            current_pos += len;
        }
        current_arg = sl_cdr(current_arg);
    }
    *current_pos = '\0';

    sl_gc_remove_root(&args);

    // Create the string object. sl_make_string should copy the buffer.
    sl_object *result = sl_make_string(buffer);

    free(buffer);  // <<< USE free

    return result;
}

// Helper to get byte offset for character index (needed for substring)
// Returns true on success, false if k is out of bounds or invalid UTF-8 found
static bool get_byte_offset_for_char_index(const char *str, size_t k, size_t *byte_offset) {
    const char *ptr = str;
    size_t current_char_index = 0;
    *byte_offset = 0;  // Initialize

    while (*ptr != '\0') {
        if (current_char_index == k) {
            *byte_offset = (size_t)(ptr - str);
            return true;  // Found the starting byte of the k-th character
        }
        const char *start_ptr = ptr;
        uint32_t cp = decode_utf8(&ptr);                         // Advances ptr
        if (cp == 0 && *ptr == '\0' && ptr == start_ptr) break;  // End of string
        if (cp == UTF8_REPLACEMENT_CHAR && ptr == start_ptr + 1) {
            // Decode error, but still count as one char position if ptr advanced
        } else if (ptr == start_ptr) {
            // Should not happen with valid decode_utf8 unless at end
            break;
        }
        current_char_index++;
    }

    // If loop finishes, check if k is exactly the length of the string
    if (current_char_index == k) {
        *byte_offset = (size_t)(ptr - str);  // Offset is end of string
        return true;
    }

    return false;  // k was out of bounds
}

// (substring str start [end]) -> string
static sl_object *sl_builtin_substring(sl_object *args) {
    // Use the newly added check_arity_range
    sl_object *arity_check = check_arity_range("substring", args, 2, 3);  // <<< USE check_arity_range
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    sl_object *start_obj = sl_cadr(args);
    // Use the newly added sl_cddr
    sl_object *end_obj = sl_is_pair(sl_cddr(args)) ? sl_car(sl_cddr(args)) : NULL;  // <<< USE sl_cddr

    if (!sl_is_string(str_obj)) return sl_make_errorf("substring: Expected string as first argument, got %s", sl_type_name(str_obj ? str_obj->type : -1));
    // Use the existing sl_number_is_integer predicate
    if (!sl_is_number(start_obj) || !sl_number_is_integer(start_obj)) return sl_make_errorf("substring: Expected integer as start index, got %s", sl_type_name(start_obj ? start_obj->type : -1));     // <<< USE sl_number_is_integer
    if (end_obj && (!sl_is_number(end_obj) || !sl_number_is_integer(end_obj))) return sl_make_errorf("substring: Expected integer as end index, got %s", sl_type_name(end_obj ? end_obj->type : -1));  // <<< USE sl_number_is_integer

    int64_t k_start;
    if (!get_number_as_int64(start_obj, &k_start, "substring")) return SL_NIL;

    int64_t k_end = -1;
    if (end_obj) {
        if (!get_number_as_int64(end_obj, &k_end, "substring")) return SL_NIL;
    }

    if (k_start < 0) return sl_make_errorf("substring: Start index %" PRId64 " must be non-negative", k_start);
    if (end_obj && k_end < k_start) return sl_make_errorf("substring: End index %" PRId64 " cannot be less than start index %" PRId64, k_end, k_start);

    const char *input_str = str_obj->data.string_val;
    if (!input_str) input_str = "";

    size_t start_byte_offset = 0;
    size_t end_byte_offset = 0;

    // Find start byte offset
    if (!get_byte_offset_for_char_index(input_str, (size_t)k_start, &start_byte_offset)) {
        // Calculate actual length for error message
        size_t len = 0;  // <<< Define and calculate len here
        const char *len_ptr = input_str;
        while (*len_ptr != '\0') {
            const char *cp_start = len_ptr;
            uint32_t cp = decode_utf8(&len_ptr);
            if (cp == 0 && *len_ptr == '\0' && len_ptr == cp_start) break;
            len++;
            if (cp == UTF8_REPLACEMENT_CHAR && *len_ptr == '\0') break;
        }
        return sl_make_errorf("substring: Start index %" PRId64 " is out of bounds for string of length %zu", k_start, len);
    }

    // Find end byte offset
    if (end_obj) {
        if (!get_byte_offset_for_char_index(input_str, (size_t)k_end, &end_byte_offset)) {
            // Calculate actual length for error message
            size_t len = 0;  // <<< Define and calculate len here too
            const char *len_ptr = input_str;
            while (*len_ptr != '\0') {
                const char *cp_start = len_ptr;
                uint32_t cp = decode_utf8(&len_ptr);
                if (cp == 0 && *len_ptr == '\0' && len_ptr == cp_start) break;
                len++;
                if (cp == UTF8_REPLACEMENT_CHAR && *len_ptr == '\0') break;
            }
            return sl_make_errorf("substring: End index %" PRId64 " is out of bounds for string of length %zu", k_end, len);
        }
    } else {
        end_byte_offset = strlen(input_str);
    }

    // Allocate buffer for the substring using standard malloc
    size_t sub_len = end_byte_offset - start_byte_offset;
    char *buffer = malloc(sub_len + 1);  // <<< USE malloc
    if (!buffer) {
        return SL_OUT_OF_MEMORY_ERROR;
    }

    // Copy the bytes
    memcpy(buffer, input_str + start_byte_offset, sub_len);
    buffer[sub_len] = '\0';

    // Create the string object
    sl_object *result = sl_make_string(buffer);
    free(buffer);  // <<< USE free

    return result;
}

// (string->list str) -> list-of-chars
// (This function does not use sl_string_buffer and should be okay as is)
static sl_object *sl_builtin_string_to_list(sl_object *args) {
    sl_object *arity_check = check_arity("string->list", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    if (!sl_is_string(str_obj)) return sl_make_errorf("string->list: Expected a string, got %s", sl_type_name(str_obj ? str_obj->type : -1));

    const char *input_str = str_obj->data.string_val;
    if (!input_str) input_str = "";

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    const char *ptr = input_str;

    sl_gc_add_root(&head);  // Protect the list being built

    while (*ptr != '\0') {
        const char *start_ptr = ptr;
        uint32_t cp = decode_utf8(&ptr);                         // Advances ptr
        if (cp == 0 && *ptr == '\0' && ptr == start_ptr) break;  // End of string

        // Handle decode error? R7RS says string->list raises error on invalid encoding.
        if (cp == UTF8_REPLACEMENT_CHAR && ptr == start_ptr + 1) {
            sl_gc_remove_root(&head);
            return sl_make_errorf("string->list: Invalid UTF-8 sequence in string");
        }

        sl_object *char_obj = sl_make_char(cp);
        if (char_obj == SL_OUT_OF_MEMORY_ERROR) {
            sl_gc_remove_root(&head);
            return SL_OUT_OF_MEMORY_ERROR;
        }
        sl_gc_add_root(&char_obj);  // Protect new char

        sl_object *new_pair = sl_make_pair(char_obj, SL_NIL);
        sl_gc_remove_root(&char_obj);  // Unroot char, now part of pair

        if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
            sl_gc_remove_root(&head);
            return SL_OUT_OF_MEMORY_ERROR;
        }

        *tail_ptr = new_pair;
        tail_ptr = &new_pair->data.pair.cdr;
    }

    sl_gc_remove_root(&head);
    return head;
}

// (list->string list-of-chars) -> string
static sl_object *sl_builtin_list_to_string(sl_object *args) {
    sl_object *arity_check = check_arity("list->string", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *list_obj = sl_car(args);
    if (!sl_is_list(list_obj)) {
        return sl_make_errorf("list->string: Expected a proper list, got %s", sl_type_name(list_obj ? list_obj->type : -1));
    }

    // Use realloc approach for building the string buffer
    char *buffer = NULL;
    size_t capacity = 0;
    size_t length = 0;
    sl_object *current = list_obj;
    sl_gc_add_root(&current);

    while (sl_is_pair(current)) {
        sl_object *char_obj = sl_car(current);
        if (!sl_is_char(char_obj)) {
            free(buffer);  // <<< USE free
            sl_gc_remove_root(&current);
            return sl_make_errorf("list->string: List element is not a character: %s", sl_type_name(char_obj ? char_obj->type : -1));
        }

        uint32_t cp = char_obj->data.code_point;
        char utf8_bytes[5];
        size_t bytes_written = encode_utf8(cp, utf8_bytes);

        // Ensure buffer has enough space (+1 for null terminator)
        if (length + bytes_written + 1 > capacity) {
            size_t new_capacity = capacity == 0 ? 16 : capacity * 2;
            if (new_capacity < length + bytes_written + 1) {
                new_capacity = length + bytes_written + 1;
            }
            // Use standard realloc
            char *new_buffer = realloc(buffer, new_capacity);  // <<< USE realloc
            if (!new_buffer) {
                free(buffer);  // <<< USE free
                sl_gc_remove_root(&current);
                return SL_OUT_OF_MEMORY_ERROR;
            }
            buffer = new_buffer;
            capacity = new_capacity;
        }

        // Append encoded bytes
        memcpy(buffer + length, utf8_bytes, bytes_written);
        length += bytes_written;

        current = sl_cdr(current);
    }

    // Check if list was proper
    if (!sl_is_nil(current)) {
        free(buffer);  // <<< USE free
        sl_gc_remove_root(&current);
        return sl_make_errorf("list->string: Expected a proper list, but encountered non-nil cdr");
    }

    sl_gc_remove_root(&current);

    // Finalize buffer
    if (!buffer) {           // Handle empty list case
        buffer = malloc(1);  // <<< USE malloc
        if (!buffer) return SL_OUT_OF_MEMORY_ERROR;
        capacity = 1;
    } else if (length + 1 > capacity) {
        char *new_buffer = realloc(buffer, length + 1);  // <<< USE realloc
        if (!new_buffer) {
            free(buffer);  // <<< USE free
            return SL_OUT_OF_MEMORY_ERROR;
        }
        buffer = new_buffer;
    }
    buffer[length] = '\0';

    // Create string object
    sl_object *result = sl_make_string(buffer);
    free(buffer);  // <<< USE free

    return result;
}

// --- SRFI-13 Inspired Functions ---

// (string-join list-of-strings delimiter) -> string
static sl_object *sl_builtin_string_join(sl_object *args) {
    sl_object *arity_check = check_arity("string-join", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *list_obj = sl_car(args);
    sl_object *delim_obj = sl_cadr(args);

    if (!sl_is_list(list_obj)) return sl_make_errorf("string-join: Expected a proper list as first argument, got %s", sl_type_name(list_obj ? list_obj->type : -1));
    if (!sl_is_string(delim_obj)) return sl_make_errorf("string-join: Expected a string delimiter as second argument, got %s", sl_type_name(delim_obj ? delim_obj->type : -1));

    const char *delimiter = delim_obj->data.string_val;
    if (!delimiter) delimiter = "";  // Handle null string_val
    size_t delim_len = strlen(delimiter);

    // Use realloc approach
    char *buffer = NULL;
    size_t capacity = 0;
    size_t length = 0;
    sl_object *current = list_obj;
    bool first_item = true;

    sl_gc_add_root(&current);  // Protect list iterator

    while (sl_is_pair(current)) {
        sl_object *str_obj = sl_car(current);
        if (!sl_is_string(str_obj)) {
            free(buffer);
            sl_gc_remove_root(&current);
            return sl_make_errorf("string-join: List element is not a string: %s", sl_type_name(str_obj ? str_obj->type : -1));
        }

        const char *str_val = str_obj->data.string_val;
        if (!str_val) str_val = "";  // Handle null string_val
        size_t str_len = strlen(str_val);
        size_t needed = length + str_len + (first_item ? 0 : delim_len) + 1;  // +1 for null

        // Ensure buffer has enough space
        if (needed > capacity) {
            size_t new_capacity = capacity == 0 ? 16 : capacity * 2;
            if (new_capacity < needed) new_capacity = needed;
            char *new_buffer = realloc(buffer, new_capacity);
            if (!new_buffer) {
                free(buffer);
                sl_gc_remove_root(&current);
                return SL_OUT_OF_MEMORY_ERROR;
            }
            buffer = new_buffer;
            capacity = new_capacity;
        }

        // Append delimiter (if not first item)
        if (!first_item && delim_len > 0) {
            memcpy(buffer + length, delimiter, delim_len);
            length += delim_len;
        }

        // Append string
        memcpy(buffer + length, str_val, str_len);
        length += str_len;

        first_item = false;
        current = sl_cdr(current);
    }

    // Check if list was proper
    if (!sl_is_nil(current)) {
        free(buffer);
        sl_gc_remove_root(&current);
        return sl_make_errorf("string-join: Expected a proper list, but encountered non-nil cdr");
    }

    sl_gc_remove_root(&current);

    // Finalize buffer
    if (!buffer) {  // Handle empty list case
        buffer = malloc(1);
        if (!buffer) return SL_OUT_OF_MEMORY_ERROR;
        capacity = 1;
    } else if (length + 1 > capacity) {
        char *new_buffer = realloc(buffer, length + 1);
        if (!new_buffer) {
            free(buffer);
            return SL_OUT_OF_MEMORY_ERROR;
        }
        buffer = new_buffer;
    }
    buffer[length] = '\0';

    // Create string object
    sl_object *result = sl_make_string(buffer);
    free(buffer);

    return result;
}

// Helper for string-split and string-tokenize: Creates a substring object
// Assumes start and end point to valid memory within a larger C string.
// Creates a *copy* for the new Scheme string.
static sl_object *make_substring_obj(const char *start, size_t length) {
    char *sub_buffer = malloc(length + 1);
    if (!sub_buffer) return SL_OUT_OF_MEMORY_ERROR;
    memcpy(sub_buffer, start, length);
    sub_buffer[length] = '\0';
    sl_object *sub_obj = sl_make_string(sub_buffer);
    free(sub_buffer);  // sl_make_string copied it
    return sub_obj;
}

// (string-split str delimiter-char) -> list-of-strings
// Simple version: splits by a single character. Treats consecutive delimiters
// as separating empty strings.
static sl_object *sl_builtin_string_split(sl_object *args) {
    sl_object *arity_check = check_arity("string-split", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    sl_object *delim_char_obj = sl_cadr(args);

    if (!sl_is_string(str_obj)) return sl_make_errorf("string-split: Expected string as first argument, got %s", sl_type_name(str_obj ? str_obj->type : -1));
    if (!sl_is_char(delim_char_obj)) return sl_make_errorf("string-split: Expected char delimiter as second argument, got %s", sl_type_name(delim_char_obj ? delim_char_obj->type : -1));

    const char *input_str = str_obj->data.string_val;
    if (!input_str) input_str = "";
    uint32_t delim_cp = delim_char_obj->data.code_point;

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    const char *ptr = input_str;
    const char *segment_start = input_str;

    sl_gc_add_root(&head);  // Protect the list being built

    while (*ptr != '\0') {
        const char *char_start = ptr;
        uint32_t current_cp = decode_utf8(&ptr);  // Advances ptr

        if (current_cp == delim_cp) {
            // Found delimiter, create substring from segment_start to char_start
            size_t segment_len = (size_t)(char_start - segment_start);
            sl_object *sub_obj = make_substring_obj(segment_start, segment_len);
            if (sub_obj == SL_OUT_OF_MEMORY_ERROR) {
                sl_gc_remove_root(&head);
                return SL_OUT_OF_MEMORY_ERROR;
            }
            sl_gc_add_root(&sub_obj);  // Protect substring

            sl_object *new_pair = sl_make_pair(sub_obj, SL_NIL);
            sl_gc_remove_root(&sub_obj);  // Unroot substring

            if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
                sl_gc_remove_root(&head);
                return SL_OUT_OF_MEMORY_ERROR;
            }

            *tail_ptr = new_pair;
            tail_ptr = &new_pair->data.pair.cdr;

            segment_start = ptr;  // Start next segment after the delimiter
        }
        // Handle decode errors or end of string edge cases if necessary
        if (current_cp == 0 && *ptr == '\0' && ptr == char_start) break;  // Normal end
        if (current_cp == UTF8_REPLACEMENT_CHAR && *ptr == '\0') break;   // End after replacement
    }

    // Add the final segment (from last delimiter to end of string)
    size_t segment_len = (size_t)(ptr - segment_start);
    sl_object *sub_obj = make_substring_obj(segment_start, segment_len);
    if (sub_obj == SL_OUT_OF_MEMORY_ERROR) {
        sl_gc_remove_root(&head);
        return SL_OUT_OF_MEMORY_ERROR;
    }
    sl_gc_add_root(&sub_obj);

    sl_object *new_pair = sl_make_pair(sub_obj, SL_NIL);
    sl_gc_remove_root(&sub_obj);

    if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
        sl_gc_remove_root(&head);
        return SL_OUT_OF_MEMORY_ERROR;
    }

    *tail_ptr = new_pair;  // Append the last segment

    sl_gc_remove_root(&head);
    return head;
}

// Helper: Check if a codepoint exists in a delimiter string
static bool is_in_delimiter_set(uint32_t cp, const char *delim_set_str) {
    const char *ptr = delim_set_str;
    while (*ptr != '\0') {
        const char *start_ptr = ptr;
        uint32_t delim_cp = decode_utf8(&ptr);
        if (delim_cp == cp) return true;
        if (delim_cp == 0 && *ptr == '\0' && ptr == start_ptr) break;  // End of set
    }
    return false;
}

// (string-tokenize str delimiter-set-string) -> list-of-strings
// Simple version: splits by any character in the set. Treats consecutive
// delimiters as a single split point (no empty strings between them).
static sl_object *sl_builtin_string_tokenize(sl_object *args) {
    sl_object *arity_check = check_arity("string-tokenize", args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    sl_object *delim_set_obj = sl_cadr(args);

    if (!sl_is_string(str_obj)) return sl_make_errorf("string-tokenize: Expected string as first argument, got %s", sl_type_name(str_obj ? str_obj->type : -1));
    if (!sl_is_string(delim_set_obj)) return sl_make_errorf("string-tokenize: Expected delimiter set string as second argument, got %s", sl_type_name(delim_set_obj ? delim_set_obj->type : -1));

    const char *input_str = str_obj->data.string_val;
    if (!input_str) input_str = "";
    const char *delim_set = delim_set_obj->data.string_val;
    if (!delim_set) delim_set = "";

    sl_object *head = SL_NIL;
    sl_object **tail_ptr = &head;
    const char *ptr = input_str;
    const char *segment_start = NULL;  // Start of current token (initially null)

    sl_gc_add_root(&head);  // Protect the list being built

    while (*ptr != '\0') {
        const char *char_start = ptr;
        uint32_t current_cp = decode_utf8(&ptr);  // Advances ptr
        bool is_delimiter = is_in_delimiter_set(current_cp, delim_set);

        if (!is_delimiter && segment_start == NULL) {
            // Start of a new token
            segment_start = char_start;
        } else if (is_delimiter && segment_start != NULL) {
            // End of the current token
            size_t segment_len = (size_t)(char_start - segment_start);
            sl_object *sub_obj = make_substring_obj(segment_start, segment_len);
            if (sub_obj == SL_OUT_OF_MEMORY_ERROR) {
                sl_gc_remove_root(&head);
                return SL_OUT_OF_MEMORY_ERROR;
            }
            sl_gc_add_root(&sub_obj);

            sl_object *new_pair = sl_make_pair(sub_obj, SL_NIL);
            sl_gc_remove_root(&sub_obj);

            if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
                sl_gc_remove_root(&head);
                return SL_OUT_OF_MEMORY_ERROR;
            }

            *tail_ptr = new_pair;
            tail_ptr = &new_pair->data.pair.cdr;

            segment_start = NULL;  // Reset for next token
        }
        // Handle decode errors or end of string edge cases if necessary
        if (current_cp == 0 && *ptr == '\0' && ptr == char_start) break;  // Normal end
        if (current_cp == UTF8_REPLACEMENT_CHAR && *ptr == '\0') break;   // End after replacement
    }

    // Add the final token if we were in the middle of one
    if (segment_start != NULL) {
        size_t segment_len = (size_t)(ptr - segment_start);
        sl_object *sub_obj = make_substring_obj(segment_start, segment_len);
        if (sub_obj == SL_OUT_OF_MEMORY_ERROR) {
            sl_gc_remove_root(&head);
            return SL_OUT_OF_MEMORY_ERROR;
        }
        sl_gc_add_root(&sub_obj);

        sl_object *new_pair = sl_make_pair(sub_obj, SL_NIL);
        sl_gc_remove_root(&sub_obj);

        if (new_pair == SL_OUT_OF_MEMORY_ERROR) {
            sl_gc_remove_root(&head);
            return SL_OUT_OF_MEMORY_ERROR;
        }

        *tail_ptr = new_pair;
    }

    sl_gc_remove_root(&head);
    return head;
}

// --- Conversion Functions ---

// (number->string number [radix]) -> string
// Converts a number (rational) to its string representation.
// Radix currently MUST be 10 for rationals. Only integers support other radix.
static sl_object *sl_builtin_number_to_string(sl_object *args) {
    sl_object *arity_check = check_arity_range("number->string", args, 1, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *num_obj = sl_car(args);
    sl_object *radix_obj = sl_is_pair(sl_cdr(args)) ? sl_cadr(args) : NULL;
    int radix = 10;  // Default radix

    if (!sl_is_number(num_obj)) return sl_make_errorf("number->string: Expected number as first argument, got %s", sl_type_name(num_obj ? num_obj->type : -1));

    bool is_integer = sl_number_is_integer(num_obj);  // Check if it's an integer

    if (radix_obj) {
        if (!sl_is_number(radix_obj) || !sl_number_is_integer(radix_obj)) return sl_make_errorf("number->string: Expected integer radix (2-62), got %s", sl_type_name(radix_obj ? radix_obj->type : -1));
        int64_t r_val;
        if (!get_number_as_int64(radix_obj, &r_val, "number->string")) return SL_NIL;  // Error handled
        if (r_val < 2 || r_val > 62) return sl_make_errorf("number->string: Radix %" PRId64 " must be between 2 and 62", r_val);
        radix = (int)r_val;
        if (radix != 10 && !is_integer) {
            return sl_make_errorf("number->string: Radix other than 10 is only supported for integers.");
        }
    }

    char *buffer = NULL;

    if (is_integer && radix != 10) {
        // Handle integer with non-decimal radix using mpz
        mpz_t num_z;
        mpz_init(num_z);
        sl_number_get_z(num_obj, num_z);  // Extracts integer part

        // Add 1 for sign and 1 for null terminator.
        size_t needed_size = mpz_sizeinbase(num_z, radix) + 2;
        buffer = malloc(needed_size);
        if (!buffer) {
            mpz_clear(num_z);
            return SL_OUT_OF_MEMORY_ERROR;
        }
        mpz_get_str(buffer, radix, num_z);
        mpz_clear(num_z);
    } else {
        // Handle rational (or integer with radix 10) using mpq
        mpq_t num_q;
        // get_number_as_mpq initializes num_q
        if (!get_number_as_mpq(num_obj, num_q, "number->string")) {
            // Should not happen if sl_is_number passed
            return sl_make_errorf("number->string: Internal error getting rational value.");
        }

        // Use gmp_asprintf to format the rational (allocates buffer)
        // %Qd formats as num/den or just num if den is 1.
        if (gmp_asprintf(&buffer, "%Qd", num_q) < 0) {
            mpq_clear(num_q);
            return SL_OUT_OF_MEMORY_ERROR;  // Allocation or formatting failed
        }
        mpq_clear(num_q);
    }

    // Now buffer contains the formatted string (either from mpz_get_str or gmp_asprintf)
    sl_object *result = sl_make_string(buffer);

    // Free the buffer allocated by malloc or gmp_asprintf
    // NOTE: GMP's free function might be different. Check GMP docs.
    // Assuming standard free works for gmp_asprintf for now.
    free(buffer);

    return result;  // sl_make_string copied it
}

// (string->number string [radix]) -> number or #f
// Converts a string representation to a number.
// Supports integers (base 2-62), fractions N/D (base 10), decimals (base 10),
// and scientific notation (base 10).
// Returns #f if the string is not a valid representation in the given radix,
// or if there are non-whitespace characters after the number part.
static sl_object *sl_builtin_string_to_number(sl_object *args) {
    sl_object *arity_check = check_arity_range("string->number", args, 1, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    sl_object *radix_obj = sl_is_pair(sl_cdr(args)) ? sl_cadr(args) : NULL;
    int radix = 10;  // Default radix

    if (!sl_is_string(str_obj)) return sl_make_errorf("string->number: Expected string as first argument, got %s", sl_type_name(str_obj ? str_obj->type : -1));

    if (radix_obj) {
        if (!sl_is_number(radix_obj) || !sl_number_is_integer(radix_obj)) return sl_make_errorf("string->number: Expected integer radix (2-62), got %s", sl_type_name(radix_obj ? radix_obj->type : -1));
        int64_t r_val;
        if (!get_number_as_int64(radix_obj, &r_val, "string->number")) return SL_NIL;  // Error handled
        // R7RS allows bases 2 through 36. GMP allows 2-62 or -2 to -36. Let's stick to 2-36 for R7RS.
        // Or keep 2-62 if GMP supports it well via mpq_set_str for integers. Let's keep 2-62 for now.
        if (r_val < 2 || r_val > 62) return sl_make_errorf("string->number: Radix %" PRId64 " must be between 2 and 62", r_val);
        radix = (int)r_val;
    }

    const char *input_str_orig = str_obj->data.string_val;
    if (!input_str_orig) input_str_orig = "";

    const char *parse_start_ptr = input_str_orig;
    // Skip leading whitespace
    while (*parse_start_ptr && isspace((unsigned char)*parse_start_ptr)) {
        parse_start_ptr++;
    }

    // Handle empty string or string with only whitespace
    if (*parse_start_ptr == '\0') {
        return SL_FALSE;
    }

    mpq_t temp_q;
    mpq_init(temp_q);
    const char *end_ptr = NULL;
    sl_object *result = SL_FALSE;  // Default to failure

    // Call the parsing helper
    if (parse_rational_from_string(parse_start_ptr, radix, temp_q, &end_ptr)) {
        // Parsing succeeded up to end_ptr. Check for trailing characters.
        // Skip trailing whitespace
        while (*end_ptr && isspace((unsigned char)*end_ptr)) {
            end_ptr++;
        }
        // If we reached the end of the original string, it's a valid number string
        if (*end_ptr == '\0') {
            result = make_number_from_mpq(temp_q);  // Simplify if possible
        }
        // Otherwise, there were non-whitespace trailing characters, result remains SL_FALSE
    }
    // If parse_rational_from_string returned false, result remains SL_FALSE

    mpq_clear(temp_q);
    return result;
}

// --- Comparison Functions ---

// Helper for string comparisons
static sl_object *string_compare(sl_object *args, const char *func_name, int (*compare_func)(const char *, const char *)) {
    sl_object *arity_check = check_arity(func_name, args, 2);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str1_obj = sl_car(args);
    sl_object *str2_obj = sl_cadr(args);

    if (!sl_is_string(str1_obj)) return sl_make_errorf("%s: Expected string as first argument, got %s", func_name, sl_type_name(str1_obj ? str1_obj->type : -1));
    if (!sl_is_string(str2_obj)) return sl_make_errorf("%s: Expected string as second argument, got %s", func_name, sl_type_name(str2_obj ? str2_obj->type : -1));

    const char *s1 = str1_obj->data.string_val ? str1_obj->data.string_val : "";
    const char *s2 = str2_obj->data.string_val ? str2_obj->data.string_val : "";

    int cmp_result = compare_func(s1, s2);

    // Determine boolean result based on which function called this helper
    bool result_bool = false;
    if (strcmp(func_name, "string=?") == 0)
        result_bool = (cmp_result == 0);
    else if (strcmp(func_name, "string<?") == 0)
        result_bool = (cmp_result < 0);
    else if (strcmp(func_name, "string>?") == 0)
        result_bool = (cmp_result > 0);
    else if (strcmp(func_name, "string<=?") == 0)
        result_bool = (cmp_result <= 0);
    else if (strcmp(func_name, "string>=?") == 0)
        result_bool = (cmp_result >= 0);
    // Add case-insensitive versions later if needed

    return result_bool ? SL_TRUE : SL_FALSE;
}

// (string=? str1 str2) -> boolean
static sl_object *sl_builtin_string_eq(sl_object *args) {
    return string_compare(args, "string=?", strcmp);
}

// (string<? str1 str2) -> boolean
static sl_object *sl_builtin_string_lt(sl_object *args) {
    return string_compare(args, "string<?", strcmp);
}

// (string>? str1 str2) -> boolean
static sl_object *sl_builtin_string_gt(sl_object *args) {
    return string_compare(args, "string>?", strcmp);
}

// (string<=? str1 str2) -> boolean
static sl_object *sl_builtin_string_le(sl_object *args) {
    return string_compare(args, "string<=?", strcmp);
}

// (string>=? str1 str2) -> boolean
static sl_object *sl_builtin_string_ge(sl_object *args) {
    return string_compare(args, "string>=?", strcmp);
}

/**
 * @brief Builtin function (symbol->string symbol)
 * Converts a symbol to its string representation.
 * @param env The current environment (unused).
 * @param args A list containing the symbol argument.
 * @return A string object representing the symbol's name, or an error object.
 */
sl_object *sl_builtin_symbol_to_string(sl_object *args) {
    sl_object *arg1;
    sl_object *arity_check = check_arity("symbol->string", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    arg1 = sl_car(args);
    if (!sl_is_symbol(arg1)) {
        return sl_make_errorf("symbol->string: argument must be a symbol, got %s", sl_type_name(arg1->type));
    }

    // --- Conversion ---
    // The symbol's name is already a C string. Create a Scheme string from it.
    return sl_make_string(arg1->data.symbol_name);
}

/**
 * @brief Builtin function (string->symbol string)
 * Converts a string to the corresponding symbol.
 * @param env The current environment (unused).
 * @param args A list containing the string argument.
 * @return The symbol object corresponding to the string, or an error object.
 */
sl_object *sl_builtin_string_to_symbol(sl_object *args) {
    sl_object *arg1;

    sl_object *arity_check = check_arity("string->symbol", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    arg1 = sl_car(args);
    if (!sl_is_string(arg1)) {
        return sl_make_errorf("string->symbol: argument must be a string, got %s", sl_type_name(arg1->type));
    }

    // --- Conversion ---
    // sl_make_symbol handles interning.
    return sl_make_symbol(arg1->data.string_val);
}

/**
 * @brief Builtin function (expr->string expr)
 * Converts any Scheme object to its string representation.
 * Essentially calls sl_write_to_string.
 * @param args A list containing the expression object.
 * @return A string object representing the expression, or an error object.
 */
sl_object *sl_builtin_expr_to_string(sl_object *args) {
    sl_object *arity_check = check_arity("expr->string", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *expr_obj = sl_car(args);

    // sl_write_to_string handles the conversion
    char *str_repr = sl_write_to_string(expr_obj);
    if (!str_repr) {
        // sl_write_to_string likely failed due to memory allocation
        return SL_OUT_OF_MEMORY_ERROR;
    }

    // Create a Scheme string object from the result
    sl_object *result_str_obj = sl_make_string(str_repr);
    free(str_repr);  // sl_make_string copied it

    return result_str_obj;  // Can still be SL_OUT_OF_MEMORY_ERROR if sl_make_string fails
}

/**
 * @brief Builtin function (string->expr string)
 * Converts a string containing a single Scheme expression into that object.
 * Essentially calls sl_parse_string and checks for complete consumption.
 * @param args A list containing the string object.
 * @return The parsed Scheme object, or an error object if parsing fails
 *         or if the string contains more than one expression (ignoring trailing whitespace).
 */
sl_object *sl_builtin_string_to_expr(sl_object *args) {
    sl_object *arity_check = check_arity("string->expr", args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    if (!sl_is_string(str_obj)) {
        return sl_make_errorf("string->expr: argument must be a string, got %s", sl_type_name(str_obj->type));
    }

    const char *input_str = sl_string_value(str_obj);
    if (!input_str) input_str = "";  // Handle potential NULL

    const char *end_ptr = NULL;
    sl_object *parsed_obj = sl_parse_string(input_str, &end_ptr);

    // Check for parsing errors or OOM
    if (parsed_obj == SL_NIL && end_ptr == input_str) {
        // Check if the input was just whitespace/comments
        const char *check_ptr = input_str;
        skip_whitespace_and_comments(&check_ptr);
        if (*check_ptr == '\0') {
            return sl_make_errorf("string->expr: No expression found in input string.");
        } else {
            return sl_make_errorf("string->expr: Failed to parse expression from string.");
        }
    }
    if (parsed_obj == SL_OUT_OF_MEMORY_ERROR || sl_is_error(parsed_obj)) {
        return parsed_obj;  // Propagate OOM or specific parse error
    }
    if (parsed_obj == SL_NIL) {  // General parse failure indicated by NIL
        return sl_make_errorf("string->expr: Failed to parse expression from string.");
    }

    // Check if the entire string was consumed (ignoring trailing whitespace)
    const char *check_ptr = end_ptr;
    skip_whitespace_and_comments(&check_ptr);

    if (*check_ptr != '\0') {
        // There was leftover non-whitespace content after the first expression
        return sl_make_errorf("string->expr: String contains more than one expression or trailing characters: '%s'", check_ptr);
    }

    // Success!
    return parsed_obj;
}

// --- Helper for case conversion ---
static sl_object *string_case_convert(const char *name, sl_object *args, uint32_t (*converter)(uint32_t)) {
    sl_object *arity_check = check_arity(name, args, 1);
    if (arity_check != SL_TRUE) return arity_check;

    sl_object *str_obj = sl_car(args);
    if (!sl_is_string(str_obj)) {
        return sl_make_errorf("%s: argument must be a string, got %s", name, sl_type_name(str_obj->type));
    }

    const char *input_str = sl_string_value(str_obj);
    if (!input_str) input_str = "";  // Handle potential NULL

    sl_string_buffer sbuf;
    sbuf_init(&sbuf);
    const char *ptr = input_str;
    char utf8_output_buffer[5];  // Max 4 bytes for UTF-8 + null terminator

    while (*ptr != '\0') {
        const char *start_ptr = ptr;
        uint32_t cp = decode_utf8(&ptr);  // Advances ptr

        if (cp == UTF8_REPLACEMENT_CHAR && ptr == start_ptr + 1) {
            // Handle invalid UTF-8 byte - append replacement char or original byte?
            // Let's append the original byte for now.
            if (!sbuf_append_char(&sbuf, *start_ptr)) goto oom;
        } else if (cp == UTF8_REPLACEMENT_CHAR) {
            // Handle invalid multi-byte sequence - append replacement?
            if (!sbuf_append_str(&sbuf, "\xEF\xBF\xBD")) goto oom;  // UTF-8 for U+FFFD
        } else {
            // Valid code point decoded
            uint32_t converted_cp = converter(cp);
            int len = encode_utf8(converted_cp, utf8_output_buffer);
            if (len > 0) {
                if (!sbuf_append_bytes(&sbuf, utf8_output_buffer, len)) goto oom;
            } else {
                // Encoding error? Should not happen for valid code points.
                // Append replacement char
                if (!sbuf_append_str(&sbuf, "\xEF\xBF\xBD")) goto oom;
            }
        }
    }

    // Null-terminate the result buffer
    if (!sbuf_append_char(&sbuf, '\0')) goto oom;
    sbuf.length--;  // Don't include null in Scheme string length

    sl_object *result_str = sl_make_string(sbuf.buffer ? sbuf.buffer : "");
    sbuf_free(&sbuf);

    if (result_str == SL_OUT_OF_MEMORY_ERROR) {
        // sl_make_string failed
        return SL_OUT_OF_MEMORY_ERROR;
    }
    return result_str;

oom:
    sbuf_free(&sbuf);
    return SL_OUT_OF_MEMORY_ERROR;
}

/**
 * @brief Builtin function (string-upcase string)
 * Converts a string to uppercase using simple Unicode mappings.
 */
sl_object *sl_builtin_string_upcase(sl_object *args) {
    return string_case_convert("string-upcase", args, sl_unicode_to_upper);
}

/**
 * @brief Builtin function (string-downcase string)
 * Converts a string to lowercase using simple Unicode mappings.
 */
sl_object *sl_builtin_string_downcase(sl_object *args) {
    return string_case_convert("string-downcase", args, sl_unicode_to_lower);
}

// --- Initialization ---

void sl_strings_init(sl_object *global_env) {
    define_builtin(global_env, "string", sl_builtin_string);  // <<< ADDED
    define_builtin(global_env, "string-length", sl_builtin_string_length);
    define_builtin(global_env, "string-ref", sl_builtin_string_ref);
    define_builtin(global_env, "string-append", sl_builtin_string_append);      // <<< ADDED
    define_builtin(global_env, "substring", sl_builtin_substring);              // <<< ADDED
    define_builtin(global_env, "string->list", sl_builtin_string_to_list);      // <<< ADDED
    define_builtin(global_env, "list->string", sl_builtin_list_to_string);      // <<< ADDED
    define_builtin(global_env, "string-join", sl_builtin_string_join);          // <<< ADDED
    define_builtin(global_env, "string-split", sl_builtin_string_split);        // <<< ADDED
    define_builtin(global_env, "string-tokenize", sl_builtin_string_tokenize);  // <<< ADDED

    // Conversions
    define_builtin(global_env, "number->string", sl_builtin_number_to_string);  // <<< ADDED
    define_builtin(global_env, "string->number", sl_builtin_string_to_number);  // <<< ADDED
    define_builtin(global_env, "symbol->string", sl_builtin_symbol_to_string);
    define_builtin(global_env, "string->symbol", sl_builtin_string_to_symbol);
    define_builtin(global_env, "expr->string", sl_builtin_expr_to_string);      // <<< ADDED
    define_builtin(global_env, "string->expr", sl_builtin_string_to_expr);      // <<< ADDED
    define_builtin(global_env, "string-upcase", sl_builtin_string_upcase);      // <<< ADDED
    define_builtin(global_env, "string-downcase", sl_builtin_string_downcase);  // <<< ADDED

    // Comparisons
    define_builtin(global_env, "string=?", sl_builtin_string_eq);   // <<< ADDED
    define_builtin(global_env, "string<?", sl_builtin_string_lt);   // <<< ADDED
    define_builtin(global_env, "string>?", sl_builtin_string_gt);   // <<< ADDED
    define_builtin(global_env, "string<=?", sl_builtin_string_le);  // <<< ADDED
    define_builtin(global_env, "string>=?", sl_builtin_string_ge);  // <<< ADDED
}