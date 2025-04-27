#ifndef SL_PARSE_H
#define SL_PARSE_H

#include "sl_core.h"
#include <stdio.h>  // For FILE*

/**
 * @brief Parses a Scheme expression from a null-terminated string.
 *
 * Takes a string containing a single Scheme expression (like a number, symbol,
 * string, or list) and attempts to parse it into an sl_object structure.
 * Handles basic atoms, lists, strings, booleans, numbers (integers and rationals),
 * and the quote (') syntax.
 *
 * @param input A pointer to the null-terminated string containing the Scheme expression.
 * @param end_ptr If not NULL, this will be updated to point to the character in the
 *                input string immediately after the parsed expression. This allows
 *                parsing multiple expressions from a single string.
 * @return sl_object* A pointer to the newly created sl_object representing the parsed
 *                    expression, or SL_NIL if parsing fails (e.g., syntax error,
 *                    out of memory). More specific error handling might be added later.
 *                    The caller is responsible for managing the lifecycle of the
 *                    returned object via the GC.
 */
sl_object *sl_parse_string(const char *input, const char **end_ptr);

/**
 * @brief Parses a Scheme expression from a FILE stream.
 *
 * Reads a single Scheme expression from the given stream.
 *
 * @param stream The FILE stream to read from.
 * @return sl_object* A pointer to the parsed object, or SL_NIL on EOF or error.
 */
sl_object *sl_parse_stream(FILE *stream);

/**
 * @brief Converts an sl_object into its string representation (S-expression).
 *
 * Allocates a new string buffer containing the S-expression representation
 * of the given object. Handles different object types including lists,
 * numbers (small and big), strings (with escaping), symbols, booleans, and nil.
 *
 * @param obj The sl_object to serialize.
 * @return char* A pointer to a newly allocated null-terminated string containing
 *               the S-expression. The caller is responsible for freeing this
 *               string using free(). Returns NULL on allocation failure or if
 *               obj is NULL.
 */
char *sl_write_to_string(sl_object *obj);

/**
 * @brief Writes the string representation of an sl_object to a stream.
 *
 * @param obj The sl_object to write.
 * @param stream The FILE stream to write to.
 * @return int Non-negative on success, EOF on failure.
 */
int sl_write_to_stream(sl_object *obj, FILE *stream);

/**
 * @brief Prints a detailed, indented representation of an sl_object structure to a stream.
 * Useful for debugging the internal structure.
 *
 * @param obj The sl_object to print.
 * @param stream The FILE stream to write to (e.g., stdout, stderr).
 * @param indent The initial indentation level (usually 0).
 */
void sl_debug_print_object(sl_object *obj, FILE *stream, int indent);

/**
 * @brief Writes a minimal string representation of an object to a buffer.
 * Primarily intended for use in error messages where a full recursive write
 * might be too complex or undesirable. This is a simplified version.
 *
 * @param obj The object to represent.
 * @param buffer The character buffer to write into.
 * @param size The size of the buffer.
 */
void sl_object_to_string_buf(sl_object *obj, char *buffer, size_t size);

#endif  // SL_PARSE_H