#ifndef SL_UNICODE_CASE_H
#define SL_UNICODE_CASE_H

#include <stdint.h>

/**
 * @brief Converts a Unicode code point to its simple uppercase equivalent.
 * Handles Latin, Greek, Cyrillic, Armenian, and Georgian scripts based on
 * common 1-to-1 mappings. Does not handle context or locale sensitivity.
 * @param cp The input code point.
 * @return The uppercase code point, or the original code point if no simple
 *         uppercase mapping exists in the supported scripts.
 */
uint32_t sl_unicode_to_upper(uint32_t cp);

/**
 * @brief Converts a Unicode code point to its simple lowercase equivalent.
 * Handles Latin, Greek, Cyrillic, Armenian, and Georgian scripts based on
 * common 1-to-1 mappings. Does not handle context or locale sensitivity.
 * @param cp The input code point.
 * @return The lowercase code point, or the original code point if no simple
 *         lowercase mapping exists in the supported scripts.
 */
uint32_t sl_unicode_to_lower(uint32_t cp);

#endif  // sl_UNICODE_CASE_H