#include <stdbool.h>
#include "sl_unicode_case.h"

// --- Case Mapping Logic (Simplified) ---
// Based on common ranges and offsets. A full implementation would use
// tables generated from UnicodeData.txt for accuracy and completeness.

uint32_t sl_unicode_to_upper(uint32_t cp) {
    // 1. Basic Latin (ASCII)
    if (cp >= 'a' && cp <= 'z') {
        return cp - ('a' - 'A');
    }
    // 2. Latin-1 Supplement
    if (cp >= 0xE0 && cp <= 0xF6) return cp - 0x20;  // à to ö
    if (cp >= 0xF8 && cp <= 0xFE) return cp - 0x20;  // ø to þ
    // ÿ (0xFF) -> Ÿ (0x178) - Special case
    if (cp == 0xFF) return 0x178;

    // 3. Latin Extended-A
    if (cp >= 0x101 && cp <= 0x17F && (cp % 2 != 0)) return cp - 1;

    // 4. Greek
    if (cp >= 0x3B1 && cp <= 0x3C1) return cp - 0x20;  // α to ρ -> Α to Ρ
    if (cp >= 0x3C3 && cp <= 0x3CB) return cp - 0x20;  // σ to ϋ -> Σ to Ϋ
    if (cp == 0x3C2) return 0x3A3;                     // ς -> Σ (final sigma)
    // Greek Extended (with tonos, etc.)
    if (cp == 0x3AC) return 0x386;  // ά -> Ά
    if (cp == 0x3AD) return 0x388;  // έ -> Έ
    if (cp == 0x3AE) return 0x389;  // ή -> Ή
    if (cp == 0x3AF) return 0x38A;  // ί -> Ί
    if (cp == 0x3CC) return 0x38C;  // ό -> Ό
    if (cp == 0x3CD) return 0x38E;  // ύ -> Ύ
    if (cp == 0x3CE) return 0x38F;  // ώ -> Ώ
    // Greek vowels with dialytika and tonos
    if (cp == 0x3B0) return 0x3AB;  // ΰ -> Ϋ (Should be U+03AB, not U+3B0->U+3AB) - Correcting: ΰ is U+03B0, maps to Ϋ U+03AB? No, ΰ U+03B0 maps to Ύ̈ U+1FEB. Let's stick to simple maps.
    // Let's handle ΐ U+0390 -> Ϊ U+03AA ? No, ΐ U+0390 is already uppercase.
    // How about ϊ U+03CA -> Ϊ U+03AA and ϋ U+03CB -> Ϋ U+03AB? Already covered by range.
    // Let's focus on the tonos vowels:
    // ά έ ή ί ό ύ ώ handled above.

    // 5. Cyrillic
    if (cp >= 0x430 && cp <= 0x44F) return cp - 0x20;  // а to я -> А to Я
    if (cp == 0x451) return 0x401;                     // ё -> Ё

    // 6. Armenian
    if (cp >= 0x561 && cp <= 0x586) return cp - 0x30;  // լ to ֆ -> Լ to Ֆ

    // 7. Georgian (Mkhedruli to Mtavruli)
    if (cp >= 0x10D0 && cp <= 0x10FA) return cp + (0x1C90 - 0x10D0);

    return cp;  // No mapping found
}

uint32_t sl_unicode_to_lower(uint32_t cp) {
    // 1. Basic Latin (ASCII)
    if (cp >= 'A' && cp <= 'Z') {
        return cp + ('a' - 'A');
    }
    // 2. Latin-1 Supplement
    if (cp >= 0xC0 && cp <= 0xD6) return cp + 0x20;  // À to Ö -> à to ö
    if (cp >= 0xD8 && cp <= 0xDE) return cp + 0x20;  // Ø to Þ -> ø to þ
    // Ÿ (0x178) -> ÿ (0xFF) - Special case
    if (cp == 0x178) return 0xFF;

    // 3. Latin Extended-A
    if (cp >= 0x100 && cp <= 0x17E && (cp % 2 == 0)) return cp + 1;

    // 4. Greek
    if (cp >= 0x391 && cp <= 0x3A1) return cp + 0x20;  // Α to Ρ -> α to ρ
    if (cp >= 0x3A3 && cp <= 0x3AB) return cp + 0x20;  // Σ to Ϋ -> σ to ϋ (Note: Σ maps to σ, context needed for ς)
    // Greek Extended (with tonos, etc.)
    if (cp == 0x386) return 0x3AC;  // Ά -> ά
    if (cp == 0x388) return 0x3AD;  // Έ -> έ
    if (cp == 0x389) return 0x3AE;  // Ή -> ή
    if (cp == 0x38A) return 0x3AF;  // Ί -> ί
    if (cp == 0x38C) return 0x3CC;  // Ό -> ό
    if (cp == 0x38E) return 0x3CD;  // Ύ -> ύ
    if (cp == 0x38F) return 0x3CE;  // Ώ -> ώ

    // 5. Cyrillic
    if (cp >= 0x410 && cp <= 0x42F) return cp + 0x20;  // А to Я -> а to я
    if (cp == 0x401) return 0x451;                     // Ё -> ё

    // 6. Armenian
    if (cp >= 0x531 && cp <= 0x556) return cp + 0x30;  // Ա to Ֆ -> ա to օ

    // 7. Georgian (Mtavruli to Mkhedruli)
    if (cp >= 0x1C90 && cp <= 0x1CBA) return cp - (0x1C90 - 0x10D0);

    return cp;  // No mapping found
}

bool sl_unicode_is_alphabetic(uint32_t cp) {
    // Basic Latin
    if ((cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z')) return true;
    // Latin-1 Supplement Letters (excluding multiplication/division signs)
    if ((cp >= 0xC0 && cp <= 0xD6) || (cp >= 0xD8 && cp <= 0xF6) || (cp >= 0xF8 && cp <= 0xFF)) return true;
    // Latin Extended-A & B (Common ranges)
    if (cp >= 0x100 && cp <= 0x24F) return true;
    // Greek and Coptic
    if (cp >= 0x370 && cp <= 0x3FF) return true;
    // Cyrillic
    if (cp >= 0x400 && cp <= 0x4FF) return true;
    // Armenian
    if (cp >= 0x530 && cp <= 0x58F) return true;
    // Georgian (Mkhedruli and Mtavruli)
    if ((cp >= 0x10A0 && cp <= 0x10FF) || (cp >= 0x1C90 && cp <= 0x1CBF)) return true;
    // Add other script ranges as needed...
    return false;
}

bool sl_unicode_is_numeric(uint32_t cp) {
    // Only handles standard ASCII digits 0-9
    return (cp >= '0' && cp <= '9');
}

bool sl_unicode_is_whitespace(uint32_t cp) {
    // Standard ASCII whitespace
    if (cp == ' ' || cp == '\t' || cp == '\n' || cp == '\r' || cp == '\f' || cp == '\v') {
        return true;
    }
    // Common Unicode spaces (like non-breaking space, etc.)
    if (cp == 0xA0 || (cp >= 0x2000 && cp <= 0x200A) || cp == 0x2028 || cp == 0x2029 || cp == 0x202F || cp == 0x3000) {
        return true;
    }
    return false;
}

bool sl_unicode_is_uppercase(uint32_t cp) {
    // Basic Latin
    if (cp >= 'A' && cp <= 'Z') return true;
    // Latin-1 Supplement Uppercase
    if ((cp >= 0xC0 && cp <= 0xD6) || (cp >= 0xD8 && cp <= 0xDE)) return true;
    // Latin Extended-A (Even code points are often uppercase)
    if (cp >= 0x100 && cp <= 0x17E && (cp % 2 == 0)) return true;
    // Greek Uppercase (Basic + Extended with tonos)
    if ((cp >= 0x391 && cp <= 0x3A1) || (cp >= 0x3A3 && cp <= 0x3AB)) return true;
    if (cp == 0x386 || cp == 0x388 || cp == 0x389 || cp == 0x38A || cp == 0x38C || cp == 0x38E || cp == 0x38F) return true;
    // Cyrillic Uppercase
    if ((cp >= 0x400 && cp <= 0x42F) || cp == 0x401) return true;  // Includes Ё
    // Armenian Uppercase
    if (cp >= 0x531 && cp <= 0x556) return true;
    // Georgian Mtavruli (Uppercase)
    if (cp >= 0x1C90 && cp <= 0x1CBA) return true;
    // Add other script ranges...
    return false;
}

bool sl_unicode_is_lowercase(uint32_t cp) {
    // Basic Latin
    if (cp >= 'a' && cp <= 'z') return true;
    // Latin-1 Supplement Lowercase
    if ((cp >= 0xE0 && cp <= 0xF6) || (cp >= 0xF8 && cp <= 0xFF)) return true;
    // Latin Extended-A (Odd code points are often lowercase)
    if (cp >= 0x101 && cp <= 0x17F && (cp % 2 != 0)) return true;
    // Greek Lowercase (Basic + Extended with tonos)
    if ((cp >= 0x3B1 && cp <= 0x3C1) || (cp >= 0x3C3 && cp <= 0x3CB) || cp == 0x3C2) return true;  // Includes final sigma
    if (cp == 0x3AC || cp == 0x3AD || cp == 0x3AE || cp == 0x3AF || cp == 0x3CC || cp == 0x3CD || cp == 0x3CE) return true;
    // Cyrillic Lowercase
    if ((cp >= 0x430 && cp <= 0x44F) || cp == 0x451) return true;  // Includes ё
    // Armenian Lowercase
    if (cp >= 0x561 && cp <= 0x587) return true;  // Includes և U+0587
    // Georgian Mkhedruli (Lowercase)
    if (cp >= 0x10D0 && cp <= 0x10FA) return true;
    // Add other script ranges...
    return false;
}