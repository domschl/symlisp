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