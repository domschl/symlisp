#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>    // For va_list
#include <inttypes.h>  // For PRId64
#include <limits.h>
#include <gmp.h>

#include "sl_core.h"
#include "sl_env.h"  // Include necessary headers

// Define buffer size for error messages formatted by sl_make_errorf
#define ERROR_BUFFER_SIZE 256
#define DEFAULT_CHUNK_OBJECT_COUNT 8192  // Objects per chunk

// --- Memory Management Variables ---
// Structure to manage heap chunks
typedef struct heap_chunk {
    sl_object *objects;             // Pointer to the objects in this chunk
    size_t object_count;            // Number of objects in this chunk
    struct heap_chunk *next_chunk;  // Link to the next chunk
} heap_chunk;

static heap_chunk *first_chunk = NULL;  // Head of the chunk list
static size_t total_heap_objects = 0;   // Total objects across all chunks
static sl_object *free_list = NULL;
static size_t free_count = 0;
// No need for dynamic_heap flag anymore, it's always dynamic with chunks

// --- GC Root Set ---
// Simple dynamic array for roots
static sl_object ***gc_roots = NULL;
static size_t root_count = 0;
static size_t root_capacity = 0;

// --- Global Constants / Singletons ---
// Note: These objects themselves are not garbage collected, they live statically.
// Their string contents are also static.
sl_object *SL_TRUE = NULL;
sl_object *SL_FALSE = NULL;
sl_object *SL_NIL = NULL;
sl_object *SL_EOF_OBJECT = NULL;

sl_object sl_oom_error_obj = {SL_TYPE_ERROR, .data.error_str = "Out of Memory"};
sl_object *SL_OUT_OF_MEMORY_ERROR = &sl_oom_error_obj;

sl_object sl_parse_error_obj = {SL_TYPE_ERROR, .data.error_str = "Parse Error"};
sl_object *SL_PARSE_ERROR = &sl_parse_error_obj;

// --- Global Environment & Symbol Table (Definitions) ---
sl_object *sl_global_env = NULL;
sl_object *sl_symbol_table = NULL;  // Simple list for now

// --- Forward Declarations ---
static void sl_gc_mark(sl_object *root);
static void sl_gc_sweep();
static bool allocate_new_chunk(size_t objects_in_chunk);  // New function

// Helper function to check if a value fits in int64_t
// Note: GMP's mpz_fits_slong_p uses 'long', which might not be 64-bit.
// We need a check specifically for int64_t range if long != int64_t.
// For simplicity here, we assume long is sufficient or handle potential truncation.
// A more robust solution might involve mpz_export or string conversion.
bool fits_int64(const mpz_t val) {
#if LONG_MAX >= INT64_MAX && LONG_MIN <= INT64_MIN
    // If long is guaranteed to be at least 64 bits, mpz_fits_slong_p is sufficient
    return mpz_fits_slong_p(val);
#else
    // If long might be smaller than 64 bits (e.g., 32-bit systems),
    // mpz_fits_slong_p is too strict. We need a more direct check.
    // Check if the number of bits is within the range for a signed 64-bit integer.
    // mpz_sizeinbase(val, 2) gives the number of bits.
    // It must be less than 64 for positive numbers (0 to 2^63 - 1)
    // or exactly 64 for the most negative number (-2^63).
    size_t bits = mpz_sizeinbase(val, 2);
    if (bits < 64) {
        return true;  // Fits within 63 bits + sign bit
    } else if (bits == 64) {
        // Check if it's exactly the minimum int64_t value (-2^63)
        // We can do this by comparing it to a temporary mpz_t initialized to INT64_MIN
        static bool min_int64_initialized = false;
        static mpz_t min_int64_z;
        if (!min_int64_initialized) {
            mpz_init(min_int64_z);
            // Set mpz from int64_t requires intermediate string conversion or careful bit manipulation
            // Using string is easiest here.
            char min_int64_str[30];  // Sufficient buffer
            snprintf(min_int64_str, sizeof(min_int64_str), "%" PRId64, INT64_MIN);
            mpz_set_str(min_int64_z, min_int64_str, 10);
            min_int64_initialized = true;
            // TODO: Add cleanup for min_int64_z in sl_mem_shutdown or via atexit?
        }
        return mpz_cmp(val, min_int64_z) == 0;
    } else {
        // More than 64 bits, definitely doesn't fit.
        return false;
    }
#endif
}

// DEBUG: Function to find and print details of a specific symbol in the heap
void debug_find_symbol(const char *target_name, const char *label) {
    heap_chunk *chunk = first_chunk;
    bool found = false;
    while (chunk) {
        for (size_t i = 0; i < chunk->object_count; ++i) {
            sl_object *obj = &chunk->objects[i];
            // Check if it's a symbol, has a name, and the name matches
            if (obj->type == SL_TYPE_SYMBOL && obj->data.symbol_name != NULL) {
                // Use strncmp for safety in case name isn't null-terminated?
                // For now, assume strdup worked correctly initially.
                if (strcmp(obj->data.symbol_name, target_name) == 0) {
                    printf("[DEBUG SYMBOL CHECK - %s] Found '%s' Object: %p, Name Ptr: %p, Name Value: \"%s\"\n",
                           label, target_name, (void *)obj, (void *)obj->data.symbol_name, obj->data.symbol_name);
                    found = true;
                    // Don't return early, check all chunks in case of duplicates (shouldn't happen without interning)
                }
            }
        }
        chunk = chunk->next_chunk;
    }
    if (!found) {
        printf("[DEBUG SYMBOL CHECK - %s] '%s' symbol object NOT found in heap.\n", label, target_name);
    }
}

// Encodes a Unicode code point into a UTF-8 sequence in buffer.
// Returns the number of bytes written (1-4).
// Buffer should have space for at least 4 bytes + null terminator if needed.
int encode_utf8(uint32_t code_point, char *buffer) {
    if (code_point <= 0x7F) {  // 1-byte sequence (ASCII)
        buffer[0] = (char)code_point;
        return 1;
    } else if (code_point <= 0x7FF) {  // 2-byte sequence
        buffer[0] = (char)(0xC0 | (code_point >> 6));
        buffer[1] = (char)(0x80 | (code_point & 0x3F));
        return 2;
    } else if (code_point <= 0xFFFF) {  // 3-byte sequence
        // Check for surrogates (should not happen for valid code points)
        if (code_point >= 0xD800 && code_point <= 0xDFFF) return 0;  // Error
        buffer[0] = (char)(0xE0 | (code_point >> 12));
        buffer[1] = (char)(0x80 | ((code_point >> 6) & 0x3F));
        buffer[2] = (char)(0x80 | (code_point & 0x3F));
        return 3;
    } else if (code_point <= 0x10FFFF) {  // 4-byte sequence
        buffer[0] = (char)(0xF0 | (code_point >> 18));
        buffer[1] = (char)(0x80 | ((code_point >> 12) & 0x3F));
        buffer[2] = (char)(0x80 | ((code_point >> 6) & 0x3F));
        buffer[3] = (char)(0x80 | (code_point & 0x3F));
        return 4;
    } else {
        return 0;  // Invalid code point
    }
}

// Decodes a single UTF-8 character sequence starting at *ptr.
// Advances *ptr past the consumed bytes (1-4).
// Returns the decoded Unicode code point (uint32_t).
// Returns UTF8_REPLACEMENT_CHAR on decoding errors (invalid sequence, overlong, etc.)
// and advances *ptr by 1 byte in case of error.
uint32_t decode_utf8(const char **ptr) {
    const unsigned char *p = (const unsigned char *)*ptr;
    uint32_t code_point = 0;
    int bytes_consumed = 0;

    if (*p == '\0') {
        return 0;  // End of string, return 0 (or a specific EOF marker if needed)
    }

    // Determine sequence length and initial bits
    if (*p <= 0x7F) {  // 1-byte sequence (ASCII 0xxxxxxx)
        code_point = *p;
        bytes_consumed = 1;
    } else if ((*p & 0xE0) == 0xC0) {           // 2-byte sequence (110xxxxx 10xxxxxx)
        if ((p[1] & 0xC0) != 0x80) goto error;  // Check continuation byte
        code_point = ((uint32_t)(p[0] & 0x1F) << 6) |
                     ((uint32_t)(p[1] & 0x3F));
        bytes_consumed = 2;
        // Check for overlong encoding (should be >= 0x80)
        if (code_point < 0x80) goto error;
    } else if ((*p & 0xF0) == 0xE0) {                                    // 3-byte sequence (1110xxxx 10xxxxxx 10xxxxxx)
        if ((p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80) goto error;  // Check continuation bytes
        code_point = ((uint32_t)(p[0] & 0x0F) << 12) |
                     ((uint32_t)(p[1] & 0x3F) << 6) |
                     ((uint32_t)(p[2] & 0x3F));
        bytes_consumed = 3;
        // Check for overlong encoding (should be >= 0x800)
        if (code_point < 0x800) goto error;
        // Check for surrogates (U+D800 to U+DFFF)
        if (code_point >= 0xD800 && code_point <= 0xDFFF) goto error;
    } else if ((*p & 0xF8) == 0xF0) {                                                             // 4-byte sequence (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)
        if ((p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80 || (p[3] & 0xC0) != 0x80) goto error;  // Check continuation bytes
        code_point = ((uint32_t)(p[0] & 0x07) << 18) |
                     ((uint32_t)(p[1] & 0x3F) << 12) |
                     ((uint32_t)(p[2] & 0x3F) << 6) |
                     ((uint32_t)(p[3] & 0x3F));
        bytes_consumed = 4;
        // Check for overlong encoding (should be >= 0x10000)
        if (code_point < 0x10000) goto error;
        // Check if code point is within valid Unicode range (<= U+10FFFF)
        if (code_point > 0x10FFFF) goto error;
    } else {
        // Invalid starting byte (e.g., 10xxxxxx, 11111xxx)
        goto error;
    }

    // Check for incomplete sequence (EOF before all bytes read)
    // This requires knowing the string length or checking for null terminators within the sequence.
    // Let's assume null-terminated strings for now. Check if any byte within the sequence is '\0'.
    for (int i = 1; i < bytes_consumed; ++i) {
        if (p[i] == '\0') goto error;
    }

    // Success
    *ptr += bytes_consumed;
    return code_point;

error:
    // Invalid sequence, return replacement char and advance by 1 byte
    (*ptr)++;
    return UTF8_REPLACEMENT_CHAR;
}

// --- Memory Management Functions ---

// Allocate and initialize a new heap chunk
static bool allocate_new_chunk(size_t objects_in_chunk) {
    if (objects_in_chunk == 0) objects_in_chunk = DEFAULT_CHUNK_OBJECT_COUNT;

    printf("[DEBUG] Allocating new heap chunk with %zu objects...\n", objects_in_chunk);

    heap_chunk *new_chunk_node = (heap_chunk *)malloc(sizeof(heap_chunk));
    if (!new_chunk_node) {
        perror("[DEBUG] Failed to allocate heap chunk node");
        return false;
    }

    sl_object *new_objects = (sl_object *)malloc(objects_in_chunk * sizeof(sl_object));
    if (!new_objects) {
        perror("[DEBUG] Failed to allocate objects for new heap chunk");
        free(new_chunk_node);  // Clean up node allocation
        return false;
    }

    // Initialize objects in the new chunk and link them to the *front* of the global free list
    sl_object *new_chunk_free_head = NULL;
    for (size_t i = 0; i < objects_in_chunk; ++i) {
        new_objects[i].type = SL_TYPE_FREE;
        new_objects[i].marked = false;
        memset(&new_objects[i].data, 0, sizeof(new_objects[i].data));
        new_objects[i].next = new_chunk_free_head;  // Prepend to local list
        new_chunk_free_head = &new_objects[i];
    }

    // Find tail of new list segment
    if (new_chunk_free_head) {
        sl_object *tail = new_chunk_free_head;
        while (tail->next != NULL) {
            tail = tail->next;
        }
        // Link tail of new segment to head of old global free list
        tail->next = free_list;
        free_list = new_chunk_free_head;  // Global free list now starts with new chunk's list
    }
    // If new_chunk_free_head is NULL (0 objects?), free_list remains unchanged.

    // Update chunk list and counts
    new_chunk_node->objects = new_objects;
    new_chunk_node->object_count = objects_in_chunk;
    new_chunk_node->next_chunk = first_chunk;  // Prepend chunk node to global list
    first_chunk = new_chunk_node;

    total_heap_objects += objects_in_chunk;
    free_count += objects_in_chunk;

    printf("[DEBUG] New chunk allocated. Total objects: %zu. Free count: %zu\n", total_heap_objects, free_count);
    return true;
}

// Initialize memory. requested_heap_size is now the size of the *first* chunk (0 for default).
void sl_mem_init(size_t first_chunk_size) {
    if (first_chunk != NULL) {
        return;  // Already initialized
    }

    printf("Initializing SymLisp memory...\n");
    if (first_chunk_size == 0) first_chunk_size = DEFAULT_CHUNK_OBJECT_COUNT;

    if (!allocate_new_chunk(first_chunk_size)) {
        fprintf(stderr, "FATAL: Failed to allocate initial heap chunk.\n");
        exit(EXIT_FAILURE);
    }

    // --- Allocate Constants ---
    // NIL
    SL_NIL = (sl_object *)malloc(sizeof(sl_object));
    if (!SL_NIL) {
        perror("Failed to allocate NIL");
        exit(EXIT_FAILURE);
    }
    SL_NIL->type = SL_TYPE_NIL;
    SL_NIL->marked = false;  // Always reachable conceptually
    SL_NIL->next = NULL;
    memset(&SL_NIL->data, 0, sizeof(SL_NIL->data));

    // TRUE
    SL_TRUE = (sl_object *)malloc(sizeof(sl_object));
    if (!SL_TRUE) {
        perror("Failed to allocate TRUE");
        exit(EXIT_FAILURE);
    }
    SL_TRUE->type = SL_TYPE_BOOLEAN;
    SL_TRUE->marked = false;  // Always reachable
    SL_TRUE->next = NULL;
    SL_TRUE->data.boolean = true;

    // FALSE
    SL_FALSE = (sl_object *)malloc(sizeof(sl_object));
    if (!SL_FALSE) {
        perror("Failed to allocate FALSE");
        exit(EXIT_FAILURE);
    }
    SL_FALSE->type = SL_TYPE_BOOLEAN;
    SL_FALSE->marked = false;  // Always reachable
    SL_FALSE->next = NULL;
    SL_FALSE->data.boolean = false;

    // Initialize global symbol table (global env created in main)
    // sl_global_env = sl_env_create(SL_NIL); // <<< REMOVE THIS LINE
    sl_symbol_table = SL_NIL;

    // Initialize GC roots array
    root_capacity = 16;  // Initial capacity
    gc_roots = malloc(root_capacity * sizeof(sl_object **));
    if (!gc_roots) {
        perror("Failed to allocate GC roots array");
        // Cleanup?
        exit(EXIT_FAILURE);
    }
    root_count = 0;
    // Add permanent roots (global_env added in main after creation)
    sl_gc_add_root(&sl_symbol_table);
}

void sl_mem_shutdown() {
    printf("[DEBUG] Shutting down memory...\n");
    heap_chunk *current_chunk = first_chunk;
    while (current_chunk != NULL) {
        heap_chunk *next = current_chunk->next_chunk;

        // Free resources within objects of the current chunk
        for (size_t i = 0; i < current_chunk->object_count; ++i) {
            sl_object *obj = &current_chunk->objects[i];
            if (obj->type != SL_TYPE_FREE) {
                switch (obj->type) {
                case SL_TYPE_NUMBER:
                    if (obj->data.number.is_bignum) mpq_clear(obj->data.number.value.big_num);
                    break;
                case SL_TYPE_STRING:
                    free(obj->data.string_val);
                    break;
                case SL_TYPE_SYMBOL:
                    free(obj->data.symbol_name);
                    break;
                case SL_TYPE_ERROR:
                    if (obj != SL_OUT_OF_MEMORY_ERROR) free(obj->data.error_str);
                    break;
                default:
                    break;
                }
            }
        }
        // Free the object array for this chunk
        // printf("[DEBUG] Freeing objects for chunk %p\n", (void *)current_chunk);
        free(current_chunk->objects);
        // Free the chunk management node itself
        // printf("[DEBUG] Freeing chunk node %p\n", (void *)current_chunk);
        free(current_chunk);
        current_chunk = next;
    }

    first_chunk = NULL;
    total_heap_objects = 0;
    free_list = NULL;
    free_count = 0;

    // Free GC roots array
    free(gc_roots);
    gc_roots = NULL;
    root_count = 0;
    root_capacity = 0;

    // Free the constants allocated outside the heap
    free(SL_NIL);
    SL_NIL = NULL;
    free(SL_TRUE);
    SL_TRUE = NULL;
    free(SL_FALSE);
    SL_FALSE = NULL;

    // TODO: Cleanup GMP statics like min_int64_z if used in fits_int64
}

// Add/Remove GC Roots (Implementation)
void sl_gc_add_root(sl_object **root_ptr) {
    if (root_count >= root_capacity) {
        // Resize roots array
        size_t new_capacity = root_capacity == 0 ? 16 : root_capacity * 2;
        sl_object ***new_roots = realloc(gc_roots, new_capacity * sizeof(sl_object **));
        if (!new_roots) {
            fprintf(stderr, "Error: Failed to resize GC roots array\n");
            // Cannot add root, potential memory leak if object becomes unreachable otherwise
            return;
        }
        gc_roots = new_roots;
        root_capacity = new_capacity;
    }
    gc_roots[root_count++] = root_ptr;
}

void sl_gc_remove_root(sl_object **root_ptr) {
    for (size_t i = 0; i < root_count; ++i) {
        // Cast root_ptr to sl_object** for comparison (though types match)
        if (gc_roots[i] == root_ptr) {
            gc_roots[i] = gc_roots[root_count - 1];
            root_count--;
            return;
        }
    }
    // fprintf(stderr, "Warning: Attempted to remove non-existent GC root.\n");
}

// Simple allocation from the free list
sl_object *sl_allocate_object() {
    if (free_list == NULL) {
        /*
        printf("[DEBUG] free_list is NULL. Running GC...\n");  // DEBUG
        sl_env_dump(sl_global_env, "Before GC");               // <<< DUMP 1
        debug_find_symbol("set!", "Before GC");                // <<< ADD
        sl_gc();
        sl_env_dump(sl_global_env, "After GC");  // <<< DUMP 2
        debug_find_symbol("set!", "After GC");   // <<< ADD
        */
        if (free_list == NULL) {
            printf("[DEBUG] free_list still NULL after GC. Allocating new chunk...\n");  // DEBUG
            // Still no memory after GC. Allocate a new chunk.
            if (!allocate_new_chunk(DEFAULT_CHUNK_OBJECT_COUNT)) {
                fprintf(stderr, "Error: Out of memory! Failed to allocate new heap chunk.\n");
                // Even allocation failed, return the static error object
                return SL_OUT_OF_MEMORY_ERROR;  // <<< Return static error
            }
            // allocate_new_chunk should have populated free_list
            if (free_list == NULL) {
                fprintf(stderr, "[DEBUG] Internal Error: New chunk allocation reported success but free_list is still NULL.\n");
                return SL_OUT_OF_MEMORY_ERROR;  // <<< Return static error
            }
            printf("[DEBUG] New chunk allocated. Proceeding with allocation.\n");  // DEBUG
        } else {
            printf("[DEBUG] GC freed objects. free_list is now %p.\n", (void *)free_list);  // DEBUG
        }
    }

    // Allocate from free list
    sl_object *new_obj = free_list;
    free_list = new_obj->next;
    new_obj->next = NULL;
    new_obj->marked = false;
    memset(&new_obj->data, 0, sizeof(new_obj->data));
    free_count--;
    return new_obj;
}

// --- Object Constructors ---

sl_object *sl_make_number_si(int64_t num, int64_t den) {
    if (den == 0) {
        fprintf(stderr, "Error: Division by zero in number creation.\n");
        // Return NIL or signal error appropriately
        return SL_NIL;  // Or perhaps exit?
    }
    // Canonical form: denominator positive
    if (den < 0) {
        num = -num;
        den = -den;
    }

    sl_object *new_num = sl_allocate_object();
    CHECK_ALLOC(new_num);  // <<< ADD CHECK

    new_num->type = SL_TYPE_NUMBER;
    new_num->data.number.is_bignum = false;  // Assume small initially
    new_num->data.number.value.small_num.num = num;
    new_num->data.number.value.small_num.den = den;

    // Note: We don't automatically promote to bignum here.
    // Arithmetic operations will need to handle potential overflow
    // and create bignums as needed. This constructor just stores
    // the int64_t values directly.

    return new_num;
}

sl_object *sl_make_number_z(const mpz_t num_z) {
    sl_object *new_num = sl_allocate_object();
    CHECK_ALLOC(new_num);  // <<< ADD CHECK

    new_num->type = SL_TYPE_NUMBER;
    new_num->data.number.is_bignum = true;
    mpq_init(new_num->data.number.value.big_num);  // GMP alloc might fail, but less common to check explicitly here
    mpq_set_z(new_num->data.number.value.big_num, num_z);
    return new_num;
}

sl_object *sl_make_number_zz(const mpz_t num_z, const mpz_t den_z) {
    // Check for denominator being zero
    if (mpz_sgn(den_z) == 0) {
        fprintf(stderr, "Error: Division by zero in number creation (zz).\n");
        return SL_NIL;  // Or perhaps SL_OUT_OF_MEMORY_ERROR or a specific error object
    }

    mpq_t temp_q;
    mpq_init(temp_q);

    // Set the rational from the integers
    mpz_set(mpq_numref(temp_q), num_z);
    mpz_set(mpq_denref(temp_q), den_z);

    // Canonicalize the rational (handles signs and common factors)
    mpq_canonicalize(temp_q);

    // Use the existing helper to create the sl_object, simplifying if possible
    sl_object *result = make_number_from_mpq(temp_q);

    mpq_clear(temp_q);  // Clear the temporary rational

    CHECK_ALLOC(result);  // Check allocation result before returning
    return result;
}

sl_object *sl_make_number_q(const mpq_t value_q) {
    sl_object *new_num = sl_allocate_object();
    CHECK_ALLOC(new_num);  // <<< ADD CHECK

    new_num->type = SL_TYPE_NUMBER;

    // Check if the GMP rational fits into int64_t components
    mpz_t num_z, den_z;
    mpz_inits(num_z, den_z, NULL);
    mpq_get_num(num_z, value_q);
    mpq_get_den(den_z, value_q);  // Denominator is always positive after canonicalization

    if (fits_int64(num_z) && fits_int64(den_z)) {
        // It fits, store as smallnum
        new_num->data.number.is_bignum = false;
        new_num->data.number.value.small_num.num = mpz_get_si(num_z);  // Assuming long fits int64_t based on fits_int64 logic
        new_num->data.number.value.small_num.den = mpz_get_si(den_z);
    } else {
        // Doesn't fit, store as bignum
        new_num->data.number.is_bignum = true;
        mpq_init(new_num->data.number.value.big_num);
        mpq_set(new_num->data.number.value.big_num, value_q);
        // No need to canonicalize here, assuming input value_q is already canonical or mpq_set handles it.
        // If input might not be canonical, add: mpq_canonicalize(new_num->data.number.value.big_num);
    }

    mpz_clears(num_z, den_z, NULL);
    return new_num;
}

// Helper function to put an object back on the free list
static void return_object_to_free_list(sl_object *obj) {
    if (!obj || obj == SL_OUT_OF_MEMORY_ERROR) return;  // Safety checks
    // Assuming obj is valid and was just allocated but failed internally
    memset(&obj->data, 0, sizeof(obj->data));  // Clear data
    obj->type = SL_TYPE_FREE;
    obj->marked = false;
    obj->next = free_list;
    free_list = obj;
    free_count++;
}

sl_object *sl_make_string(const char *str) {
    sl_object *new_str = sl_allocate_object();
    CHECK_ALLOC(new_str);  // <<< ADD CHECK

    new_str->type = SL_TYPE_STRING;
    new_str->data.string_val = strdup(str);

    if (!new_str->data.string_val) {
        // strdup failed! Return the object to the free list.
        return_object_to_free_list(new_str);  // <<< Cleanup
        return SL_OUT_OF_MEMORY_ERROR;        // Signal error
    }
    return new_str;
}

// Create a character object
sl_object *sl_make_char(uint32_t code_point) {
    sl_object *new_char = sl_allocate_object();
    CHECK_ALLOC(new_char);
    new_char->type = SL_TYPE_CHAR;
    new_char->data.code_point = code_point;
    return new_char;
}

sl_object *sl_make_symbol(const char *name) {
    // --- Symbol Interning Logic ---
    sl_object *current_node = sl_symbol_table;
    sl_gc_add_root(&current_node);  // Root traversal pointer

    while (current_node != SL_NIL) {
        if (!sl_is_pair(current_node)) {
            // Should not happen if table is managed correctly
            fprintf(stderr, "Internal Error: Symbol table corrupted (non-pair node).\n");
            sl_gc_remove_root(&current_node);
            return SL_OUT_OF_MEMORY_ERROR;  // Or a specific error
        }
        sl_object *sym_obj = sl_car(current_node);
        if (sl_is_symbol(sym_obj) && sym_obj->data.symbol_name != NULL &&
            strcmp(sym_obj->data.symbol_name, name) == 0) {
            // Found existing symbol, return it
            sl_gc_remove_root(&current_node);
            return sym_obj;
        }
        current_node = sl_cdr(current_node);
    }
    sl_gc_remove_root(&current_node);  // Unroot traversal pointer
    // --- End Symbol Interning Search ---

    // Symbol not found, create a new one
    sl_object *new_sym = sl_allocate_object();
    CHECK_ALLOC(new_sym);  // Checks for SL_OUT_OF_MEMORY_ERROR

    sl_gc_add_root(&new_sym);  // Root the new symbol temporarily

    new_sym->type = SL_TYPE_SYMBOL;
    new_sym->data.symbol_name = strdup(name);
    if (!new_sym->data.symbol_name) {
        return_object_to_free_list(new_sym);  // Cleanup allocated object
        sl_gc_remove_root(&new_sym);
        return SL_OUT_OF_MEMORY_ERROR;
    }

    // Add the new symbol object to the global symbol table
    // The table stores pairs: (symbol_obj . next_node)
    sl_object *new_table_node = sl_make_pair(new_sym, sl_symbol_table);
    if (new_table_node == SL_OUT_OF_MEMORY_ERROR) {
        // Failed to make pair, cleanup the symbol object too
        free(new_sym->data.symbol_name);      // Free the string
        return_object_to_free_list(new_sym);  // Return the object
        sl_gc_remove_root(&new_sym);
        return SL_OUT_OF_MEMORY_ERROR;  // Propagate error
    }

    // Update global symbol table pointer *after* successful pair creation
    sl_symbol_table = new_table_node;

    sl_gc_remove_root(&new_sym);  // Unroot the symbol, it's now reachable via sl_symbol_table
    return new_sym;               // Return the newly created and interned symbol
}

sl_object *sl_make_pair(sl_object *car_obj, sl_object *cdr_obj) {
    sl_object *new_pair = sl_allocate_object();
    CHECK_ALLOC(new_pair);  // <<< ADD CHECK

    new_pair->type = SL_TYPE_PAIR;
    new_pair->data.pair.car = car_obj;
    new_pair->data.pair.cdr = cdr_obj;
    return new_pair;
}

sl_object *sl_make_closure(sl_object *params, sl_object *body, sl_object *env) {
    if (!sl_is_env(env)) {
        fprintf(stderr, "Internal Error: sl_make_closure requires a valid environment.\n");
        return SL_NIL;  // Or perhaps an internal error object?
    }
    // Basic validation of params (should be list of symbols or NIL) - can enhance later
    // Basic validation of body (should not be NULL) - can enhance later

    sl_object *func_obj = sl_allocate_object();
    CHECK_ALLOC(func_obj);  // <<< ADD CHECK

    func_obj->type = SL_TYPE_FUNCTION;
    func_obj->data.function.is_builtin = false;
    func_obj->data.function.def.closure.params = params;
    func_obj->data.function.def.closure.body = body;
    func_obj->data.function.def.closure.env = env;

    return func_obj;
}

sl_object *sl_make_builtin(const char *name, sl_object *(*func_ptr)(sl_object *args)) {
    sl_object *new_func = sl_allocate_object();
    CHECK_ALLOC(new_func);  // <<< ADD CHECK

    new_func->type = SL_TYPE_FUNCTION;
    new_func->data.function.is_builtin = true;
    // Note: 'name' is assumed to be a static string literal, not allocated.
    new_func->data.function.def.builtin.name = name;
    new_func->data.function.def.builtin.func_ptr = func_ptr;
    return new_func;
}

sl_object *sl_make_errorf(const char *format, ...) {
    sl_object *err_obj = sl_allocate_object();
    CHECK_ALLOC(err_obj);  // <<< ADD CHECK

    err_obj->type = SL_TYPE_ERROR;

    // Use a temporary buffer for formatting
    char temp_buffer[ERROR_BUFFER_SIZE];  // <<< Uses defined constant
    va_list args;
    va_start(args, format);
    vsnprintf(temp_buffer, sizeof(temp_buffer), format, args);
    va_end(args);
    temp_buffer[sizeof(temp_buffer) - 1] = '\0';

    err_obj->data.error_str = strdup(temp_buffer);
    if (!err_obj->data.error_str) {
        return_object_to_free_list(err_obj);  // <<< Cleanup
        return SL_OUT_OF_MEMORY_ERROR;
    }

    return err_obj;
}

// Helper to create a number object from mpq_t, simplifying if possible
sl_object *make_number_from_mpq(mpq_t val) {
    mpq_canonicalize(val);  // Ensure result is canonical

    mpz_t num_z, den_z;
    mpz_inits(num_z, den_z, NULL);
    mpq_get_num(num_z, val);
    mpq_get_den(den_z, val);

    sl_object *result = NULL;
    // Check if denominator is 1 and numerator fits int64_t
    if (mpz_cmp_si(den_z, 1) == 0 && fits_int64(num_z)) {
        result = sl_make_number_si(mpz_get_si(num_z), 1);
    }
    // Check if both numerator and denominator fit int64_t
    else if (fits_int64(num_z) && fits_int64(den_z)) {
        result = sl_make_number_si(mpz_get_si(num_z), mpz_get_si(den_z));
    } else {
        // Doesn't fit small int, use the bignum
        result = sl_make_number_q(val);  // This copies val
    }

    mpz_clears(num_z, den_z, NULL);
    CHECK_ALLOC(result);  // Check allocation result before returning
    return result;
}

// --- Number Predicates ---

bool sl_number_is_integer(sl_object *obj) {
    if (!sl_is_number(obj)) return false;
    if (obj->data.number.is_bignum) {
        // Check if denominator is 1
        return mpz_cmp_ui(mpq_denref(obj->data.number.value.big_num), 1) == 0;
    } else {
        // Smallnum: check if denominator is 1
        return obj->data.number.value.small_num.den == 1;
    }
}

bool sl_number_is_zero(sl_object *obj) {
    if (!sl_is_number(obj)) return false;
    if (obj->data.number.is_bignum) {
        return mpq_sgn(obj->data.number.value.big_num) == 0;
    } else {
        // Smallnum: check if numerator is 0 (denominator is always > 0)
        return obj->data.number.value.small_num.num == 0;
    }
}

// --- Number Accessors ---

// Gets the integer value as GMP integer (copies into rop).
// Returns false and sets rop to 0 if obj is not an integer number.
// 'rop' must be initialized by the caller (mpz_init).
bool sl_number_get_z(sl_object *obj, mpz_t rop) {  // <<< RENAMED from sl_number_get_num_z
    if (!sl_number_is_integer(obj)) {
        mpz_set_si(rop, 0);  // Set to 0 on failure
        return false;
    }
    if (obj->data.number.is_bignum) {
        // Denominator is 1, just get numerator
        mpz_set(rop, mpq_numref(obj->data.number.value.big_num));
    } else {
        // Smallnum, denominator is 1
        mpz_set_si(rop, obj->data.number.value.small_num.num);
    }
    return true;
}

// Gets the denominator as GMP integer (copies into rop).
// 'rop' must be initialized by the caller (mpz_init).
void sl_number_get_den_z(sl_object *obj, mpz_t rop) {
    if (!sl_is_number(obj)) {
        mpz_set_ui(rop, 1);  // Set to 1 if not a number? Or signal error?
        return;
    }
    if (obj->data.number.is_bignum) {
        mpz_set(rop, mpq_denref(obj->data.number.value.big_num));
    } else {
        mpz_set_si(rop, obj->data.number.value.small_num.den);
    }
}

// ... rest of the file ...
// --- Number Accessors ---

bool sl_number_get_si(sl_object *obj, int64_t *num_out, int64_t *den_out) {
    if (!sl_is_number(obj)) return false;  // Not a number

    if (obj->data.number.is_bignum) {
        // It's a bignum, check if it fits in int64_t
        mpz_t num_z, den_z;
        mpz_inits(num_z, den_z, NULL);
        mpq_get_num(num_z, obj->data.number.value.big_num);
        mpq_get_den(den_z, obj->data.number.value.big_num);

        bool fits = fits_int64(num_z) && fits_int64(den_z);
        if (fits) {
            *num_out = mpz_get_si(num_z);  // Use mpz_get_si (long) - check limits if needed
            *den_out = mpz_get_si(den_z);
        }
        mpz_clears(num_z, den_z, NULL);
        return fits;
    } else {
        // It's already a small number
        *num_out = obj->data.number.value.small_num.num;
        *den_out = obj->data.number.value.small_num.den;
        return true;
    }
}

void sl_number_get_q(sl_object *obj, mpq_t rop) {
    // Assumes rop is initialized by caller (mpq_init)
    if (!sl_is_number(obj)) {
        mpq_set_si(rop, 0, 1);  // Set to 0 if not a number? Or signal error?
        return;
    }

    if (obj->data.number.is_bignum) {
        mpq_set(rop, obj->data.number.value.big_num);
    } else {
        // Convert small number to mpq_t
        mpq_set_si(rop,
                   obj->data.number.value.small_num.num,
                   obj->data.number.value.small_num.den);
        mpq_canonicalize(rop);  // Ensure canonical form
    }
}

void sl_number_get_num_z(sl_object *obj, mpz_t rop) {
    // Assumes rop is initialized by caller (mpz_init)
    if (!sl_is_number(obj)) {
        mpz_set_si(rop, 0);  // Set to 0 if not a number? Or signal error?
        return;
    }
    if (obj->data.number.is_bignum) {
        mpq_get_num(rop, obj->data.number.value.big_num);
    } else {
        mpz_set_si(rop, obj->data.number.value.small_num.num);
    }
}

// --- Helpers ---
// Helper needed for set! (if not already defined in sl_core.c/h)
sl_object *sl_cadr(sl_object *list) {
    if (!sl_is_pair(list)) return SL_NIL;
    return sl_car(sl_cdr(list));
}

// Checks if obj is a proper list (NIL or pairs ending in NIL)
// Does NOT detect cycles.
bool sl_is_list(sl_object *obj) {
    sl_object *slow = obj;
    sl_object *fast = obj;

    while (true) {
        if (fast == SL_NIL) {
            return true;  // Reached end, proper list
        }
        if (!sl_is_pair(fast)) {
            return false;  // Ended in non-pair, non-NIL atom
        }
        fast = sl_cdr(fast);  // Move fast one step

        if (fast == SL_NIL) {
            return true;  // Reached end, proper list
        }
        if (!sl_is_pair(fast)) {
            return false;  // Ended in non-pair, non-NIL atom
        }
        fast = sl_cdr(fast);  // Move fast second step

        // Move slow one step
        slow = sl_cdr(slow);

        // Check for cycle
        if (fast == slow) {
            return false;  // Cycle detected
        }
    }
}

// --- Garbage Collection (Mark and Sweep) ---

// Mark phase: Recursively mark all reachable objects
static void sl_gc_mark(sl_object *root) {
    if (!root || root->marked) {
        return;
    }
    root->marked = true;

    switch (root->type) {
    case SL_TYPE_PAIR:
        sl_gc_mark(sl_car(root));
        sl_gc_mark(sl_cdr(root));
        break;
    case SL_TYPE_FUNCTION:
        if (!root->data.function.is_builtin) {
            // Mark closure components
            sl_gc_mark(root->data.function.def.closure.params);
            sl_gc_mark(root->data.function.def.closure.body);
            sl_gc_mark(root->data.function.def.closure.env);
        }
        // Builtin name is const char*, func_ptr is code - no marking needed
        break;
    case SL_TYPE_ENV:
        sl_gc_mark(root->data.env.bindings);
        sl_gc_mark(root->data.env.outer);
        break;
    case SL_TYPE_NUMBER:  // Numbers don't reference other sl_objects
    case SL_TYPE_STRING:  // String data is freed in sweep, no sl_object refs
    case SL_TYPE_CHAR:
    case SL_TYPE_BOOLEAN:  // Constants or simple value
    case SL_TYPE_SYMBOL:   // Symbol name is freed in sweep (if not interned), no sl_object refs
    case SL_TYPE_NIL:      // Constant
    case SL_TYPE_FREE:     // Should not be reachable if logic is correct
        break;
    default:
        // Should not happen
        fprintf(stderr, "Warning: Unknown object type %d during GC mark.\n", root->type);
        break;
    }
}

// Sweep phase: Collect all unmarked objects across ALL chunks
static void sl_gc_sweep() {
    // printf("[DEBUG] Starting GC Sweep...\n"); // DEBUG
    free_list = NULL;  // Rebuild the free list from scratch
    free_count = 0;

    heap_chunk *current_chunk = first_chunk;
    while (current_chunk != NULL) {
        // printf("[DEBUG] Sweeping chunk %p (%zu objects)\n", (void*)current_chunk, current_chunk->object_count); // DEBUG
        for (size_t i = 0; i < current_chunk->object_count; ++i) {
            sl_object *obj = &current_chunk->objects[i];

            if (obj == SL_OUT_OF_MEMORY_ERROR) continue;  // Skip static error obj

            // Skip objects already on the free list (e.g., from previous sweeps if GC interrupted?)
            // This check might be redundant if sweep always completes.
            if (obj->type == SL_TYPE_FREE) {
                // It's already free, ensure it gets onto the new list if somehow missed
                // This logic might need refinement depending on exact free list state.
                // For now, assume we rebuild fully. If it's FREE, it will be added below.
                // continue; // Let the logic below handle it.
            }

            if (obj->marked) {
                obj->marked = false;  // Keep marked object, unmark for next cycle
            } else {
                // --- Add Debugging ---
                // printf("[DEBUG GC SWEEP] Freeing object %p (type: %d)\n", (void *)obj, obj->type);
                if (obj->type == SL_TYPE_PAIR) {
                    // Be careful printing car/cdr if they might be invalid!
                    // Maybe just print their addresses?
                    // printf("    Pair CAR: %p, CDR: %p\n", (void *)obj->data.pair.car, (void *)obj->data.pair.cdr);
                }
                bool is_critical = false;
                if (obj->type == SL_TYPE_SYMBOL && obj->data.symbol_name != NULL) {
                    // Check for specific critical symbols being freed
                    if (strcmp(obj->data.symbol_name, "set!") == 0 ||
                        strcmp(obj->data.symbol_name, "define") == 0 ||
                        strcmp(obj->data.symbol_name, "lambda") == 0 ||
                        strcmp(obj->data.symbol_name, "assert-equal") == 0)  // Add others if needed
                    {
                        printf("[DEBUG GC SWEEP] *** WARNING: Freeing critical SYMBOL '%s' (%p) ***\n", obj->data.symbol_name, (void *)obj);
                        is_critical = true;
                    } else {
                        // Optional: Print non-critical symbols being freed
                        // printf("[DEBUG GC SWEEP] Freeing SYMBOL: %s (%p)\n", obj->data.symbol.name, (void*)obj);
                    }
                } else if (obj->type == SL_TYPE_PAIR) {
                    // Optional: Print pairs being freed
                    // printf("[DEBUG GC SWEEP] Freeing PAIR: (%p)\n", (void*)obj);
                } else if (obj->type == SL_TYPE_FUNCTION && obj->data.function.is_builtin) {
                    // Check if a builtin function object itself is being freed
                    if (strcmp(obj->data.function.def.builtin.name, "set!") == 0) {  // Check name stored in builtin
                        printf("[DEBUG GC SWEEP] *** WARNING: Freeing 'set!' BUILTIN function object (%p) ***\n", (void *)obj);
                        is_critical = true;
                    }
                }
                // --- End Debugging ---
                if (obj->type != SL_TYPE_FREE) {  // Only free resources if it wasn't already free
                    switch (obj->type) {
                    case SL_TYPE_NUMBER:
                        if (obj->data.number.is_bignum) mpq_clear(obj->data.number.value.big_num);
                        break;
                    case SL_TYPE_STRING:
                        free(obj->data.string_val);
                        break;
                    case SL_TYPE_SYMBOL:
                        // --- REMOVED: free(obj->data.symbol_name); ---
                        // Interned symbol names are managed by the symbol table
                        // and freed only during sl_mem_shutdown.
                        // free(obj->data.symbol_name);
                        break;
                    case SL_TYPE_ERROR:
                        if (obj != SL_OUT_OF_MEMORY_ERROR) free(obj->data.error_str);
                        break;
                    default:
                        break;  // Types without external resources
                    }
                    // Clear the object's data
                    memset(&obj->data, 0, sizeof(obj->data));
                    obj->type = SL_TYPE_FREE;
                }

                // Add to the rebuilt free list (prepend)
                obj->next = free_list;
                free_list = obj;
                free_count++;
                // Optional: Add breakpoint if a critical object was freed
                if (is_critical) {
                    fprintf(stderr, "[DEBUG GC SWEEP] Critical object freed...\n");
                }  // raise(SIGTRAP);  // Or{ __builtin_debugtrap(); } // Or raise(SIGTRAP);
            }
        }
        current_chunk = current_chunk->next_chunk;
    }
    // printf("[DEBUG] GC Sweep finished. New free count: %zu\n", free_count); // DEBUG
}

// Main GC function
void sl_gc() {
    // printf("Starting GC... Free count before: %zu\n", free_count);

    // 1. Mark all objects reachable from the root set
    for (size_t i = 0; i < root_count; ++i) {
        sl_object **root_ptr = (sl_object **)(gc_roots[i]);
        // Check if the root pointer itself is valid AND if the object it points to is valid
        if (root_ptr && *root_ptr) {
            sl_gc_mark(*root_ptr);
        }
    }

    // 2. Sweep unreachable objects
    sl_gc_sweep();

    // printf("GC finished. Free count after: %zu\n", free_count);
}

// --- String Conversion ---

// Dynamic sprintf-like function for creating strings
char *dynamic_sprintf(const char *format, ...) {
    va_list args;
    va_start(args, format);

    // Determine required size
    int size = vsnprintf(NULL, 0, format, args);
    va_end(args);

    if (size < 0) {
        return NULL;  // Error in formatting
    }

    // Allocate buffer (+1 for null terminator)
    char *buffer = (char *)malloc(size + 1);
    if (!buffer) {
        return NULL;  // Memory allocation failed
    }

    // Format the string into the buffer
    va_start(args, format);
    vsnprintf(buffer, size + 1, format, args);
    va_end(args);

    return buffer;
}

char *sl_object_to_string(sl_object *obj) {
    if (!obj) {
        return strdup("InternalError:NULL_Object");
    }

    switch (obj->type) {
    case SL_TYPE_NIL:
        return strdup("()");
    case SL_TYPE_BOOLEAN:
        return strdup(obj == SL_TRUE ? "#t" : "#f");
    case SL_TYPE_NUMBER:
        if (obj->data.number.is_bignum) {
            // Use gmp_asprintf correctly
            char *gmp_str = NULL;
            // gmp_asprintf allocates memory using GMP's allocator
            if (gmp_asprintf(&gmp_str, "%Qd", obj->data.number.value.big_num) < 0) {
                return NULL;  // Allocation or formatting failed
            }
            // Copy to standard malloc'd memory for consistency
            char *result = strdup(gmp_str);
            // Free the GMP-allocated string
            // Assuming GMP uses a compatible free function. If not, use GMP's free.
            free(gmp_str);
            return result;
        } else {
            // Small number (code as before)
            if (obj->data.number.value.small_num.den == 1) {
                return dynamic_sprintf("%lld", (long long)obj->data.number.value.small_num.num);
            } else {
                return dynamic_sprintf("%lld/%lld",
                                       (long long)obj->data.number.value.small_num.num,
                                       (long long)obj->data.number.value.small_num.den);
            }
        }
    case SL_TYPE_SYMBOL:
        return strdup(obj->data.symbol_name ? obj->data.symbol_name : "InvalidSymbol");
    case SL_TYPE_STRING: {
        const char *s = obj->data.string_val;
        if (!s) return strdup("\"\"");

        // Calculate required size first (consider escapes)
        size_t len = strlen(s);
        size_t needed = len + 2;  // Quotes
        for (size_t i = 0; i < len; ++i) {
            if (s[i] == '"' || s[i] == '\\') needed++;
        }
        char *buf = malloc(needed + 1);  // buf declared here
        if (!buf) return NULL;
        char *p = buf;
        *p++ = '"';
        for (size_t i = 0; i < len; ++i) {
            if (s[i] == '"' || s[i] == '\\') {
                *p++ = '\\';
            }
            *p++ = s[i];
        }
        *p++ = '"';
        *p = '\0';
        return buf;  // Correct: buf is valid here
    }
    case SL_TYPE_CHAR: {
        char utf8_buffer[5];  // Max 4 bytes + null terminator
        int bytes_written = encode_utf8(obj->data.code_point, utf8_buffer);
        if (bytes_written > 0) {
            utf8_buffer[bytes_written] = '\0';  // Null-terminate
            printf("%s", utf8_buffer);          // Print the UTF-8 sequence
        } else {
            // Handle invalid code point? Print placeholder?
            printf("<?>");
        }
        break;
    }
    case SL_TYPE_PAIR: {
        char *car_str = sl_object_to_string(sl_car(obj));
        char *cdr_str = sl_object_to_string(sl_cdr(obj));
        if (!car_str || !cdr_str) {
            free(car_str);
            free(cdr_str);
            return NULL;
        }
        char *result = dynamic_sprintf("(%s . %s)", car_str, cdr_str);  // result declared here
        free(car_str);
        free(cdr_str);
        return result;  // Correct: result is valid here
    }
    case SL_TYPE_FUNCTION:
        if (obj->data.function.is_builtin) {
            // Builtin: return name
            return dynamic_sprintf("#<builtin:%s>", obj->data.function.def.builtin.name ? obj->data.function.def.builtin.name : "unnamed");
        } else {
            // Closure: return generic representation
            return strdup("#<closure>");
        }
        // <<< Implicit break/return was missing here >>>
        // No break needed after return

    case SL_TYPE_ENV:
        return strdup("#<environment>");
    case SL_TYPE_ERROR:
        return dynamic_sprintf("Error: %s", obj->data.error_str ? obj->data.error_str : "Unknown Error");

    default:
        return dynamic_sprintf("#<unknown_type:%d>", obj->type);
    }
    // Should not be reachable if all cases return or break
    // return strdup("#<InternalError:UnhandledTypeInToString>");
}

// Helper to create a number object from mpz_t, simplifying if possible
sl_object *sl_make_number_from_mpz(mpz_t val) {
    // Check if it fits in int64_t
    if (fits_int64(val)) {
        // Use mpz_get_si (long) - assumes long fits int64_t based on fits_int64 logic
        return sl_make_number_si(mpz_get_si(val), 1);
    } else {
        // Doesn't fit small int, use bignum integer.
        // We can represent this as a rational with denominator 1 using mpq_t.
        sl_object *new_num = sl_allocate_object();
        CHECK_ALLOC(new_num);

        new_num->type = SL_TYPE_NUMBER;
        new_num->data.number.is_bignum = true;
        mpq_init(new_num->data.number.value.big_num);
        mpq_set_z(new_num->data.number.value.big_num, val);  // Set rational from integer
        // mpq_canonicalize is not strictly needed here as den is 1, but doesn't hurt.
        // mpq_canonicalize(new_num->data.number.value.big_num);
        return new_num;
    }
}

// (+) or (+ x y ...)
sl_object *sl_add(sl_object *args) {
    // Implementation of addition
    // args is a list of numbers
    mpq_t result;
    mpq_init(result);
    bool first = true;

    while (args != SL_NIL) {
        sl_object *current = sl_car(args);
        if (!sl_is_number(current)) {
            // Handle error: non-number in addition
            mpq_clear(result);
            return SL_PARSE_ERROR;
        }

        mpq_t current_value;
        sl_number_get_q(current, current_value);

        if (first) {
            mpq_set(result, current_value);
            first = false;
        } else {
            mpq_add(result, result, current_value);
        }

        args = sl_cdr(args);
    }

    // Convert result to sl_object
    sl_object *result_obj = make_number_from_mpq(result);
    mpq_clear(result);
    return result_obj;
}