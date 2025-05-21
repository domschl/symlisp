#ifndef MCP_JSONRPC_H
#define MCP_JSONRPC_H

#include <stdbool.h>
#include "deps/cJSON.h"
#include "sl_core.h"

// JSONRPC 2.0 constants
#define JSONRPC_VERSION "2.0"

// Error codes (standard JSONRPC 2.0 codes)
#define JSONRPC_PARSE_ERROR      -32700
#define JSONRPC_INVALID_REQUEST  -32600
#define JSONRPC_METHOD_NOT_FOUND -32601
#define JSONRPC_INVALID_PARAMS   -32602
#define JSONRPC_INTERNAL_ERROR   -32603
// Server error codes range from -32000 to -32099
#define JSONRPC_SERVER_ERROR     -32000

// JSONRPC request structure
typedef struct {
    char *id;              // Request ID (can be string, number, or null)
    char *method;          // Method name
    cJSON *params;         // Parameters (can be array or object)
    bool is_notification;  // If true, no response is expected
} jsonrpc_request;

// JSONRPC response structure
typedef struct {
    char *id;              // Request ID (must match the request)
    cJSON *result;         // Result (null if error)
    cJSON *error;          // Error (null if success)
} jsonrpc_response;

// Initialize JSONRPC system
void jsonrpc_init(void);

// Cleanup JSONRPC system resources
void jsonrpc_cleanup(void);

// Parse a JSONRPC message
jsonrpc_request* jsonrpc_parse_request(const char *json_str);

// Free resources used by a request
void jsonrpc_free_request(jsonrpc_request *req);

// Create a JSONRPC success response
jsonrpc_response* jsonrpc_create_success_response(const char *id, cJSON *result);

// Create a JSONRPC error response
jsonrpc_response* jsonrpc_create_error_response(const char *id, int code, const char *message, cJSON *data);

// Convert response to JSON string (caller must free the returned string)
char* jsonrpc_response_to_string(jsonrpc_response *res);

// Free resources used by a response
void jsonrpc_free_response(jsonrpc_response *res);

// Convert SymLisp object to JSON
cJSON* sl_object_to_json(sl_object *obj);

// Convert JSON to SymLisp object
sl_object* json_to_sl_object(cJSON *json);

#endif /* MCP_JSONRPC_H */
