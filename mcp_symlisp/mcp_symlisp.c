#define _GNU_SOURCE  // For getline()
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>  // For getopt
#include <getopt.h>
#include <sys/types.h>  // For ssize_t

#include "sl_core.h"
#include "sl_env.h"
#include "sl_builtins.h"
#include "sl_eval.h"
#include "mcp_jsonrpc.h"

// Default path for standard library relative to executable location
#define DEFAULT_STDLIB_PATH "../stdsymlisp"

// Buffer size for reading from stdin
#define BUFFER_SIZE 4096

/**
 * Simple MCP (Model Context Protocol) server implementation for SymLisp
 * This is a basic implementation that communicates over stdio using JSONRPC 2.0
 */

// Handle specific MCP methods
jsonrpc_response *handle_eval_method(jsonrpc_request *req, sl_object *env) {
    // Check if params is an object with a "code" field
    cJSON *code_json = NULL;
    if (req->params && cJSON_IsObject(req->params)) {
        code_json = cJSON_GetObjectItem(req->params, "code");
    }

    if (!code_json || !cJSON_IsString(code_json)) {
        return jsonrpc_create_error_response(req->id, JSONRPC_INVALID_PARAMS,
                                             "Missing or invalid 'code' parameter", NULL);
    }

    // Evaluate the code
    sl_object *result = sl_eval_string(code_json->valuestring, env);
    if (result == NULL || sl_is_error(result)) {
        // Return error as JSON
        char *err_str = sl_object_to_string(result ? result : SL_NIL);
        cJSON *err_data = cJSON_CreateString(err_str ? err_str : "Unknown error");
        if (err_str) free(err_str);

        return jsonrpc_create_error_response(req->id, JSONRPC_SERVER_ERROR,
                                             "Evaluation error", err_data);
    } else {
        // Convert result to JSON and return
        cJSON *json_result = sl_object_to_json(result);
        return jsonrpc_create_success_response(req->id, json_result);
    }
}

// Process a JSONRPC request and generate a response
jsonrpc_response *process_request(jsonrpc_request *req, sl_object *env) {
    if (!req || !req->method) {
        return jsonrpc_create_error_response(req ? req->id : NULL,
                                             JSONRPC_INVALID_REQUEST,
                                             "Invalid request", NULL);
    }

    // Handle method: eval (evaluate SymLisp code)
    if (strcmp(req->method, "eval") == 0) {
        return handle_eval_method(req, env);
    }
    // Handle method: version (get MCP server version)
    else if (strcmp(req->method, "version") == 0) {
        cJSON *version = cJSON_CreateObject();
        cJSON_AddStringToObject(version, "mcp_version", "0.1.0");
        cJSON_AddStringToObject(version, "server", "SymLisp MCP");
        cJSON_AddStringToObject(version, "language", "SymLisp");
        return jsonrpc_create_success_response(req->id, version);
    }
    // Handle method: capabilities (get server capabilities)
    else if (strcmp(req->method, "capabilities") == 0) {
        cJSON *capabilities = cJSON_CreateObject();

        // Add supported methods array
        cJSON *methods = cJSON_CreateArray();
        cJSON_AddItemToArray(methods, cJSON_CreateString("eval"));
        cJSON_AddItemToArray(methods, cJSON_CreateString("version"));
        cJSON_AddItemToArray(methods, cJSON_CreateString("capabilities"));

        cJSON_AddItemToObject(capabilities, "methods", methods);

        return jsonrpc_create_success_response(req->id, capabilities);
    }
    // Unknown method
    else {
        return jsonrpc_create_error_response(req->id, JSONRPC_METHOD_NOT_FOUND,
                                             "Method not found", NULL);
    }
}

// Helper function to write a response to stdout
void write_response(jsonrpc_response *response) {
    if (!response) return;

    char *resp_str = jsonrpc_response_to_string(response);
    if (resp_str) {
        printf("%s\n", resp_str);
        fflush(stdout);
        free(resp_str);
    } else {
        // Fallback in case of error generating response
        fprintf(stderr, "Error generating response JSON\n");
        printf("{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}\n");
        fflush(stdout);
    }
}

// Main MCP server loop
void run_mcp_server() {
    fprintf(stderr, "MCP SymLisp server initialized\n");

    // Initialize JSONRPC
    jsonrpc_init();

    char *buffer = NULL;
    size_t buffer_size = 0;
    size_t length;
    bool running = true;

    // Disable stdout buffering for immediate responses
    setbuf(stdout, NULL);

    // Main server loop
    while (running) {
        // Read a line from stdin
        ssize_t read_bytes = getline(&buffer, &buffer_size, stdin);

        if (read_bytes <= 0) {
            // EOF or error
            fprintf(stderr, "Error reading from stdin or EOF\n");
            break;
        }

        // Remove newline character if present
        length = strlen(buffer);
        if (length > 0 && buffer[length - 1] == '\n') {
            buffer[length - 1] = '\0';
        }

        // Skip empty lines
        if (length <= 1) {
            continue;
        }

        // Special command to exit (for debugging purposes)
        if (strcmp(buffer, "##EXIT##") == 0) {
            fprintf(stderr, "Exit command received\n");
            break;
        }

        // Parse the request
        jsonrpc_request *request = jsonrpc_parse_request(buffer);
        if (!request) {
            // Send parse error response
            fprintf(stderr, "Error parsing JSON request: %s\n", buffer);
            jsonrpc_response *error_resp = jsonrpc_create_error_response(
                NULL, JSONRPC_PARSE_ERROR, "Parse error", NULL);
            write_response(error_resp);
            jsonrpc_free_response(error_resp);
            continue;
        }

        // Log the request (for debugging)
        fprintf(stderr, "Received request: method=%s, id=%s, notification=%s\n",
                request->method,
                request->id ? request->id : "null",
                request->is_notification ? "true" : "false");

        // Process the request
        jsonrpc_response *response = process_request(request, sl_global_env);

        // If not a notification, send the response
        if (!request->is_notification && response) {
            write_response(response);
        }

        // Free resources
        jsonrpc_free_request(request);
        if (response) {
            jsonrpc_free_response(response);
        }

        // Run garbage collection after each request
        sl_gc();
    }

    // Clean up
    if (buffer) {
        free(buffer);
    }

    // Clean up JSONRPC
    jsonrpc_cleanup();

    // Clean up library memory before exit
    sl_mem_shutdown();
}

int main(int argc, char *argv[]) {
    bool load_stdlib = true;  // Default to loading stdlib
    int opt;
    char *stdlib_path = NULL;  // Path to standard library, NULL means use default

    // Simple argument parsing using getopt
    while ((opt = getopt(argc, argv, "nl:")) != -1) {
        switch (opt) {
        case 'n':
            load_stdlib = false;
            break;
        case 'l':
            stdlib_path = optarg;
            break;
        case '?':  // Unknown option or missing argument
            fprintf(stderr, "Usage: %s [-n] [-l path]\n", argv[0]);
            fprintf(stderr, "  -n: Do not load standard library\n");
            fprintf(stderr, "  -l path: Specify path to standard library folder\n");
            return 1;
        default:
            // Should not happen with this simple option string
            break;
        }
    }

    // Initialize memory, GC, core objects (NIL, TRUE, FALSE, OOM)
    sl_mem_init(0);  // Use default heap size for now

    // Create the global environment
    printf("Creating global environment...\n");
    sl_global_env = sl_env_create(NULL);  // Top-level env has no parent
    if (!sl_global_env || sl_global_env == SL_OUT_OF_MEMORY_ERROR) {
        fprintf(stderr, "Fatal: Failed to create global environment.\n");
        sl_mem_shutdown();  // Use shutdown before exit
        return 1;
    }
    printf("Global environment created.\n");
    sl_gc_add_root(&sl_global_env);  // Keep global env rooted

    // Initialize built-in functions and add them to the global environment
    sl_builtins_init(sl_global_env);

    // --- Load Standard Library (conditionally) ---
    if (load_stdlib) {
        // Use specified path or default
        const char *path_to_use = stdlib_path ? stdlib_path : DEFAULT_STDLIB_PATH;
        printf("Loading standard library from: %s\n", path_to_use);
        sl_object *load_lib_result = sl_load_directory(path_to_use, sl_global_env);
        if (load_lib_result != SL_TRUE) {
            fprintf(stderr, "Error loading standard library:\n");
            // Print the error object returned by sl_load_directory
            char *err_str = sl_object_to_string(load_lib_result);
            if (err_str) {
                fprintf(stderr, "  %s\n", err_str);
                free(err_str);
            } else {
                fprintf(stderr, "  Unknown error during library loading.\n");
            }
            sl_mem_shutdown();  // Use shutdown before exit
            return 1;
        }
        printf("Standard library loaded.\n");
    } else {
        printf("Skipping standard library loading (-n specified).\n");
    }

    // Run the MCP server
    run_mcp_server();

    return 0;  // Normal exit
}
