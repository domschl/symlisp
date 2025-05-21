#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mcp_jsonrpc.h"

void jsonrpc_init(void) {
    // Currently nothing to initialize
}

void jsonrpc_cleanup(void) {
    // Currently nothing to clean up
}

jsonrpc_request* jsonrpc_parse_request(const char *json_str) {
    if (!json_str) {
        return NULL;
    }

    cJSON *root = cJSON_Parse(json_str);
    if (!root) {
        // Parse error
        return NULL;
    }

    jsonrpc_request *req = (jsonrpc_request*)malloc(sizeof(jsonrpc_request));
    if (!req) {
        cJSON_Delete(root);
        return NULL;
    }
    memset(req, 0, sizeof(jsonrpc_request));

    // Check jsonrpc version
    cJSON *version = cJSON_GetObjectItem(root, "jsonrpc");
    if (!version || !cJSON_IsString(version) || strcmp(version->valuestring, JSONRPC_VERSION) != 0) {
        cJSON_Delete(root);
        free(req);
        return NULL;
    }

    // Get method
    cJSON *method = cJSON_GetObjectItem(root, "method");
    if (!method || !cJSON_IsString(method)) {
        cJSON_Delete(root);
        free(req);
        return NULL;
    }
    req->method = strdup(method->valuestring);

    // Get ID (if present)
    cJSON *id = cJSON_GetObjectItem(root, "id");
    if (id) {
        if (cJSON_IsString(id)) {
            req->id = strdup(id->valuestring);
        } else if (cJSON_IsNumber(id)) {
            // Convert number to string
            char num_str[64];
            snprintf(num_str, sizeof(num_str), "%.0f", id->valuedouble);
            req->id = strdup(num_str);
        } else {
            // Null or invalid ID
            req->id = NULL;
        }
        req->is_notification = false;
    } else {
        req->id = NULL;
        req->is_notification = true;
    }

    // Get params (if present)
    cJSON *params = cJSON_GetObjectItem(root, "params");
    if (params) {
        // Detach params from root so it's not deleted
        cJSON_DetachItemViaPointer(root, params);
        req->params = params;
    } else {
        req->params = NULL;
    }

    cJSON_Delete(root);
    return req;
}

void jsonrpc_free_request(jsonrpc_request *req) {
    if (!req) {
        return;
    }

    free(req->method);
    free(req->id);
    if (req->params) {
        cJSON_Delete(req->params);
    }
    free(req);
}

jsonrpc_response* jsonrpc_create_success_response(const char *id, cJSON *result) {
    jsonrpc_response *res = (jsonrpc_response*)malloc(sizeof(jsonrpc_response));
    if (!res) {
        return NULL;
    }
    memset(res, 0, sizeof(jsonrpc_response));

    res->id = id ? strdup(id) : NULL;
    res->result = result;  // Take ownership
    res->error = NULL;

    return res;
}

jsonrpc_response* jsonrpc_create_error_response(const char *id, int code, const char *message, cJSON *data) {
    jsonrpc_response *res = (jsonrpc_response*)malloc(sizeof(jsonrpc_response));
    if (!res) {
        return NULL;
    }
    memset(res, 0, sizeof(jsonrpc_response));

    res->id = id ? strdup(id) : NULL;
    res->result = NULL;

    // Create error object
    res->error = cJSON_CreateObject();
    if (!res->error) {
        free(res->id);
        free(res);
        return NULL;
    }

    cJSON_AddNumberToObject(res->error, "code", code);
    cJSON_AddStringToObject(res->error, "message", message ? message : "Unknown error");
    
    if (data) {
        cJSON_AddItemToObject(res->error, "data", data);  // Take ownership
    }

    return res;
}

char* jsonrpc_response_to_string(jsonrpc_response *res) {
    if (!res) {
        return NULL;
    }

    cJSON *root = cJSON_CreateObject();
    if (!root) {
        return NULL;
    }

    // Add jsonrpc version
    cJSON_AddStringToObject(root, "jsonrpc", JSONRPC_VERSION);

    // Add id (can be null)
    if (res->id) {
        cJSON_AddStringToObject(root, "id", res->id);
    } else {
        cJSON_AddNullToObject(root, "id");
    }

    // Add result or error
    if (res->error) {
        // Detach error from response so it's not deleted twice
        cJSON *error = res->error;
        res->error = NULL;
        cJSON_AddItemToObject(root, "error", error);
    } else if (res->result) {
        // Detach result from response so it's not deleted twice
        cJSON *result = res->result;
        res->result = NULL;
        cJSON_AddItemToObject(root, "result", result);
    } else {
        // Either result or error must be present
        cJSON_AddNullToObject(root, "result");
    }

    // Generate JSON string
    char *json_str = cJSON_PrintUnformatted(root);
    cJSON_Delete(root);

    return json_str;
}

void jsonrpc_free_response(jsonrpc_response *res) {
    if (!res) {
        return;
    }

    free(res->id);
    if (res->result) {
        cJSON_Delete(res->result);
    }
    if (res->error) {
        cJSON_Delete(res->error);
    }
    free(res);
}

// Convert SymLisp object to JSON
cJSON* sl_object_to_json(sl_object *obj) {
    if (!obj) {
        return cJSON_CreateNull();
    }

    // Handle basic types
    sl_object_type type = sl_get_object_type(obj);
    switch (type) {
        case SL_TYPE_NIL:
            return cJSON_CreateNull();
        case SL_TYPE_BOOLEAN:
            return sl_is_true(obj) ? cJSON_CreateTrue() : cJSON_CreateFalse();
        case SL_TYPE_NUMBER: {
            // Get number as double (simplest approach)
            mpq_t q;
            mpq_init(q);
            sl_number_get_q(obj, q);
            double val = mpq_get_d(q);
            mpq_clear(q);
            return cJSON_CreateNumber(val);
        }
        case SL_TYPE_STRING: {
            // Get string value
            char *str_val = sl_object_to_string(obj);
            cJSON *json = cJSON_CreateString(str_val);
            free(str_val);
            return json;
        }
        case SL_TYPE_SYMBOL: {
            // Get symbol name
            const char *sym_name = sl_symbol_name(obj);
            return cJSON_CreateString(sym_name);
        }
        case SL_TYPE_PAIR: {
            // Convert list to JSON array
            cJSON *array = cJSON_CreateArray();
            sl_object *current = obj;
            
            while (sl_is_pair(current)) {
                cJSON *item = sl_object_to_json(sl_car(current));
                cJSON_AddItemToArray(array, item);
                current = sl_cdr(current);
            }
            
            // Handle improper list with non-nil tail
            if (current != SL_NIL) {
                cJSON *tail = sl_object_to_json(current);
                cJSON_AddItemToArray(array, tail);
            }
            
            return array;
        }
        default:
            // For other types, convert to string
            char *str = sl_object_to_string(obj);
            cJSON *json = cJSON_CreateString(str);
            free(str);
            return json;
    }
}

// Convert JSON to SymLisp object
sl_object* json_to_sl_object(cJSON *json) {
    if (!json) {
        return SL_NIL;
    }

    // Handle basic types
    if (cJSON_IsNull(json)) {
        return SL_NIL;
    } else if (cJSON_IsBool(json)) {
        return cJSON_IsTrue(json) ? SL_TRUE : SL_FALSE;
    } else if (cJSON_IsNumber(json)) {
        // Create a number object from the double value
        // For simplicity, we'll use the integer version if it's a whole number
        double value = json->valuedouble;
        if (value == (int64_t)value) {
            return sl_make_number_si((int64_t)value, 1);
        } else {
            mpq_t q;
            mpq_init(q);
            mpq_set_d(q, value);
            sl_object *num = sl_make_number_q(q);
            mpq_clear(q);
            return num;
        }
    } else if (cJSON_IsString(json)) {
        return sl_make_string(json->valuestring);
    } else if (cJSON_IsArray(json)) {
        // Convert JSON array to SymLisp list
        sl_object *list = SL_NIL;
        int size = cJSON_GetArraySize(json);
        
        // Build list in reverse order
        for (int i = size - 1; i >= 0; i--) {
            cJSON *item = cJSON_GetArrayItem(json, i);
            sl_object *sl_item = json_to_sl_object(item);
            list = sl_make_pair(sl_item, list);
        }
        
        return list;
    } else if (cJSON_IsObject(json)) {
        // Convert JSON object to SymLisp association list (alist)
        sl_object *alist = SL_NIL;
        
        cJSON *child = json->child;
        while (child) {
            sl_object *key = sl_make_symbol(child->string);
            sl_object *value = json_to_sl_object(child);
            sl_object *pair = sl_make_pair(key, value);
            alist = sl_make_pair(pair, alist);
            
            child = child->next;
        }
        
        return alist;
    }
    
    // Should not happen
    return SL_NIL;
}
