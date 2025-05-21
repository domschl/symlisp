#!/bin/bash

# Test script for MCP SymLisp server
cd "$(dirname "$0")/../build/mcp_symlisp"

# Start the server with no standard library loading
echo "Starting MCP server..."
./mcp_symlisp -n &
SERVER_PID=$!

# Give it a moment to initialize
sleep 1

# Function to send JSONRPC request and receive response
function send_request() {
  local request="$1"
  echo "Sending: $request"
  echo "$request" | ./mcp_symlisp -n
}

# Test version request
echo -e "\n=== Testing version request ==="
send_request '{"jsonrpc":"2.0","id":"1","method":"version"}'

# Test capabilities request
echo -e "\n=== Testing capabilities request ==="
send_request '{"jsonrpc":"2.0","id":"2","method":"capabilities"}'

# Test eval request with simple arithmetic
echo -e "\n=== Testing eval request ==="
send_request '{"jsonrpc":"2.0","id":"3","method":"eval","params":{"code":"(+ 2 3)"}}'

# Test eval request with error
echo -e "\n=== Testing eval with error ==="
send_request '{"jsonrpc":"2.0","id":"4","method":"eval","params":{"code":"(/ 1 0)"}}'

# Test invalid method
echo -e "\n=== Testing invalid method ==="
send_request '{"jsonrpc":"2.0","id":"5","method":"invalid_method"}'

# Test invalid JSON
echo -e "\n=== Testing invalid JSON ==="
send_request '{"jsonrpc":"2.0","id":"6",method:"broken"}'

# Test notification (no response expected)
echo -e "\n=== Testing notification ==="
send_request '{"jsonrpc":"2.0","method":"eval","params":{"code":"(display \"Hello, notification!\")"}}'

echo -e "\nTests complete!"
