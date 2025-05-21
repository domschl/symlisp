#!/bin/bash

echo "This script will help you test the MCP server interactively."
echo "Start the MCP server in another terminal with: ./build/mcp_symlisp/mcp_symlisp -n"
echo "Then run this script and copy/paste the JSONRPC requests below into the server terminal."
echo ""
echo "=== Version request ==="
echo '{"jsonrpc":"2.0","id":"1","method":"version"}'
echo ""
echo "=== Capabilities request ==="
echo '{"jsonrpc":"2.0","id":"2","method":"capabilities"}'
echo ""
echo "=== Eval simple arithmetic ==="
echo '{"jsonrpc":"2.0","id":"3","method":"eval","params":{"code":"(+ 2 3)"}}'
echo ""
echo "=== Eval with error ==="
echo '{"jsonrpc":"2.0","id":"4","method":"eval","params":{"code":"(/ 1 0)"}}'
echo ""
echo "=== Invalid method ==="
echo '{"jsonrpc":"2.0","id":"5","method":"invalid_method"}'
