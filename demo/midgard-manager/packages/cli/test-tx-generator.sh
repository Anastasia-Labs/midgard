#!/bin/bash

# Test script for the transaction generator CLI commands
echo "=== Testing Midgard Transaction Generator CLI ==="
echo ""

# Set the path to the CLI executable
CLI_BIN="dist/bin.js"

# Test wallet management
echo "Testing wallet management commands..."
echo "- Listing wallets:"
node $CLI_BIN wallet list

echo "- Adding a test wallet:"
node $CLI_BIN wallet add --name test-wallet2 --private-key ed25519_sk1m6mqw03v8p52je07ptkzrxkcdgwpvdmmc6stzjqclsjk72zv99vqxl7xz3

echo "- Listing wallets again:"
node $CLI_BIN wallet list

echo ""

# Test transaction generator
echo "Testing transaction generator commands..."
echo "- Starting one-to-one transaction generator (will run for 10 seconds):"
node $CLI_BIN generate-tx --type one-to-one --batch-size 2 --interval 3 &
GENERATOR_PID=$!

echo "- Checking status after 5 seconds:"
sleep 5
node $CLI_BIN tx-status

echo "- Stopping generator:"
node $CLI_BIN stop-tx
kill $GENERATOR_PID 2>/dev/null  # backup in case stop-tx doesn't work

echo ""

echo "- Starting mixed transaction generator (will run for 10 seconds):"
node $CLI_BIN generate-tx --type mixed --one-to-one-ratio 60 --batch-size 2 --interval 3 &
GENERATOR_PID=$!

echo "- Checking status after 5 seconds:"
sleep 5
node $CLI_BIN tx-status

echo "- Stopping generator:"
node $CLI_BIN stop-tx
kill $GENERATOR_PID 2>/dev/null  # backup in case stop-tx doesn't work

echo ""
echo "=== Tests Complete ==="

# Note: This is a basic test script. In a real environment, you'd want more robust testing
# with proper assertions and error handling. 