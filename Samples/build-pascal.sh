#!/bin/bash

# Build script for Pascal gRPC components with gRPC-Web support

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  Building Pascal gRPC Components (gRPC-Web enabled)           ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo

# Set up paths
MODULES_PATH="../modules/*"
TEMP_PATH="/tmp/pascal-grpc-build"

# Create temp directory for compiled units
mkdir -p "$TEMP_PATH"

echo "Building ServiceServer..."
fpc -Sd ServiceServer.lpr \
  -Fu"$MODULES_PATH" \
  -FU"$TEMP_PATH" \
  -oServiceServer

if [ $? -eq 0 ]; then
    echo "✓ ServiceServer built successfully"
else
    echo "✗ ServiceServer build failed"
    exit 1
fi

echo
echo "Building ServiceClient..."
fpc -Sd ServiceClient.lpr \
  -Fu"$MODULES_PATH" \
  -FU"$TEMP_PATH" \
  -oServiceClient

if [ $? -eq 0 ]; then
    echo "✓ ServiceClient built successfully"
else
    echo "✗ ServiceClient build failed"
    exit 1
fi

echo
echo "╔═══════════════════════════════════════════════════════════════╗"
echo "║  Build Complete!                                              ║"
echo "╚═══════════════════════════════════════════════════════════════╝"
echo
echo "Binaries created:"
echo "  • ServiceServer (gRPC-Web server on port 50051)"
echo "  • ServiceClient (gRPC-Web client)"
echo
echo "Run tests with: ./test-interop.sh"
echo "Or manually:"
echo "  Terminal 1: ./ServiceServer"
echo "  Terminal 2: ./ServiceClient"
