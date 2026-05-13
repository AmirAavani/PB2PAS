#!/bin/bash

# Setup script for Go gRPC-Web client and server
# Builds the gRPC-Web compatible binaries (HTTP/1.1, no gRPC library needed)

set -e

echo "═══════════════════════════════════════════════════════════════════════════"
echo "       Setting up Go gRPC-Web Client and Server                           "
echo "═══════════════════════════════════════════════════════════════════════════"
echo

# Get the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Step 1: Generate Go protobuf code (messages only, no gRPC)
echo "Step 1: Generating Go protobuf code..."
if ! command -v protoc &> /dev/null; then
    echo "Error: protoc (Protocol Buffer Compiler) not found!"
    echo "Install it with: brew install protobuf"
    exit 1
fi

if ! command -v protoc-gen-go &> /dev/null; then
    echo "Installing protoc-gen-go..."
    go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
fi

protoc --go_out=go-server --go_opt=paths=source_relative service.proto

if [ $? -eq 0 ]; then
    echo "✓ Go protobuf code generated in go-server"
else
    echo "✗ Failed to generate Go code"
    exit 1
fi

# Fix package name in generated file (change from __ to main)
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    sed -i '' 's/^package __$/package main/' go-server/service.pb.go
else
    # Linux
    sed -i 's/^package __$/package main/' go-server/service.pb.go
fi
echo "✓ Fixed package name to 'main'"

# Copy generated files to client directory
cp go-server/service.pb.go go-client/
echo "✓ Go code copied to go-client"
echo

# Step 2: Clean up old files
echo "Step 2: Cleaning up..."
rm -f go-client/go.work go-client/go.work.sum
rm -f go-server/go.work go-server/go.work.sum
rm -f go-client/service_grpc.pb.go go-server/service_grpc.pb.go
# Also check parent directory
rm -f go.work go.work.sum
# Remove any GOWORK environment override
export GOWORK=off
echo "✓ Removed workspace and old gRPC files"
echo

# Step 3: Build Go server
echo "Step 3: Building Go gRPC-Web server..."
cd go-server
export GOWORK=off
rm -f go.work go.work.sum
go mod tidy
go build -o greeter-server *.go
chmod +x greeter-server
if [ $? -eq 0 ]; then
    echo "✓ Go server built: go-server/greeter-server"
else
    echo "✗ Failed to build Go server"
    exit 1
fi
cd ..
echo

# Step 4: Build Go client
echo "Step 4: Building Go gRPC-Web client..."
cd go-client
export GOWORK=off
rm -f go.work go.work.sum
go mod tidy
go build -o greeter-client *.go
chmod +x greeter-client
if [ $? -eq 0 ]; then
    echo "✓ Go client built: go-client/greeter-client"
else
    echo "✗ Failed to build Go client"
    exit 1
fi
cd ..
echo

echo "═══════════════════════════════════════════════════════════════════════════"
echo "                       Setup Complete!                                     "
echo "═══════════════════════════════════════════════════════════════════════════"
echo
echo "Protocol: gRPC-Web (HTTP/1.1) - Compatible with Pascal server!"
echo
echo "Binaries created:"
echo "  • go-server/greeter-server (port 50052)"
echo "  • go-client/greeter-client"
echo
echo "To test:"
echo "  Terminal 1: ./go-server/greeter-server"
echo "  Terminal 2: ./go-client/greeter-client localhost:50052"
echo "  Or with Pascal: ./ServiceClient localhost:50052"
echo
