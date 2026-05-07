#!/bin/bash

# Setup script for Go implementations
set -e

echo "=== Setting up Go gRPC Client and Server ==="
echo

# Get the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Generate Go code
echo "Step 1: Generating Go protobuf code..."
if ! command -v protoc &> /dev/null; then
    echo "Error: protoc (Protocol Buffer Compiler) not found!"
    echo "Install it with: brew install protobuf"
    exit 1
fi

# Check for protoc-gen-go and protoc-gen-go-grpc
if ! command -v protoc-gen-go &> /dev/null; then
    echo "Installing protoc-gen-go..."
    go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
fi

if ! command -v protoc-gen-go-grpc &> /dev/null; then
    echo "Installing protoc-gen-go-grpc..."
    go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
fi

# Generate Go code for server (run from Samples dir)
protoc --go_out=go-server --go_opt=paths=source_relative \
       --go-grpc_out=go-server --go-grpc_opt=paths=source_relative \
       service.proto

# Fix package name in generated files to be 'main'
sed -i '' 's/^package __$/package main/' go-server/service.pb.go
sed -i '' 's/^package __$/package main/' go-server/service_grpc.pb.go

echo "✓ Go code generated in go-server"

# Copy generated files to go-client
cp go-server/*.pb.go go-client/
echo "✓ Go code copied to go-client"
echo

# Build Go server
echo "Step 2: Building Go server..."
cd go-server
go mod tidy
go build -o greeter-server *.go
chmod +x greeter-server
cd ..
echo "✓ Go server built: go-server/greeter-server"
echo

# Build Go client
echo "Step 3: Building Go client..."
cd go-client
go mod tidy
go build -o greeter-client *.go
chmod +x greeter-client
cd ..
echo "✓ Go client built: go-client/greeter-client"
echo

echo "╔═══════════════════════════════════════════════════════════╗"
echo "║               Setup Complete!                             ║"
echo "╚═══════════════════════════════════════════════════════════╝"
echo
echo "Binaries created:"
echo "  • go-server/greeter-server"
echo "  • go-client/greeter-client"
echo
echo "To test:"
echo "  Terminal 1: ./go-server/greeter-server"
echo "  Terminal 2: ./go-client/greeter-client"
echo
