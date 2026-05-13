#!/bin/bash

# Setup gRPC-Web proxy for cross-language testing
# This allows Go/Python/etc gRPC clients to communicate with Pascal gRPC-Web servers

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "╔═══════════════════════════════════════════════════════════════════════════╗"
echo "║       gRPC-Web Proxy Setup for Cross-Language Testing                    ║"
echo "╚═══════════════════════════════════════════════════════════════════════════╝"
echo

# Check if grpcwebproxy is installed
if ! command -v grpcwebproxy &> /dev/null; then
    echo "📦 Installing grpcwebproxy..."
    go install github.com/improbable-eng/grpc-web/go/grpcwebproxy@latest
    
    if ! command -v grpcwebproxy &> /dev/null; then
        echo "❌ Failed to install grpcwebproxy"
        echo "   Make sure Go is installed and GOPATH/bin is in your PATH"
        exit 1
    fi
    echo "✅ grpcwebproxy installed"
else
    echo "✅ grpcwebproxy is already installed"
fi

echo
echo "╔═══════════════════════════════════════════════════════════════════════════╗"
echo "║                    Starting gRPC-Web Proxy                                ║"
echo "╚═══════════════════════════════════════════════════════════════════════════╝"
echo
echo "Configuration:"
echo "  Frontend (Clients connect here): http://localhost:8080"
echo "  Backend (Pascal server):         http://localhost:50051"
echo "  Protocol Translation:            HTTP/2 gRPC ↔ gRPC-Web"
echo
echo "Usage:"
echo "  1. Start your Pascal server on port 50051"
echo "  2. Point your Go/Python clients to localhost:8080"
echo "  3. The proxy will translate between protocols"
echo
echo "Press Ctrl+C to stop the proxy"
echo
echo "─────────────────────────────────────────────────────────────────────────────"
echo

# Start the proxy
grpcwebproxy \
  --backend_addr=localhost:50051 \
  --backend_tls=false \
  --run_tls_server=false \
  --allow_all_origins \
  --server_http_debug_port=8081 \
  --server_http_max_read_timeout=5m \
  --server_http_max_write_timeout=5m
