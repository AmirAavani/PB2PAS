# Go Client ↔ Pascal Server Communication Guide

## Overview

This guide explains how to develop a Go client that communicates with a Pascal gRPC server. The Pascal implementation uses **gRPC-Web over HTTP/1.1**, while Go's standard gRPC uses **HTTP/2**. This document covers both direct communication and proxy-based approaches.

## Protocol Compatibility

| Component | Protocol | Content-Type |
|-----------|----------|--------------|
| Pascal Server | gRPC-Web / HTTP/1.1 | `application/grpc-web+proto` |
| Go Standard gRPC | HTTP/2 | `application/grpc+proto` |

**The Challenge:** These protocols are not directly compatible.

## Solution Options

### Option 1: Use gRPC-Web in Go (Recommended for Development)

Make your Go client speak gRPC-Web to communicate directly with the Pascal server.

### Option 2: Use Envoy Proxy (Recommended for Production)

Deploy an Envoy proxy that translates between HTTP/2 gRPC and gRPC-Web.

### Option 3: Update Pascal Server to HTTP/2

Implement full HTTP/2 support in Pascal (complex, not covered in this guide).

---

## Option 1: Go Client with gRPC-Web

### Step 1: Install gRPC-Web for Go

```bash
go get github.com/improbable-eng/grpc-web/go/grpcweb
go get google.golang.org/grpc
go get google.golang.org/protobuf
```

### Step 2: Create Your Proto Definition

```protobuf
// service.proto
syntax = "proto3";

option go_package = "./pb";

message HelloRequest {
  string name = 1;
}

message HelloReply {
  string message = 1;
}

service GreeterService {
  rpc SayHello (HelloRequest) returns (HelloReply);
}
```

### Step 3: Generate Go Code

```bash
protoc --go_out=. --go_opt=paths=source_relative \
       --go-grpc_out=. --go-grpc_opt=paths=source_relative \
       service.proto
```

### Step 4: Create gRPC-Web Go Client

```go
// client.go
package main

import (
    "context"
    "fmt"
    "log"
    "net/http"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
    pb "./pb" // Your generated code
)

func main() {
    // Create HTTP/1.1 transport with gRPC-Web support
    transport := &http.Transport{
        // Force HTTP/1.1
        DisableHTTP2: true,
    }
    
    httpClient := &http.Client{
        Transport: transport,
    }

    // Connect to Pascal server
    conn, err := grpc.Dial(
        "localhost:50051",
        grpc.WithTransportCredentials(insecure.NewCredentials()),
        // Use gRPC-Web protocol
        grpc.WithDefaultCallOptions(
            grpc.CallContentSubtype("grpc-web+proto"),
        ),
        // Custom dialer for HTTP/1.1
        grpc.WithContextDialer(func(ctx context.Context, addr string) (net.Conn, error) {
            return net.Dial("tcp", addr)
        }),
    )
    if err != nil {
        log.Fatalf("Failed to connect: %v", err)
    }
    defer conn.Close()

    // Create client
    client := pb.NewGreeterServiceClient(conn)

    // Make request
    ctx := context.Background()
    req := &pb.HelloRequest{Name: "Go Client"}
    
    resp, err := client.SayHello(ctx, req)
    if err != nil {
        log.Fatalf("Error: %v", err)
    }
    
    fmt.Printf("Response: %s\n", resp.Message)
}
```

### Step 5: Alternative - Using grpcweb Package

```go
package main

import (
    "context"
    "fmt"
    "log"
    "net/http"

    "github.com/improbable-eng/grpc-web/go/grpcwebclient"
    "google.golang.org/grpc"
    pb "./pb"
)

func main() {
    // Create gRPC-Web client
    opts := []grpc.DialOption{
        grpc.WithTransportCredentials(insecure.NewCredentials()),
        grpc.WithContextDialer(grpcwebclient.GrpcWebClientDialer(http.DefaultClient)),
    }

    conn, err := grpc.Dial("http://localhost:50051", opts...)
    if err != nil {
        log.Fatalf("Failed to dial: %v", err)
    }
    defer conn.Close()

    client := pb.NewGreeterServiceClient(conn)
    
    req := &pb.HelloRequest{Name: "Go gRPC-Web Client"}
    resp, err := client.SayHello(context.Background(), req)
    if err != nil {
        log.Fatalf("Error: %v", err)
    }
    
    fmt.Printf("Response: %s\n", resp.Message)
}
```

---

## Option 2: Envoy Proxy (Production Setup)

### Step 1: Install Envoy

```bash
# Using Docker
docker pull envoyproxy/envoy:v1.27-latest

# Or using Homebrew on macOS
brew install envoy
```

### Step 2: Create Envoy Configuration

```yaml
# envoy.yaml
static_resources:
  listeners:
  - name: listener_0
    address:
      socket_address:
        address: 0.0.0.0
        port_value: 8080
    filter_chains:
    - filters:
      - name: envoy.filters.network.http_connection_manager
        typed_config:
          "@type": type.googleapis.com/envoy.extensions.filters.network.http_connection_manager.v3.HttpConnectionManager
          codec_type: AUTO
          stat_prefix: ingress_http
          route_config:
            name: local_route
            virtual_hosts:
            - name: local_service
              domains: ["*"]
              routes:
              - match:
                  prefix: "/"
                route:
                  cluster: pascal_backend
                  timeout: 30s
              cors:
                allow_origin_string_match:
                - prefix: "*"
                allow_methods: GET, PUT, DELETE, POST, OPTIONS
                allow_headers: keep-alive,user-agent,cache-control,content-type,content-transfer-encoding,x-accept-content-transfer-encoding,x-accept-response-streaming,x-user-agent,x-grpc-web,grpc-timeout
                max_age: "1728000"
                expose_headers: grpc-status,grpc-message
          http_filters:
          - name: envoy.filters.http.grpc_web
            typed_config:
              "@type": type.googleapis.com/envoy.extensions.filters.http.grpc_web.v3.GrpcWeb
          - name: envoy.filters.http.cors
            typed_config:
              "@type": type.googleapis.com/envoy.extensions.filters.http.cors.v3.Cors
          - name: envoy.filters.http.router
            typed_config:
              "@type": type.googleapis.com/envoy.extensions.filters.http.router.v3.Router

  clusters:
  - name: pascal_backend
    type: LOGICAL_DNS
    connect_timeout: 0.25s
    dns_lookup_family: V4_ONLY
    lb_policy: ROUND_ROBIN
    load_assignment:
      cluster_name: pascal_backend
      endpoints:
      - lb_endpoints:
        - endpoint:
            address:
              socket_address:
                address: localhost
                port_value: 50051
```

### Step 3: Start Envoy Proxy

```bash
# Using Docker
docker run -d \
  -p 8080:8080 \
  -v $(pwd)/envoy.yaml:/etc/envoy/envoy.yaml \
  envoyproxy/envoy:v1.27-latest

# Or directly
envoy -c envoy.yaml
```

### Step 4: Standard Go gRPC Client (via Proxy)

```go
package main

import (
    "context"
    "fmt"
    "log"
    "time"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
    pb "./pb"
)

func main() {
    // Connect to Envoy proxy (not directly to Pascal server)
    conn, err := grpc.Dial(
        "localhost:8080", // Envoy proxy address
        grpc.WithTransportCredentials(insecure.NewCredentials()),
        grpc.WithBlock(),
        grpc.WithTimeout(5*time.Second),
    )
    if err != nil {
        log.Fatalf("Failed to connect: %v", err)
    }
    defer conn.Close()

    // Use standard gRPC client
    client := pb.NewGreeterServiceClient(conn)
    
    ctx := context.Background()
    req := &pb.HelloRequest{Name: "Go Client via Envoy"}
    
    resp, err := client.SayHello(ctx, req)
    if err != nil {
        log.Fatalf("Error: %v", err)
    }
    
    fmt.Printf("Response: %s\n", resp.Message)
}
```

### Architecture with Envoy

```
┌──────────────┐   HTTP/2 gRPC    ┌───────────┐   gRPC-Web    ┌────────────────┐
│  Go Client   │ ───────────────> │   Envoy   │ ────────────> │ Pascal Server  │
│  (Standard)  │                   │   Proxy   │  (HTTP/1.1)   │   (gRPC-Web)   │
└──────────────┘                   └───────────┘               └────────────────┘
     :8080                             :8080                         :50051
```

---

## Complete Working Example

### Directory Structure

```
my-go-client/
├── go.mod
├── go.sum
├── service.proto
├── pb/
│   ├── service.pb.go
│   └── service_grpc.pb.go
├── client.go
└── envoy.yaml (optional)
```

### go.mod

```go
module myapp/client

go 1.21

require (
    google.golang.org/grpc v1.59.0
    google.golang.org/protobuf v1.31.0
)
```

### Build and Run

```bash
# Generate proto code
protoc --go_out=pb --go_opt=paths=source_relative \
       --go-grpc_out=pb --go-grpc_opt=paths=source_relative \
       service.proto

# Build
go build -o client client.go

# Start Pascal server
cd /path/to/PB2PAS/Samples
./ServiceServer

# Run Go client
./client
```

---

## Testing Communication

### Test 1: Direct Communication (gRPC-Web)

```bash
# Terminal 1: Start Pascal server
cd modules/PB2PAS/Samples
./ServiceServer

# Terminal 2: Run Go gRPC-Web client
cd my-go-client
./client
```

**Expected Output:**
```
Response: Hello, Go Client!
```

### Test 2: Via Envoy Proxy

```bash
# Terminal 1: Start Pascal server
./ServiceServer

# Terminal 2: Start Envoy
envoy -c envoy.yaml

# Terminal 3: Run standard Go gRPC client
./client
```

---

## Debugging

### Enable Verbose Logging

#### Go Client
```go
import "google.golang.org/grpc/grpclog"

func main() {
    grpclog.SetLoggerV2(grpclog.NewLoggerV2(os.Stdout, os.Stdout, os.Stderr))
    // ... rest of code
}
```

#### Pascal Server
The Pascal server already logs requests when they arrive.

### Common Issues

#### Issue 1: "context deadline exceeded"
**Cause:** Server not responding or network issue  
**Solution:** Check if Pascal server is running on correct port

```bash
lsof -i :50051  # Check if port is in use
```

#### Issue 2: "connection refused"
**Cause:** Server not started  
**Solution:** Start Pascal server first

```bash
./ServiceServer
# Wait for "READY" message
```

#### Issue 3: "http2: frame too large"
**Cause:** Go client using HTTP/2, Pascal expects gRPC-Web  
**Solution:** Use Option 1 (gRPC-Web client) or Option 2 (Envoy proxy)

### Verify Protocol with tcpdump

```bash
# Capture traffic
sudo tcpdump -i lo0 -w capture.pcap port 50051

# In Wireshark, filter by:
http.request.method == "POST"
```

---

## Performance Considerations

### HTTP/1.1 vs HTTP/2

- **HTTP/1.1 (gRPC-Web):**
  - Single request/response per connection
  - Simpler protocol
  - Better proxy compatibility
  
- **HTTP/2 (via Envoy):**
  - Multiplexing support
  - Lower latency
  - Better performance under load

### Recommendation

- **Development/Testing:** Use direct gRPC-Web (Option 1)
- **Production:** Use Envoy proxy (Option 2) for better performance

---

## Example: Complete Go Client Project

```go
// main.go
package main

import (
    "context"
    "flag"
    "fmt"
    "log"
    "time"

    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials/insecure"
    pb "./pb"
)

var (
    serverAddr = flag.String("server", "localhost:8080", "Server address (Envoy proxy)")
    name       = flag.String("name", "World", "Name to greet")
)

func main() {
    flag.Parse()

    // Set up connection
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()

    conn, err := grpc.DialContext(
        ctx,
        *serverAddr,
        grpc.WithTransportCredentials(insecure.NewCredentials()),
        grpc.WithBlock(),
    )
    if err != nil {
        log.Fatalf("Failed to connect to %s: %v", *serverAddr, err)
    }
    defer conn.Close()

    fmt.Printf("Connected to server at %s\n", *serverAddr)

    // Create client
    client := pb.NewGreeterServiceClient(conn)

    // Make multiple requests
    for i := 1; i <= 3; i++ {
        req := &pb.HelloRequest{
            Name: fmt.Sprintf("%s-%d", *name, i),
        }

        resp, err := client.SayHello(context.Background(), req)
        if err != nil {
            log.Printf("[%d] Error: %v", i, err)
            continue
        }

        fmt.Printf("[%d] Response: %s\n", i, resp.Message)
        time.Sleep(500 * time.Millisecond)
    }
}
```

### Usage

```bash
# Via Envoy (default)
./client --server localhost:8080 --name "GoApp"

# Direct to Pascal (if using gRPC-Web)
./client --server localhost:50051 --name "GoApp"
```

---

## Next Steps

1. **Test basic communication**: Start with the examples above
2. **Add error handling**: Implement retries and timeout handling
3. **Add authentication**: Use gRPC metadata for auth tokens
4. **Monitor performance**: Add metrics and logging
5. **Deploy to production**: Use Envoy for robust HTTP/2 ↔ gRPC-Web translation

---

## References

- [gRPC-Web Specification](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-WEB.md)
- [Envoy gRPC-Web Filter](https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/grpc_web_filter)
- [grpc-web Go Package](https://github.com/improbable-eng/grpc-web)
- [Pascal gRPC-Web Implementation](./Samples/)

---

## Support

For issues with:
- **Pascal server**: Check `modules/PB2PAS/Samples/ServiceServer.lpr`
- **Protocol details**: See `GRPC_WEB_IMPLEMENTATION.md`
- **Testing**: Run `./test-interop.sh` in Samples directory
