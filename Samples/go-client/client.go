package main

import (
"bytes"
"context"
"encoding/binary"
"fmt"
"io"
"log"
"net/http"
"os"
"strings"
"time"

"google.golang.org/protobuf/proto"
)

func main() {
// Default to Pascal server on 50051
serverAddr := "localhost:50051"
if len(os.Args) > 1 {
serverAddr = os.Args[1]
}

// Parse server address
if !strings.HasPrefix(serverAddr, "http://") && !strings.HasPrefix(serverAddr, "https://") {
serverAddr = "http://" + serverAddr
}

client := &grpcWebClient{
baseURL:    serverAddr,
httpClient: &http.Client{Timeout: 10 * time.Second},
}

log.Println("=== Go gRPC-Web Client ===")
log.Printf("Connected to server at %s\n", serverAddr)
log.Println()

ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()

// Test 1: SayHello
log.Println("--- Test 1: SayHello ---")
req1 := &HelloRequest{Name: "Go Client"}
log.Printf("Sending request with name: %s", req1.Name)

resp1, err := client.SayHello(ctx, req1)
if err != nil {
log.Printf("Error: %v", err)
} else {
log.Printf("Received response: %s", resp1.GetMessage())
}
log.Println()

// Test 2: ListHellos
log.Println("--- Test 2: ListHellos ---")
req2 := &HelloRequest{Name: "Go Gopher"}
log.Printf("Sending request with name: %s", req2.Name)

resp2, err := client.ListHellos(ctx, req2)
if err != nil {
log.Printf("Error: %v", err)
} else {
log.Printf("Received response: %s", resp2.GetMessage())
}
log.Println()

// Test 3: Multiple requests
log.Println("--- Test 3: Multiple Requests ---")
log.Println("Sending 5 requests...")

for i := 1; i <= 5; i++ {
req := &HelloRequest{Name: fmt.Sprintf("GoUser%d", i)}
resp, err := client.SayHello(ctx, req)
if err != nil {
log.Printf("[%d] Error: %v", i, err)
} else {
log.Printf("[%d] %s", i, resp.GetMessage())
}
}
log.Println()

log.Println("=== Client Completed Successfully ===")
}

// grpcWebClient implements gRPC-Web protocol
type grpcWebClient struct {
baseURL    string
httpClient *http.Client
}

// SayHello calls the SayHello RPC method using gRPC-Web
func (c *grpcWebClient) SayHello(ctx context.Context, req *HelloRequest) (*HelloReply, error) {
return c.callUnary(ctx, "greeter.GreeterService/SayHello", req, &HelloReply{})
}

// ListHellos calls the ListHellos RPC method using gRPC-Web
func (c *grpcWebClient) ListHellos(ctx context.Context, req *HelloRequest) (*HelloReply, error) {
return c.callUnary(ctx, "greeter.GreeterService/ListHellos", req, &HelloReply{})
}

// callUnary performs a unary RPC call using gRPC-Web protocol
func (c *grpcWebClient) callUnary(ctx context.Context, method string, req proto.Message, resp *HelloReply) (*HelloReply, error) {
// Serialize request
reqData, err := proto.Marshal(req)
if err != nil {
return nil, fmt.Errorf("failed to marshal request: %w", err)
}

// Frame the message (5-byte header + data)
framed := frameMessage(reqData)

// Build URL
url := c.baseURL + "/" + method

// Create HTTP request
httpReq, err := http.NewRequestWithContext(ctx, "POST", url, bytes.NewReader(framed))
if err != nil {
return nil, fmt.Errorf("failed to create request: %w", err)
}

// Add gRPC-Web headers
httpReq.Header.Set("Content-Type", "application/grpc-web+proto")
httpReq.Header.Set("X-Grpc-Web", "1")
httpReq.Header.Set("X-User-Agent", "grpc-web-go/1.0")

// Send request
httpResp, err := c.httpClient.Do(httpReq)
if err != nil {
return nil, fmt.Errorf("failed to send request: %w", err)
}
defer httpResp.Body.Close()

if httpResp.StatusCode != http.StatusOK {
body, _ := io.ReadAll(httpResp.Body)
return nil, fmt.Errorf("HTTP error %d: %s", httpResp.StatusCode, string(body))
}

// Read response body
respBody, err := io.ReadAll(httpResp.Body)
if err != nil {
return nil, fmt.Errorf("failed to read response: %w", err)
}

// Extract response data from gRPC-Web frames
respData, err := extractGRPCWebResponse(respBody)
if err != nil {
return nil, fmt.Errorf("failed to extract response: %w", err)
}

// Unmarshal response
if err := proto.Unmarshal(respData, resp); err != nil {
return nil, fmt.Errorf("failed to unmarshal response: %w", err)
}

return resp, nil
}

// frameMessage adds gRPC-Web framing to a message
func frameMessage(data []byte) []byte {
framed := make([]byte, 5+len(data))
framed[0] = 0 // Compression flag
binary.BigEndian.PutUint32(framed[1:5], uint32(len(data)))
copy(framed[5:], data)
return framed
}

// extractGRPCWebResponse extracts the response data from gRPC-Web frames
func extractGRPCWebResponse(body []byte) ([]byte, error) {
if len(body) < 5 {
return nil, fmt.Errorf("response too short")
}

// Read first frame (data frame)
flags := body[0]
msgLen := binary.BigEndian.Uint32(body[1:5])

if (flags & 0x80) != 0 {
// This is a trailer frame, no data
return nil, fmt.Errorf("received trailer frame without data")
}

if len(body) < 5+int(msgLen) {
return nil, fmt.Errorf("incomplete message")
}

// Extract message data
return body[5 : 5+msgLen], nil
}
