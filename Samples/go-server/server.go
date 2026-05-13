package main

import (
	"bytes"
	"context"
	"encoding/binary"
	"fmt"
	"io"
	"log"
	"net/http"
	"strings"

	"google.golang.org/protobuf/proto"
)

func main() {
	port := 50052

	// Create gRPC-Web HTTP handler
	handler := &grpcWebHandler{
		server: &server{},
	}

	log.Printf("=== Go gRPC-Web Server ===")
	log.Printf("Server listening on port %d", port)
	log.Printf("Protocol: gRPC-Web (HTTP/1.1)")
	log.Printf("Press Ctrl+C to stop")
	log.Println()

	if err := http.ListenAndServe(fmt.Sprintf(":%d", port), handler); err != nil {
		log.Fatalf("Failed to serve: %v", err)
	}
}

// Server implements the GreeterService
type server struct{}

// SayHello implements the SayHello RPC method
func (s *server) SayHello(ctx context.Context, req *HelloRequest) (*HelloReply, error) {
	log.Printf("[Go Server] SayHello called with: %s", req.GetName())
	reply := &HelloReply{
		Message: fmt.Sprintf("Hello from Go, %s!", req.GetName()),
	}
	log.Printf("[Go Server] Replying: %s", reply.GetMessage())
	return reply, nil
}

// ListHellos implements the ListHellos RPC method
func (s *server) ListHellos(ctx context.Context, req *HelloRequest) (*HelloReply, error) {
	log.Printf("[Go Server] ListHellos called with: %s", req.GetName())
	reply := &HelloReply{
		Message: fmt.Sprintf("Greetings from Go to %s! (streaming not yet implemented)", req.GetName()),
	}
	log.Printf("[Go Server] Replying: %s", reply.GetMessage())
	return reply, nil
}

// grpcWebHandler handles gRPC-Web HTTP requests
type grpcWebHandler struct {
	server *server
}

// ServeHTTP handles gRPC-Web requests
func (h *grpcWebHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	// Only accept POST
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Add CORS headers
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type, X-Grpc-Web, X-User-Agent")

	if r.Method == http.MethodOptions {
		w.WriteHeader(http.StatusOK)
		return
	}

	// Read request body
	body, err := io.ReadAll(r.Body)
	if err != nil {
		http.Error(w, "Failed to read request", http.StatusBadRequest)
		return
	}

	// Unframe gRPC-Web message
	reqData, err := unframeMessage(body)
	if err != nil {
		http.Error(w, fmt.Sprintf("Invalid gRPC frame: %v", err), http.StatusBadRequest)
		return
	}

	// Parse path to get method name
	method := strings.TrimPrefix(r.URL.Path, "/")
	
	var resp proto.Message
	var handleErr error

	// Route to appropriate handler
	if strings.HasSuffix(method, "SayHello") {
		req := &HelloRequest{}
		if err := proto.Unmarshal(reqData, req); err != nil {
			http.Error(w, "Failed to unmarshal request", http.StatusBadRequest)
			return
		}
		resp, handleErr = h.server.SayHello(r.Context(), req)
	} else if strings.HasSuffix(method, "ListHellos") {
		req := &HelloRequest{}
		if err := proto.Unmarshal(reqData, req); err != nil {
			http.Error(w, "Failed to unmarshal request", http.StatusBadRequest)
			return
		}
		resp, handleErr = h.server.ListHellos(r.Context(), req)
	} else {
		http.Error(w, "Method not found", http.StatusNotFound)
		return
	}

	if handleErr != nil {
		http.Error(w, handleErr.Error(), http.StatusInternalServerError)
		return
	}

	// Serialize response
	respData, err := proto.Marshal(resp)
	if err != nil {
		http.Error(w, "Failed to marshal response", http.StatusInternalServerError)
		return
	}

	// Build gRPC-Web response with data frame + trailer frame
	responseBody := buildGRPCWebResponse(respData)

	// Send response
	w.Header().Set("Content-Type", "application/grpc-web+proto")
	w.Header().Set("X-Grpc-Web", "1")
	w.WriteHeader(http.StatusOK)
	w.Write(responseBody)
}

// unframeMessage removes gRPC-Web framing from a message
func unframeMessage(framed []byte) ([]byte, error) {
	if len(framed) < 5 {
		return nil, fmt.Errorf("frame too short")
	}

	// Skip compression flag (byte 0)
	msgLen := binary.BigEndian.Uint32(framed[1:5])

	if len(framed) < 5+int(msgLen) {
		return nil, fmt.Errorf("incomplete message")
	}

	return framed[5 : 5+msgLen], nil
}

// buildGRPCWebResponse builds a gRPC-Web response with data and trailer frames
func buildGRPCWebResponse(data []byte) []byte {
	// Data frame: [compression flag][length][data]
	dataFrame := make([]byte, 5+len(data))
	dataFrame[0] = 0 // No compression
	binary.BigEndian.PutUint32(dataFrame[1:5], uint32(len(data)))
	copy(dataFrame[5:], data)

	// Trailer frame: [0x80][length][text trailers]
	trailerText := "grpc-status: 0\r\ngrpc-message: OK\r\n"
	trailerFrame := make([]byte, 5+len(trailerText))
	trailerFrame[0] = 0x80 // Trailer flag
	binary.BigEndian.PutUint32(trailerFrame[1:5], uint32(len(trailerText)))
	copy(trailerFrame[5:], []byte(trailerText))

	// Combine data frame + trailer frame
	var buf bytes.Buffer
	buf.Write(dataFrame)
	buf.Write(trailerFrame)
	return buf.Bytes()
}
