package main

import (
	"context"
	"fmt"
	"log"
	"net"

	"google.golang.org/grpc"
)

func main() {
	port := 50051

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		log.Fatalf("Failed to listen: %v", err)
	}

	s := grpc.NewServer()
	RegisterGreeterServiceServer(s, &server{})

	log.Printf("=== Go gRPC Server ===")
	log.Printf("Server listening on port %d", port)
	log.Printf("Press Ctrl+C to stop")
	log.Println()

	if err := s.Serve(lis); err != nil {
		log.Fatalf("Failed to serve: %v", err)
	}
}

// Server implements the GreeterService
type server struct {
	UnimplementedGreeterServiceServer
}

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
