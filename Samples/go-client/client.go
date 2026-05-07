package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

func main() {
	// Default to Pascal server on 50051
	serverAddr := "localhost:50051"
	if len(os.Args) > 1 {
		serverAddr = os.Args[1]
	}

	// Connect to the server
	conn, err := grpc.Dial(serverAddr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("Failed to connect: %v", err)
	}
	defer conn.Close()

	client := NewGreeterServiceClient(conn)

	log.Println("=== Go gRPC Client ===")
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
