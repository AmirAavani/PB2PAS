#!/bin/bash

# Cross-compatibility test script
# Tests all combinations of Go and Pascal clients/servers
# Both now use gRPC-Web protocol, so all tests should PASS!

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "╔═══════════════════════════════════════════════════════════════════════════╗"
echo "║       gRPC Cross-Compatibility Test: Go ↔ Pascal                         ║"
echo "╚═══════════════════════════════════════════════════════════════════════════╝"
echo

# Function to kill background process
cleanup() {
    if [ ! -z "$SERVER_PID" ]; then
        echo "Stopping server (PID: $SERVER_PID)..."
        kill $SERVER_PID 2>/dev/null || true
        wait $SERVER_PID 2>/dev/null || true
    fi
}

trap cleanup EXIT

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run a test
run_test() {
    local TEST_NAME="$1"
    local SERVER_CMD="$2"
    local SERVER_NAME="$3"
    local CLIENT_CMD="$4"
    local CLIENT_NAME="$5"
    local SERVER_PORT="$6"
    local IS_PASCAL_SERVER="$7"
    
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}Test: $TEST_NAME${NC}"
    echo "Server: $SERVER_NAME"
    echo "Client: $CLIENT_NAME"
    echo
    
    # Start server in background
    echo "Starting $SERVER_NAME..."
    
    # Debug: Show exact command
    echo "Command: $SERVER_CMD"
    
    $SERVER_CMD > server-$TEST_NAME.log 2>&1 &
    SERVER_PID=$!
    
    echo "Server PID: $SERVER_PID"
    
    # Wait for server to be ready
    if [ "$IS_PASCAL_SERVER" = "yes" ]; then
        # Wait for Pascal server to print "READY"
        echo "Waiting for READY signal..."
        timeout=20
        while [ $timeout -gt 0 ]; do
            if grep -q "READY" server-$TEST_NAME.log 2>/dev/null; then
                echo "Server is READY!"
                break
            fi
            sleep 0.5
            timeout=$((timeout - 1))
            
            # Show what we have so far
            if [ $((timeout % 4)) -eq 0 ]; then
                echo "  Waiting... (${timeout} half-seconds left)"
                echo "  Server log so far:"
                cat server-$TEST_NAME.log | head -5 | sed 's/^/    /'
            fi
        done
        
        if [ $timeout -eq 0 ]; then
            echo -e "${RED}✗ FAILED: Server did not become ready in time${NC}"
            echo "Server log:"
            cat server-$TEST_NAME.log
            TESTS_FAILED=$((TESTS_FAILED + 1))
            kill $SERVER_PID 2>/dev/null || true
            SERVER_PID=""
            return 1
        fi
    else
        # For Go servers, just wait a bit
        sleep 2
    fi
    
    # Check if server is running
    if ! ps -p $SERVER_PID > /dev/null; then
        echo -e "${RED}✗ FAILED: Server failed to start${NC}"
        echo "Server log:"
        cat server-$TEST_NAME.log
        TESTS_FAILED=$((TESTS_FAILED + 1))
        SERVER_PID=""
        return 1
    fi
    
    echo "Server started (PID: $SERVER_PID)"
    echo
    
    # Run client
    echo "Running $CLIENT_NAME..."
    if $CLIENT_CMD > client-$TEST_NAME.log 2>&1; then
        echo -e "${GREEN}✓ PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        
        # Show sample output
        echo
        echo "Client output (first 10 lines):"
        head -10 client-$TEST_NAME.log | sed 's/^/  /'
        
    else
        echo -e "${RED}✗ FAILED: Client returned error${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo
        echo "Client log:"
        cat client-$TEST_NAME.log
        echo
        echo "Server log:"
        cat server-$TEST_NAME.log
    fi
    
    # Stop server
    echo
    echo "Stopping $SERVER_NAME..."
    kill $SERVER_PID 2>/dev/null || true
    wait $SERVER_PID 2>/dev/null || true
    SERVER_PID=""
    
    echo
}

# Ensure binaries exist
echo "Checking binaries..."
echo

if [ ! -f "./ServiceServer" ]; then
    echo -e "${RED}Error: Pascal ServiceServer not found${NC}"
    echo "Build it first: fpc -Sd ServiceServer.lpr -Fu./ -Fu../modules/PB2PAS" -FU/tmp
    exit 1
fi

if [ ! -f "./ServiceClient" ]; then
    echo -e "${RED}Error: Pascal ServiceClient not found${NC}"
    echo "Build it first: fpc -Sd ServiceClient.lpr -Fu./ -Fu../modules/PB2PAS" -Fu/tmp
    exit 1
fi

if [ ! -f "./go-server/greeter-server" ]; then
    echo -e "${RED}Error: Go server not found${NC}"
    echo "Build it first: ./setup-go.sh"
    exit 1
fi

if [ ! -f "./go-client/greeter-client" ]; then
    echo -e "${RED}Error: Go client not found${NC}"
    echo "Build it first: ./setup-go.sh"
    exit 1
fi

echo -e "${GREEN}✓ All binaries found${NC}"
echo
echo

# Test 1: Pascal Client → Pascal Server (Baseline)
run_test "pascal-to-pascal" \
    "./ServiceServer" \
    "Pascal Server (port 50051)" \
    "./ServiceClient" \
    "Pascal Client" \
    "50051" \
    "yes"

# Test 2: Go Client → Go Server (Baseline)
run_test "go-to-go" \
    "./go-server/greeter-server" \
    "Go Server (port 50052)" \
    "./go-client/greeter-client localhost:50052" \
    "Go Client" \
    "50052" \
    "no"

# Test 3: Go Client → Pascal Server (SHOULD NOW WORK!)
run_test "go-to-pascal" \
    "./ServiceServer" \
    "Pascal Server (port 50051)" \
    "./go-client/greeter-client localhost:50051" \
    "Go Client" \
    "50051" \
    "yes"

# Test 4: Pascal Client → Go Server (SHOULD NOW WORK!)
run_test "pascal-to-go" \
    "./go-server/greeter-server" \
    "Go Server (port 50052)" \
    "./ServiceClient localhost:50052" \
    "Pascal Client" \
    "50052" \
    "no"

# Summary
echo
echo "╔═══════════════════════════════════════════════════════════════════════════╗"
echo "║                           Test Summary                                    ║"
echo "╚═══════════════════════════════════════════════════════════════════════════╝"
echo
echo -e "Tests Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests Failed: ${RED}$TESTS_FAILED${NC}"
echo
echo "Detailed logs saved:"
echo "  • server-*.log"
echo "  • client-*.log"
echo

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}╔═══════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║   ALL TESTS PASSED! ✓                        ║${NC}"
    echo -e "${GREEN}║   gRPC-Web works perfectly!                  ║${NC}"
    echo -e "${GREEN}║   Pascal ↔ Go interoperability confirmed!    ║${NC}"
    echo -e "${GREEN}╚═══════════════════════════════════════════════╝${NC}"
    exit 0
else
    echo -e "${RED}╔═══════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║   SOME TESTS FAILED ✗                        ║${NC}"
    echo -e "${RED}║   Check logs above for details               ║${NC}"
    echo -e "${RED}║                                               ║${NC}"
    echo -e "${RED}║   Make sure you ran: ./setup-go.sh           ║${NC}"
    echo -e "${RED}║   to rebuild with gRPC-Web support           ║${NC}"
    echo -e "${RED}╚═══════════════════════════════════════════════╝${NC}"
    exit 1
fi
