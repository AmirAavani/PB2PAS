# gRPC-Web Protocol Implementation

## Overview

This Pascal protobuf compiler (PB2PAS) generates **gRPC-Web** compatible code for both clients and servers. gRPC-Web is a protocol that allows gRPC services to be called from web browsers and environments that don't support HTTP/2 directly.

## Why gRPC-Web?

Standard gRPC uses HTTP/2 with specific framing that requires bidirectional streaming and complex connection management. gRPC-Web simplifies this by:

1. **Using HTTP/1.1** - Works with standard web servers and proxies
2. **Simpler framing** - Uses a straightforward binary framing protocol
3. **Broader compatibility** - Works from browsers, mobile apps, and any HTTP client
4. **Easier debugging** - Can be inspected with standard HTTP tools

## Protocol Details

### Request Format


