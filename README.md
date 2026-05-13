# PB2PAS - Pascal Protocol Buffer Compiler

PB2PAS is a protocol buffer compiler that compiles protobufs to Pascal, including **gRPC-Web** service support. This project is under development.

## Features

- ✅ Full Protocol Buffers v3 support
- ✅ **gRPC-Web services** (client and server)
- ✅ Message definitions with nested types
- ✅ Enums and oneofs
- ✅ Maps and repeated fields
- ✅ Import support
- ✅ Package namespacing

## gRPC-Web Support

PB2PAS generates **gRPC-Web** compatible code for both clients and servers. This allows your Pascal services to:
- ✅ Communicate with other Pascal applications
- ✅ Be called from web browsers
- ✅ Work with standard HTTP/1.1 infrastructure
- ⚠️ Interoperate with Go/Python/etc using a gRPC-Web proxy

**📖 See [GRPC_WEB_PROTOCOL.md](GRPC_WEB_PROTOCOL.md) for full documentation on:**
- Protocol details and framing
- Interoperability with other languages
- Usage examples
- Debugging tips

I am making my contributions/submissions to this project solely in my personal capacity and am not conveying any rights to any intellectual property of any third parties.

# Usage

1) Obtain a copy of the repo and run 
    > git submodule init

    > git submodule update

2) Build __PB2PAS__
    > fpc -Sd PB2PAS.lpr  -Fumodules/PB2PAS -Fumodules/General-Purpose-Units -Fumodules/ALogger -Fumodules/Unicode-Functions -Fumodules/Threading

3) Running __PB2PAS__
    > ls Samples/

    > Main.pp  test_dep1.proto  test.proto 

    > ./PB2PAS --InputFile Samples/test.proto

    > ls Samples/

    > Main.pp  test_dep1.proto  TestDep1Unit.pp  test.proto  TestUnit.pp 

    > fpc -Sd Samples/Main.pp -Fumodules/PB2PAS
    > ./Samples/Main
