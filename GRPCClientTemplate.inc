unit MyGrpcClient;

interface

uses
  SysUtils, Classes, Sockets, // Basic FPC units
  MyService_pb; // Your generated ProtoBuf Pascal code

type
  TMyGrpcClient = class
  private
    FConnection: TSocket;
  public
    constructor Create(ServerAddress: string; ServerPort: Integer);
    destructor Destroy; override;
    function Greet(Request: TMyRequest): TMyResponse;
  end;

implementation

constructor TMyGrpcClient.Create(ServerAddress: string; ServerPort: Integer);
begin
  FConnection := SocketCreate(AF_INET, SOCK_STREAM, 0);
  // ... Connect to the server ...
end;

destructor TMyGrpcClient.Destroy;
begin
  // ... Close the connection ...
  SocketClose(FConnection);
  inherited Destroy;
end;

function TMyGrpcClient.Greet(Request: TMyRequest): TMyResponse;
var
  RequestBytes: TBytes;
  ResponseBytes: TBytes;
begin
  // ... Serialize the request ...
  RequestBytes := SerializeMyRequest(Request);

  // ... Send the request using your HTTP/2 or simplified mechanism ...
  // ... Receive the response ...

  // ... Deserialize the response ...
  Result := DeserializeMyResponse(ResponseBytes);
end;

end.
