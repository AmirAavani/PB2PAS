program ServiceServer;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils,
  ServiceUnit, // Generated from service.proto
  GRPCServerUnit, GRPCServerConfigUnit;

type
  { TMyGreeterServiceImpl - Implementation of the greeter service }
  TMyGreeterServiceImpl = class(TGreeterServiceServer)
  protected
    function SayHello(Request: THelloRequest): THelloReply; override;
    function ListHellos(Request: THelloRequest): THelloReply; override;
  public
    function GetServiceName: AnsiString; override;
  end;

{ TMyGreeterServiceImpl }

function TMyGreeterServiceImpl.SayHello(Request: THelloRequest): THelloReply;
begin
  Result := THelloReply.Create;
  Result.Message := Format('Hello, %s!', [Request.Name]);
  WriteLn('[SayHello] Request from: ', Request.Name);
  WriteLn('[SayHello] Response: ', Result.Message);
end;

function TMyGreeterServiceImpl.ListHellos(Request: THelloRequest): THelloReply;
begin
  Result := THelloReply.Create;
  Result.Message := Format('Greetings to %s! (streaming not yet implemented)', [Request.Name]);
  WriteLn('[ListHellos] Request from: ', Request.Name);
  WriteLn('[ListHellos] Response: ', Result.Message);
end;

function TMyGreeterServiceImpl.GetServiceName: AnsiString;
begin
  Result := 'greeter.GreeterService';
end;

var
  Server: TGRPCServer;
  GreeterService: TMyGreeterServiceImpl;
  InputLine: string;
  
begin
  WriteLn('=== gRPC Server Example ===');
  WriteLn;
  
  try
    // Create server
    Server := TGRPCServer.CreateSimple(50051);
    WriteLn('Server created on port 50051');
    
    // Create and register service
    GreeterService := TMyGreeterServiceImpl.Create;
    Server.RegisterService('greeter.GreeterService', GreeterService);
    WriteLn('Registered service: greeter.GreeterService');
    WriteLn;
    
    // Start server
    Server.Start;
    WriteLn('Server is running...');
    WriteLn('READY');
    Flush(Output);
    WriteLn('Send "quit" or press Ctrl+C to stop');
    WriteLn;
    
    // Wait for quit command or EOF
    repeat
      try
        ReadLn(InputLine);
        if (InputLine = 'quit') or (InputLine = 'stop') then
          Break;
      except
        // EOF reached (pipe closed or Ctrl+D) - keep running
        Sleep(1000);
    end;
    until False;
    
    // Stop server
    WriteLn;
    WriteLn('Stopping server...');
    Server.Stop;
    
    // Cleanup
    GreeterService.Free;
    Server.Free;
    
    WriteLn('Server stopped successfully');
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
