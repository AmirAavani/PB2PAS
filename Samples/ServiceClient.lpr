{$mode objfpc}{$H+}
program ServiceClient;
uses
  cthreads,
  Classes, SysUtils,
  ServiceUnit,
  GRPCClientUnit, GRPCClientConfigUnit;

var
  Client: TGreeterServiceClient;
  Request: THelloRequest;
  Response: THelloReply;
  Config: TGRPCClientConfig;
  i: Integer;
  ServerHost: string;
  ServerPort: Integer;
  
begin
  WriteLn('=== gRPC Client Example ===');
  WriteLn;
  
  // Check for server address parameter
  ServerHost := 'localhost';
  ServerPort := 50051;
  
  if ParamCount > 0 then
  begin
    // Parse server address (format: host:port)
    if Pos(':', ParamStr(1)) > 0 then
    begin
      ServerHost := Copy(ParamStr(1), 1, Pos(':', ParamStr(1)) - 1);
      ServerPort := StrToIntDef(Copy(ParamStr(1), Pos(':', ParamStr(1)) + 1, 100), 50051);
    end;
  end;
  
  try
    // Create client configuration
    Config := TGRPCClientConfig.Create;
    Config.Host := ServerHost;
    Config.Port := ServerPort;
    Config.Timeout := 5000; // 5 seconds
    
    // Create client
    Client := TGreeterServiceClient.Create(Config, 'greeter.GreeterService');
    WriteLn(Format('Connected to server at %s:%d', [ServerHost, ServerPort]));
    WriteLn;
    
    // Test 1: SayHello
    WriteLn('--- Test 1: SayHello ---');
    Request := THelloRequest.Create;
    Request.Name := 'World';
    WriteLn('Sending request with name: ', Request.Name);
    
    Response := Client.SayHello(Request);
    
    if Response <> nil then
    begin
      WriteLn('Received response: ', Response.Message);
      Response.Free;
    end
    else
    begin
      WriteLn('Error: ', Client.LastStatusMessage);
    end;
    
    Request.Free;
    WriteLn;
    
    // Test 2: ListHellos
    WriteLn('--- Test 2: ListHellos ---');
    Request := THelloRequest.Create;
    Request.Name := 'Pascal Programmer';
    WriteLn('Sending request with name: ', Request.Name);
    
    Response := Client.ListHellos(Request);
    
    if Response <> nil then
    begin
      WriteLn('Received response: ', Response.Message);
      Response.Free;
    end
    else
    begin
      WriteLn('Error: ', Client.LastStatusMessage);
    end;
    
    Request.Free;
    WriteLn;
    
    // Test 3: Multiple requests
    WriteLn('--- Test 3: Multiple Requests ---');
    WriteLn('Sending 5 requests...');
    
    Request := THelloRequest.Create;
    
    for i := 1 to 5 do
    begin
      Request.Name := Format('User%d', [i]);
      Response := Client.SayHello(Request);
      
      if Response <> nil then
      begin
        WriteLn(Format('[%d] %s', [i, Response.Message]));
        Response.Free;
      end
      else
      begin
        WriteLn(Format('[%d] Error: %s', [i, Client.LastStatusMessage]));
      end;
    end;
    
    Request.Free;
    WriteLn;
    
    // Cleanup
    Client.Free;
    Config.Free;
    
    WriteLn('=== Client Completed Successfully ===');
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
