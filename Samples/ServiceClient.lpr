{$mode objfpc}
program ServiceClient;
uses
  ServiceUnit;

var
	Client: TGreeterServiceClient;
	Request: THelloRequest;
	Response: THelloReply;

begin
  Client := TGreeterServiceClient.CreateSimple('localhost', 8080, 'GreeterService');
  try
    Request := THelloRequest.Create;
    try
      Request.Name := 'Hello World';
      WriteLn('Calling SayHello...');
      Response := Client.SayHello(Request);
      
      if Response <> nil then
      begin
        WriteLn('Response: ', Response.Message);
        Response.Free;
      end
      else
      begin
        WriteLn('Error: ', Client.LastStatusMessage);
        WriteLn('Status Code: ', Client.LastStatusCode);
      end;
    finally
      Request.Free;
    end;
  finally
    Client.Free;
  end;
end.
