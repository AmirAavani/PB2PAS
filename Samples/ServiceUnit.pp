unit ServiceUnit;
{$Mode objfpc}

interface

uses
 classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit, GRPCClientUnit;

type
  THelloRequest = class;
  THelloReply = class;

  // message HelloRequest
  { THelloRequest }
  THelloRequest = class(TBaseMessage)
  // Forward Declarations.

  private
    FName: AnsiString;

  public
    function GetName: AnsiString;

  public
    // string name = 1;
    property Name: AnsiString read FName write FName;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
  end;

  // message HelloReply
  { THelloReply }
  THelloReply = class(TBaseMessage)
  // Forward Declarations.

  private
    FMessage: AnsiString;

  public
    function GetMessage: AnsiString;

  public
    // string message = 1;
    property Message: AnsiString read FMessage write FMessage;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
  end;

  // Client For Service GreeterService
  { TGreeterServiceClient }
  TGreeterServiceClient = class(TGRPCClient)
  public
    function SayHello(Request: THelloRequest): THelloReply;
    function ListHellos(Request: THelloRequest): THelloReply;
  end;


implementation

function THelloRequest.GetName: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FName; 

end;


constructor THelloRequest.Create;
begin
  inherited Create;


end;


destructor THelloRequest.Destroy;
begin

  inherited;
end;

procedure THelloRequest.Clear;
begin
  FName := '';

  inherited;
end;

procedure THelloRequest.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Name, 1);

end;


function THelloRequest.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Name := LoadString(Stream);


    else
      SkipField(Stream, WireType);
    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;


function THelloReply.GetMessage: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FMessage; 

end;


constructor THelloReply.Create;
begin
  inherited Create;


end;


destructor THelloReply.Destroy;
begin

  inherited;
end;

procedure THelloReply.Clear;
begin
  FMessage := '';

  inherited;
end;

procedure THelloReply.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Message, 1);

end;


function THelloReply.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Message := LoadString(Stream);


    else
      SkipField(Stream, WireType);
    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;


function TGreeterServiceClient.SayHello(Request: THelloRequest): THelloReply;
begin
	WriteLn(1);
  Result := THelloReply.Create;
	WriteLn(2);
  if not Self.CallUnary('SayHello', Request, Result) then
  begin
    FreeAndNil(Result);
   end;
	WriteLn(3);
end;
function TGreeterServiceClient.ListHellos(Request: THelloRequest): THelloReply;
begin
  Result := THelloReply.Create;
  if not Self.CallUnary('ListHellos', Request, Result) then
  begin
    FreeAndNil(Result);
   end;
end;


end.
