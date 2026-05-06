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
  public type
    TMetadata = class;

  public type
    // message Metadata
    { TMetadata }
    TMetadata = class(TBaseMessage)
    // Forward Declarations.

    private
      FKey: AnsiString;

    public
      function GetKey: AnsiString;

    public
      // string key = 1;
      property Key: AnsiString read FKey write FKey;

    private
      FValue: AnsiString;

    public
      function GetValue: AnsiString;

    public
      // string value = 2;
      property Value: AnsiString read FValue write FValue;

    private
      FCount: Int32;

    public
      function GetCount: Int32;

    public
      // int32 count = 3;
      property Count: Int32 read FCount write FCount;

    protected 
      procedure SaveToStream(Stream: TProtoStreamWriter); override;
      function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear; override;

    public // functions
    end;

  private
    FMessage: AnsiString;

  public
    function GetMessage: AnsiString;

  public
    // string message = 1;
    property Message: AnsiString read FMessage write FMessage;

  private
    FInt32Value: Int32;

  public
    function GetInt32Value: Int32;

  public
    // int32 int32_value = 2;
    property Int32Value: Int32 read FInt32Value write FInt32Value;

  private
    FInt64Value: Int64;

  public
    function GetInt64Value: Int64;

  public
    // int64 int64_value = 3;
    property Int64Value: Int64 read FInt64Value write FInt64Value;

  private
    FUint32Value: UInt32;

  public
    function GetUint32Value: UInt32;

  public
    // uint32 uint32_value = 4;
    property Uint32Value: UInt32 read FUint32Value write FUint32Value;

  private
    FUint64Value: UInt64;

  public
    function GetUint64Value: UInt64;

  public
    // uint64 uint64_value = 5;
    property Uint64Value: UInt64 read FUint64Value write FUint64Value;

  private
    FSint32Value: Int32;

  public
    function GetSint32Value: Int32;

  public
    // sint32 sint32_value = 6;
    property Sint32Value: Int32 read FSint32Value write FSint32Value;

  private
    FSint64Value: Int64;

  public
    function GetSint64Value: Int64;

  public
    // sint64 sint64_value = 7;
    property Sint64Value: Int64 read FSint64Value write FSint64Value;

  private
    FFixed32Value: UInt32;

  public
    function GetFixed32Value: UInt32;

  public
    // fixed32 fixed32_value = 8;
    property Fixed32Value: UInt32 read FFixed32Value write FFixed32Value;

  private
    FFixed64Value: UInt64;

  public
    function GetFixed64Value: UInt64;

  public
    // fixed64 fixed64_value = 9;
    property Fixed64Value: UInt64 read FFixed64Value write FFixed64Value;

  private
    FSfixed32Value: Int32;

  public
    function GetSfixed32Value: Int32;

  public
    // sfixed32 sfixed32_value = 10;
    property Sfixed32Value: Int32 read FSfixed32Value write FSfixed32Value;

  private
    FSfixed64Value: Int64;

  public
    function GetSfixed64Value: Int64;

  public
    // sfixed64 sfixed64_value = 11;
    property Sfixed64Value: Int64 read FSfixed64Value write FSfixed64Value;

  private
    FFloatValue: Single;

  public
    function GetFloatValue: Single;

  public
    // float float_value = 12;
    property FloatValue: Single read FFloatValue write FFloatValue;

  private
    FDoubleValue: Double;

  public
    function GetDoubleValue: Double;

  public
    // double double_value = 13;
    property DoubleValue: Double read FDoubleValue write FDoubleValue;

  private
    FBoolValue: Boolean;

  public
    function GetBoolValue: Boolean;

  public
    // bool bool_value = 14;
    property BoolValue: Boolean read FBoolValue write FBoolValue;

  private
    FBytesValue: AnsiString;

  public
    function GetBytesValue: AnsiString;

  public
    // string bytes_value = 15;
    property BytesValue: AnsiString read FBytesValue write FBytesValue;

  public type
    TRepeatedInts =  TInt32s;

  private
    FRepeatedInts: TRepeatedInts;


  public
    function GetRepeatedInts: TRepeatedInts;
    function GetOrCreateRepeatedInts: TRepeatedInts;

  public
    // repeated int32 repeated_ints = 16;
    property RepeatedInts: TRepeatedInts read FRepeatedInts write FRepeatedInts;
    property ConstRepeatedInts: TRepeatedInts read GetRepeatedInts;
    property MutableRepeatedInts: TRepeatedInts read GetOrCreateRepeatedInts write FRepeatedInts;

  public type
    TRepeatedStrings =  TAnsiStrings;

  private
    FRepeatedStrings: TRepeatedStrings;


  public
    function GetRepeatedStrings: TRepeatedStrings;
    function GetOrCreateRepeatedStrings: TRepeatedStrings;

  public
    // repeated string repeated_strings = 17;
    property RepeatedStrings: TRepeatedStrings read FRepeatedStrings write FRepeatedStrings;
    property ConstRepeatedStrings: TRepeatedStrings read GetRepeatedStrings;
    property MutableRepeatedStrings: TRepeatedStrings read GetOrCreateRepeatedStrings write FRepeatedStrings;

  private
    FMetadata: TMetadata;

  public
    function GetMetadata: TMetadata;
    function GetOrCreateMetadata: TMetadata;

  public
    // Metadata metadata = 18;
    property Metadata: TMetadata read FMetadata write FMetadata;
    property ConstMetadata: TMetadata read GetMetadata;
    property MutableMetadata: TMetadata read GetOrCreateMetadata;

  public type
    TRepeatedMetadata = specialize TObjectList<TMetadata>;

  private
    FRepeatedMetadata: TRepeatedMetadata;

  public
    function GetRepeatedMetadata: TRepeatedMetadata;
    function GetOrCreateRepeatedMetadata: TRepeatedMetadata;

  public
    // repeated Metadata repeated_metadata = 19;
    property RepeatedMetadata: TRepeatedMetadata read FRepeatedMetadata write FRepeatedMetadata;
    property ConstRepeatedMetadata: TRepeatedMetadata read GetRepeatedMetadata;
    property MutableRepeatedMetadata: TRepeatedMetadata read GetOrCreateRepeatedMetadata write FRepeatedMetadata;

  private
    FTimestampMs: Int64;

  public
    function GetTimestampMs: Int64;

  public
    // int64 timestamp_ms = 20;
    property TimestampMs: Int64 read FTimestampMs write FTimestampMs;

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


function THelloReply.TMetadata.GetKey: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FKey; 

end;


function THelloReply.TMetadata.GetValue: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FValue; 

end;


function THelloReply.TMetadata.GetCount: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FCount; 

end;


constructor THelloReply.TMetadata.Create;
begin
  inherited Create;


end;


destructor THelloReply.TMetadata.Destroy;
begin

  inherited;
end;

procedure THelloReply.TMetadata.Clear;
begin
  FKey := '';
  FValue := '';
  FCount := 0;

  inherited;
end;

procedure THelloReply.TMetadata.SaveToStream(Stream: TProtoStreamWriter);
begin
    SaveString(Stream, Key, 1);

    SaveString(Stream, Value, 2);

    SaveInt32(Stream, Count, 3);

end;


function THelloReply.TMetadata.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
      1:
        Key := LoadString(Stream);

      2:
        Value := LoadString(Stream);

      3:
        Count := LoadInt32(Stream);


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


function THelloReply.GetInt32Value: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FInt32Value; 

end;


function THelloReply.GetInt64Value: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FInt64Value; 

end;


function THelloReply.GetUint32Value: UInt32;
begin
  if Self = nil then
    Exit(0);

  Result := FUint32Value; 

end;


function THelloReply.GetUint64Value: UInt64;
begin
  if Self = nil then
    Exit(0);

  Result := FUint64Value; 

end;


function THelloReply.GetSint32Value: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FSint32Value; 

end;


function THelloReply.GetSint64Value: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FSint64Value; 

end;


function THelloReply.GetFixed32Value: UInt32;
begin
  if Self = nil then
    Exit(0);

  Result := FFixed32Value; 

end;


function THelloReply.GetFixed64Value: UInt64;
begin
  if Self = nil then
    Exit(0);

  Result := FFixed64Value; 

end;


function THelloReply.GetSfixed32Value: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FSfixed32Value; 

end;


function THelloReply.GetSfixed64Value: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FSfixed64Value; 

end;


function THelloReply.GetFloatValue: Single;
begin
  if Self = nil then
    Exit(0);

  Result := FFloatValue; 

end;


function THelloReply.GetDoubleValue: Double;
begin
  if Self = nil then
    Exit(0);

  Result := FDoubleValue; 

end;


function THelloReply.GetBoolValue: Boolean;
begin
  if Self = nil then
    Exit(False);

  Result := FBoolValue; 

end;


function THelloReply.GetBytesValue: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FBytesValue; 

end;



function THelloReply.GetRepeatedInts: TRepeatedInts;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepeatedInts; 

end;

function THelloReply.GetOrCreateRepeatedInts: TRepeatedInts;

begin
  if Self = nil then
    Exit(nil);

  if FRepeatedInts = nil then
    FRepeatedInts := TRepeatedInts.Create;
  Result := FRepeatedInts;

end;


function THelloReply.GetRepeatedStrings: TRepeatedStrings;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepeatedStrings; 

end;

function THelloReply.GetOrCreateRepeatedStrings: TRepeatedStrings;

begin
  if Self = nil then
    Exit(nil);

  if FRepeatedStrings = nil then
    FRepeatedStrings := TRepeatedStrings.Create;
  Result := FRepeatedStrings;

end;

function THelloReply.GetMetadata: TMetadata;
begin
  if Self = nil then
    Exit(nil);

  Result := FMetadata; 

end;

function THelloReply.GetOrCreateMetadata: TMetadata;
begin
  if Self = nil then
    Exit(nil);

  if Self.FMetadata = nil then
    FMetadata := TMetadata.Create;

  Result := FMetadata; 

end;



function THelloReply.GetRepeatedMetadata: TRepeatedMetadata;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepeatedMetadata; 

end;

function THelloReply.GetOrCreateRepeatedMetadata: TRepeatedMetadata;
begin
  if Self = nil then
    Exit(nil);

  if Self.FRepeatedMetadata = nil then
    FRepeatedMetadata := TRepeatedMetadata.Create;

  Result := FRepeatedMetadata; 

end;


function THelloReply.GetTimestampMs: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FTimestampMs; 

end;


constructor THelloReply.Create;
begin
  inherited Create;


end;


destructor THelloReply.Destroy;
begin
  FreeAndNil(FRepeatedInts);
  FreeAndNil(FRepeatedStrings);
  FreeAndNil(FMetadata);
  FreeAndNil(FRepeatedMetadata);

  inherited;
end;

procedure THelloReply.Clear;
begin
  FMessage := '';
  FInt32Value := 0;
  FInt64Value := 0;
  FUint32Value := 0;
  FUint64Value := 0;
  FSint32Value := 0;
  FSint64Value := 0;
  FFixed32Value := 0;
  FFixed64Value := 0;
  FSfixed32Value := 0;
  FSfixed64Value := 0;
  FFloatValue := 0;
  FDoubleValue := 0;
  FBoolValue := False;
  FBytesValue := '';
  if FRepeatedInts <> nil then
     RepeatedInts.Clear;
  if FRepeatedStrings <> nil then
     RepeatedStrings.Clear;
  if FMetadata <> nil then
     Metadata.Clear;
  if FRepeatedMetadata <> nil then
     RepeatedMetadata.Clear;
  FTimestampMs := 0;

  inherited;
end;

procedure THelloReply.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Message, 1);

  SaveInt32(Stream, Int32Value, 2);

  SaveInt64(Stream, Int64Value, 3);

  SaveUint32(Stream, Uint32Value, 4);

  SaveUint64(Stream, Uint64Value, 5);

  SaveSint32(Stream, Sint32Value, 6);

  SaveSint64(Stream, Sint64Value, 7);

  SaveFixed32(Stream, Fixed32Value, 8);

  SaveFixed64(Stream, Fixed64Value, 9);

  SaveSfixed32(Stream, Sfixed32Value, 10);

  SaveSfixed64(Stream, Sfixed64Value, 11);

  SaveFloat(Stream, FloatValue, 12);

  SaveDouble(Stream, DoubleValue, 13);

  SaveBool(Stream, BoolValue, 14);

  SaveString(Stream, BytesValue, 15);

  SaveRepeatedInt32(Stream, FRepeatedInts, 16);

  SaveRepeatedString(Stream, FRepeatedStrings, 17);

  SaveMessage(Stream, Metadata, 18);

  specialize SaveRepeatedMessage<TMetadata>(Stream, FRepeatedMetadata, 19);

  SaveInt64(Stream, TimestampMs, 20);

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

    2:
      Int32Value := LoadInt32(Stream);

    3:
      Int64Value := LoadInt64(Stream);

    4:
      Uint32Value := LoadUint32(Stream);

    5:
      Uint64Value := LoadUint64(Stream);

    6:
      Sint32Value := LoadSint32(Stream);

    7:
      Sint64Value := LoadSint64(Stream);

    8:
      Fixed32Value := LoadFixed32(Stream);

    9:
      Fixed64Value := LoadFixed64(Stream);

    10:
      Sfixed32Value := LoadSfixed32(Stream);

    11:
      Sfixed64Value := LoadSfixed64(Stream);

    12:
      FloatValue := LoadFloat(Stream);

    13:
      DoubleValue := LoadDouble(Stream);

    14:
      BoolValue := LoadBool(Stream);

    15:
      BytesValue := LoadString(Stream);

    16: 
      if not LoadRepeatedInt32(Stream, MutableRepeatedInts) then
        Exit(False);

    17: 
      if not LoadRepeatedString(Stream, MutableRepeatedStrings) then
        Exit(False);

    18:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableMetadata) then
        Exit(False);
    end;
    19: 
      if not (specialize LoadRepeatedMessage<TMetadata>(Stream, MutableRepeatedMetadata)) then
        Exit(False);

    20:
      TimestampMs := LoadInt64(Stream);


    else
      SkipField(Stream, WireType);
    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;


function TGreeterServiceClient.SayHello(Request: THelloRequest): THelloReply;
begin
  Result := THelloReply.Create;
  if not Self.CallUnary('SayHello', Request, Result) then
  begin
    FreeAndNil(Result);
   end;
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
