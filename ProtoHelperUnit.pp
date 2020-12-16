unit ProtoHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ProtoHelperListsUnit, ProtoStreamUnit;

type

  { TBytes }

  TBytes = class(specialize TSimpleTypeList<Byte>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: Byte): AnsiString; override;

  end;

  { TSingles }

  TSingles = class(specialize TSimpleTypeList<Single>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: Single): AnsiString; override;

  end;

  { TDoubles }

  TDoubles = class(specialize TSimpleTypeList<Double>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: Double): AnsiString; override;

  end;

  { TInt32s }

  TInt32s = class(specialize TSimpleTypeList<Int32>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: Int32): AnsiString; override;

  end;

  { TInt64s }

  TInt64s = class(specialize TSimpleTypeList<Int64>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: Int64): AnsiString; override;

  end;

  { TUInt32s }

  TUInt32s = class(specialize TSimpleTypeList<UInt32>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: UInt32): AnsiString; override;

  end;

  { TUInt64s }

  TUInt64s = class(specialize TSimpleTypeList<UInt64>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: UInt64): AnsiString; override;

  end;

  { TBooleans }

  TBooleans = class(specialize TSimpleTypeList<Boolean>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: Boolean): AnsiString; override;

  end;

  { TAnsiStrings }

  TAnsiStrings = class(specialize TSimpleTypeList<AnsiString>)
  public
    constructor Create;

  protected
    function SimpleObjectToString(Obj: AnsiString): AnsiString; override;

  end;

  TBaseMessage = class;
  TBaseOneOf = class;

  { TBaseMessage }

  TBaseMessage = class(TObject)
  private
    function ReadRepeatedLength(Stream: TProtoStreamReader): UInt32;

  protected

    procedure SaveToStream(Stream: TProtoStreamWriter);  virtual; abstract;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;  virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromString(Str: AnsiString): Boolean; virtual;
    function LoadFromStream(Stream: TStream): Boolean; virtual;
    procedure SaveToStream(Stream: TStream);  virtual;

  end;

  { TBaseOneOf }

  TBaseOneOf = class(TObject)
  protected
    _ObjectIndex: Integer;
    _Data: Pointer;

    function GetPointerByIndex(Index: Integer): Pointer;
    procedure SetPointerByIndex(Index: Integer; AValue: Pointer);

  public
    property PointerByIndex[Index: Integer]: Pointer read GetPointerByIndex write SetPointerByIndex;

    constructor Create;
    destructor Destroy; override;
  end;

  { EBaseOneOf }

  EBaseOneOf = class(Exception)
  public
    constructor CreateTwoValueAreSet;
  end;

procedure SaveSingle(Stream: TProtoStreamWriter;
    const Data: Single; const TagID: Integer);
procedure SaveDouble(Stream: TProtoStreamWriter;
    const Data: Double; const TagID: Integer);
procedure SaveInt32(Stream: TProtoStreamWriter;
    const Data: Int32; const TagID: Integer);
procedure SaveInt64(Stream: TProtoStreamWriter;
    const Data: Int64; const TagID: Integer);
procedure SaveUInt32(Stream: TProtoStreamWriter;
    const Data: UInt32; const TagID: Integer);
procedure SaveUInt64(Stream: TProtoStreamWriter;
    const Data: UInt64; const TagID: Integer);
procedure SaveAnsiString(Stream: TProtoStreamWriter; const Data: AnsiString;
  const TagID: Integer);
procedure SaveBoolean(Stream: TProtoStreamWriter; const Data: Boolean;
    const TagID: Integer);
procedure SaveByte(Stream: TProtoStreamWriter; const Data: Byte;
    const TagID: Integer);

function LoadSingle(Stream: TProtoStreamReader): Single;
function LoadDouble(Stream: TProtoStreamReader): Double;
function LoadInt32(Stream: TProtoStreamReader): Int32;
function LoadInt64(Stream: TProtoStreamReader): Int64;
function LoadUInt32(Stream: TProtoStreamReader): UInt32;
function LoadUInt64(Stream: TProtoStreamReader): UInt64;
function LoadAnsiString(Stream: TProtoStreamReader): AnsiString;
function LoadBoolean(Stream: TProtoStreamReader): Boolean;
function LoadByte(Stream: TProtoStreamReader): Byte;

procedure SaveRepeatedSingle(Stream: TProtoStreamWriter;
  const Data: TSingles;
  const TagID: Integer);
procedure SaveRepeatedDouble(Stream: TProtoStreamWriter;
    const Data: TDoubles;
    const TagID: Integer);
procedure SaveRepeatedInt32(Stream: TProtoStreamWriter;
    const Data: TInt32s;
    const TagID: Integer);
procedure SaveRepeatedInt64(Stream: TProtoStreamWriter;
    const Data: TInt64s;
    const TagID: Integer);
procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter;
    const Data: TUInt32s;
    const TagID: Integer);
procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter;
    const Data: TUInt64s;
    const TagID: Integer);
procedure SaveRepeatedAnsiString(
    Stream: TProtoStreamWriter;
   const Data: TAnsiStrings;
  const TagID: Integer);
procedure SaveRepeatedBoolean(Stream: TProtoStreamWriter;
    const Data: TBooleans;
    const TagID: Integer);
procedure SaveRepeatedByte(Stream: TProtoStreamWriter;
  const Data: TBytes;
  const TagID: Integer);

function LoadRepeatedSingle(Stream: TProtoStreamReader;
    Data: TSingles): Boolean;
function LoadRepeatedDouble(Stream: TProtoStreamReader;
    Data: TDoubles): Boolean;
function LoadRepeatedInt32(Stream: TProtoStreamReader;
    Data: TInt32s): Boolean;
function LoadRepeatedInt64(Stream: TProtoStreamReader;
    Data: TInt64s): Boolean;
function LoadRepeatedUInt32(Stream: TProtoStreamReader;
    Data: TUInt32s): Boolean;
function LoadRepeatedUInt64(Stream: TProtoStreamReader;
    Data: TUInt64s): Boolean;
function LoadRepeatedAnsiString(Stream: TProtoStreamReader;
     Data: TAnsiStrings): Boolean;
function LoadRepeatedBoolean(Stream: TProtoStreamReader;
    Data: TBooleans): Boolean;
function LoadRepeatedByte(Stream: TProtoStreamReader;
    Data: TBytes): Boolean;

procedure SaveMessage(Stream: TProtoStreamWriter;
    const Data: TBaseMessage;
    const TagID: Integer);
function LoadMessage(Stream: TProtoStreamReader;
    const Data: TBaseMessage): Boolean;
generic procedure SaveRepeatedMessage<TMessage>(Stream: TProtoStreamWriter;
    const Data: specialize TObjectList<TMessage>;
    const TagID: Integer);
generic function LoadRepeatedMessage<TMessage>(Stream: TProtoStreamReader;
    Data: specialize TObjectList<TMessage>): Boolean;

procedure MaybeDispose(P: PDouble);
procedure MaybeDispose(P: PSingle);
procedure MaybeDispose(P: PInt16);
procedure MaybeDispose(P: PInt32);
procedure MaybeDispose(P: PInt64);
procedure MaybeDispose(P: PUInt16);
procedure MaybeDispose(P: PUInt32);
procedure MaybeDispose(P: PUInt64);
procedure MaybeDispose(P: PBoolean);
procedure MaybeDispose(P: PAnsiString);
procedure MaybeDispose(P: PByte);

implementation

{ TBytes }

constructor TBytes.Create;
begin
  inherited Create;

end;

function TBytes.SimpleObjectToString(Obj: Byte): AnsiString;
begin
  Result := IntToStr(Obj);

end;

{ TSingles }

constructor TSingles.Create;
begin
  inherited Create;

end;

function TSingles.SimpleObjectToString(Obj: Single): AnsiString;
begin
  Result := FloatToStr(Obj);

end;

{ TDoubles }

constructor TDoubles.Create;
begin
  inherited Create;

end;

function TDoubles.SimpleObjectToString(Obj: Double): AnsiString;
begin
  Result := FloatToStr(Obj);

end;

{ TInt32s }

constructor TInt32s.Create;
begin
  inherited Create;

end;

function TInt32s.SimpleObjectToString(Obj: Int32): AnsiString;
begin
  Result := IntToStr(Obj);

end;

{ TInt64s }

constructor TInt64s.Create;
begin
  inherited Create;

end;

function TInt64s.SimpleObjectToString(Obj: Int64): AnsiString;
begin
  Result := IntToStr(Obj);

end;

{ TUInt32s }

constructor TUInt32s.Create;
begin
  inherited Create;

end;

function TUInt32s.SimpleObjectToString(Obj: UInt32): AnsiString;
begin
  Result := UIntToStr(Obj);

end;

{ TUInt64s }

constructor TUInt64s.Create;
begin
  inherited Create;

end;

function TUInt64s.SimpleObjectToString(Obj: UInt64): AnsiString;
begin
  Result := UIntToStr(Obj);

end;

{ TBooleans }

constructor TBooleans.Create;
begin
  inherited Create;

end;

function TBooleans.SimpleObjectToString(Obj: Boolean): AnsiString;
begin
  Result := BoolToStr(Obj, 'True', 'False');

end;

{ TAnsiStrings }

constructor TAnsiStrings.Create;
begin
  inherited Create;

end;

function TAnsiStrings.SimpleObjectToString(Obj: AnsiString): AnsiString;
begin
  Result := Obj;

end;

{ EBaseOneOf }

constructor EBaseOneOf.CreateTwoValueAreSet;
begin
  inherited Create('Two values of an OneOf Field are set!');

end;

{ TBaseOneOf }

function TBaseOneOf.GetPointerByIndex(Index: Integer): Pointer;
begin
  if Index = _ObjectIndex then
    Exit(_Data);

  Result := nil;

end;

procedure TBaseOneOf.SetPointerByIndex(Index: Integer; AValue: Pointer);
begin
  if _ObjectIndex = -1 then
  begin
    _Data := AValue;
    if AValue = nil then
      Exit;
    _ObjectIndex := Index;
    Exit;

  end;

  if AValue = nil then
  begin
    _ObjectIndex := -1;
    _Data := AValue;
    Exit;

  end;

  if Index <> _ObjectIndex then
    raise EBaseOneOf.CreateTwoValueAreSet;
end;

constructor TBaseOneOf.Create;
begin
  inherited Create;

  _ObjectIndex := -1;
end;

destructor TBaseOneOf.Destroy;
begin

  inherited Destroy;
end;

{ TBaseMessage }

procedure SaveRepeatedAnsiString(Stream: TProtoStreamWriter;
  const Data: TAnsiStrings; const TagID: Integer);
var
  AnsiStringData: AnsiString;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for AnsiStringData in Data do
  begin
    Stream.WriteRawVarint32(Length(AnsiStringData));
    Stream.WriteRawData(PChar(AnsiStringData), Length(AnsiStringData));

  end;

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedBoolean(Stream: TProtoStreamWriter;
  const Data: TBooleans; const TagID: Integer);
var
  SingleData: Boolean;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
   for SingleData in Data do
      Stream.WriteRawByte(Ord(SingleData));

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedByte(Stream: TProtoStreamWriter; const Data: TBytes;
  const TagID: Integer);
var
  SingleData: Byte;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
 for SingleData in Data do
    Stream.WriteRawByte(Ord(SingleData));

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

function LoadRepeatedSingle(Stream: TProtoStreamReader;
  Data: TSingles): Boolean;
var
  Len: uInt32;
  NewDatum: Single;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadSingle;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;

end;

function LoadRepeatedDouble(Stream: TProtoStreamReader;
  Data: TDoubles): Boolean;
var
  Len: uInt32;
  NewDatum: Double;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadDouble;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedInt32(Stream: TProtoStreamReader;
  Data: TInt32s): Boolean;
var
  Len: uInt32;
  NewDatum: Int32;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadInt32;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedInt64(Stream: TProtoStreamReader;
  Data: TInt64s): Boolean;
var
  Len: uInt32;
  NewDatum: Int64;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadInt64;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedUInt32(Stream: TProtoStreamReader;
  Data: TUInt32s): Boolean;
var
  Len: uInt32;
  NewDatum: UInt32;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadUInt32;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedUInt64(Stream: TProtoStreamReader;
  Data: TUInt64s): Boolean;
var
  Len: uInt32;
  NewDatum: UInt64;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadUInt64;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedAnsiString(Stream: TProtoStreamReader;
  Data: TAnsiStrings): Boolean;
var
  Len: uInt32;
  NewDatum: AnsiString;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadAnsiString;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedBoolean(Stream: TProtoStreamReader;
  Data: TBooleans): Boolean;
var
  Len: uInt32;
  NewDatum: Boolean;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadBoolean;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

function LoadRepeatedByte(Stream: TProtoStreamReader; Data: TBytes): Boolean;
var
  Len: uInt32;
  NewDatum: Byte;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := Stream.ReadByte;
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

procedure SaveMessage(Stream: TProtoStreamWriter;
  const Data: TBaseMessage; const TagID: Integer);
var
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  if TagID <> -1 then
    Stream.WriteTag(TagID, WIRETYPE_LENGTH_DELIMITED);
  SizeNode := Stream.AddIntervalNode;
  Data.SaveToStream(Stream);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

function LoadMessage(Stream: TProtoStreamReader;
  const Data: TBaseMessage): Boolean;
var
  Len: Integer;

begin
  Len := Stream.ReadUInt32;

  Result := Data.LoadFromStream(Stream, Len);

end;

generic procedure SaveRepeatedMessage<TMessage>(Stream: TProtoStreamWriter;
    const Data: specialize TObjectList<TMessage>;
    const TagID: Integer);
var
  Message: TMessage;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for Message in Data do
    SaveMessage(Stream, Message, -1);

  SizeNode.WriteLength(SizeNode.TotalSize);
end;

generic function LoadRepeatedMessage<TMessage>(Stream: TProtoStreamReader;
    Data: specialize TObjectList<TMessage>): Boolean;
var
  Len: uInt32;
  NewDatum: TMessage;
  StartPos: Integer;

begin
  Len := Stream.ReadUInt32;
  StartPos := Stream.Position;

  while Stream.Position < StartPos + Len do
  begin
    NewDatum := TMessage.Create;
    LoadMessage(Stream, NewDatum);
    Data.Add(NewDatum);

  end;

  Result := StartPos + Len = Stream.Position;
end;

procedure MaybeDispose(P: PDouble);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PSingle);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PInt16);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PInt32);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PInt64);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PUInt16);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PUInt32);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PUInt64);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PBoolean);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PAnsiString);
begin
  if P <> nil then
    Dispose(P);

end;

procedure MaybeDispose(P: PByte);
begin
  if P <> nil then
    Dispose(P);

end;

procedure SaveRepeatedSingle(Stream: TProtoStreamWriter;
  const Data: TSingles; const TagID: Integer);
var
  SingleData: Single;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawData(@SingleData, SizeOf(Single));

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedDouble(Stream: TProtoStreamWriter;
  const Data: TDoubles; const TagID: Integer);
var
  SingleData: Double;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
  for SingleData in Data do
    Stream.WriteRawData(@SingleData, SizeOf(Double));

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedInt32(Stream: TProtoStreamWriter;
  const Data: TInt32s; const TagID: Integer);
var
  SingleData: Int32;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
   for SingleData in Data do
      Stream.WriteRawVarint32(SingleData);

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedInt64(Stream: TProtoStreamWriter;
  const Data: TInt64s; const TagID: Integer);
var
  SingleData: Int64;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
   for SingleData in Data do
      Stream.WriteRawVarint64(SingleData);

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter;
  const Data: TUInt32s; const TagID: Integer);
var
  SingleData: UInt32;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
   for SingleData in Data do
      Stream.WriteRawVarint32(SingleData);

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter;
  const Data: TUInt64s; const TagID: Integer);
var
  SingleData: UInt64;
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, 2);
  SizeNode := Stream.AddIntervalNode;
   for SingleData in Data do
      Stream.WriteRawVarint64(SingleData);

   SizeNode.WriteLength(SizeNode.TotalSize);

end;

function TBaseMessage.ReadRepeatedLength(Stream: TProtoStreamReader): UInt32;
begin
  Result := Stream.ReadUInt32;

end;

procedure SaveSingle(Stream: TProtoStreamWriter;
  const Data: Single; const TagID: Integer);
const
  AlmostZero: Double = 1e-10;

begin
  if AlmostZero < Abs(Data) then
    Stream.WriteFloat(TagID, Data);

end;

procedure SaveDouble(Stream: TProtoStreamWriter;
  const Data: Double; const TagID: Integer);
const
  AlmostZero: Double = 1e-10;

begin
  if AlmostZero < Abs(Data) then
    Stream.WriteDouble(TagID, Data);

end;

procedure SaveInt32(Stream: TProtoStreamWriter; const Data: Int32;
  const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteInt32(TagID, Data);

end;

procedure SaveInt64(Stream: TProtoStreamWriter; const Data: Int64;
  const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteInt64(TagID, Data);

end;

procedure SaveUInt32(Stream: TProtoStreamWriter;
  const Data: UInt32; const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteUInt32(TagID, Data);

end;

procedure SaveUInt64(Stream: TProtoStreamWriter;
  const Data: UInt64; const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteUInt64(TagID, Data);

end;

procedure SaveAnsiString(Stream: TProtoStreamWriter;
  const Data: AnsiString; const TagID: Integer);
begin
  if Data <> '' then
    Stream.WriteString(TagID, Data);

end;

procedure SaveBoolean(Stream: TProtoStreamWriter;
  const Data: Boolean; const TagID: Integer);
begin
  if Data then
    Stream.WriteBoolean(TagID, Data);

end;

procedure SaveByte(Stream: TProtoStreamWriter; const Data: Byte;
  const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteByte(TagID, Data);

end;

function LoadSingle(Stream: TProtoStreamReader): Single;
begin
  Result := Stream.ReadSingle;

end;

function LoadDouble(Stream: TProtoStreamReader): Double;
begin
  Result := Stream.ReadDouble;
end;

function LoadInt32(Stream: TProtoStreamReader): Int32;
begin
  Result := Stream.ReadInt32;

end;

function LoadInt64(Stream: TProtoStreamReader): Int64;
begin
  Result := Stream.ReadInt64;

end;

function LoadUInt32(Stream: TProtoStreamReader): UInt32;
begin
  Result := Stream.ReadUInt32

end;

function LoadUInt64(Stream: TProtoStreamReader): UInt64;
begin
  Result := Stream.ReadUInt64
  ;
end;

function LoadAnsiString(Stream: TProtoStreamReader): AnsiString;
begin
  Result := Stream.ReadAnsiString;

end;

function LoadBoolean(Stream: TProtoStreamReader): Boolean;
begin
  Result := Stream.ReadBoolean;

end;

function LoadByte(Stream: TProtoStreamReader): Byte;
begin
  Result := Stream.ReadByte;

end;

constructor TBaseMessage.Create;
begin
  inherited Create;

end;

destructor TBaseMessage.Destroy;
begin
  inherited Destroy;
end;

function TBaseMessage.LoadFromString(Str: AnsiString): Boolean;
begin
  Result := self.LoadFromStream(TStringStream.Create(Str));

end;

function TBaseMessage.LoadFromStream(Stream: TStream): Boolean;
var
  ProtoStream: TProtoStreamReader;

begin
  ProtoStream := TProtoStreamReader.Create(Stream);

  Result := Self.LoadFromStream(ProtoStream, Stream.Size);

  ProtoStream.Free;
end;

procedure TBaseMessage.SaveToStream(Stream: TStream);
var
  ProtoStream: TProtoStreamWriter;

begin
  ProtoStream := TProtoStreamWriter.Create(Stream);

  Self.SaveToStream(ProtoStream);

  ProtoStream.Free;
end;

end.
