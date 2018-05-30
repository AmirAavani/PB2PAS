unit ProtoHelperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ProtoHelperListsUnit, ProtoStreamUnit;

type
  { TBaseMessage }

  TBaseMessage = class(TObject)
  private
    function ReadRepeatedLength(Stream: TProtoStreamReader): UInt32;

  protected
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

    function LoadSingle(Stream: TProtoStreamReader): Single;
    function LoadDouble(Stream: TProtoStreamReader): Double;
    function LoadInt32(Stream: TProtoStreamReader): Int32;
    function LoadInt64(Stream: TProtoStreamReader): Int64;
    function LoadUInt32(Stream: TProtoStreamReader): UInt32;
    function LoadUInt64(Stream: TProtoStreamReader): UInt64;
    function LoadAnsiString(Stream: TProtoStreamReader): AnsiString;
    function LoadBoolean(Stream: TProtoStreamReader): Boolean;

    procedure SaveRepeatedSingle(Stream: TProtoStreamWriter;
    const Data: specialize TSimpleTypeList<Single>;
    const TagID: Integer);
    procedure SaveRepeatedDouble(Stream: TProtoStreamWriter;
        const Data: specialize TSimpleTypeList<Double>;
        const TagID: Integer);
    procedure SaveRepeatedInt32(Stream: TProtoStreamWriter;
        const Data: specialize TSimpleTypeList<Int32>;
        const TagID: Integer);
    procedure SaveRepeatedInt64(Stream: TProtoStreamWriter;
        const Data: specialize TSimpleTypeList<Int64>;
        const TagID: Integer);
    procedure SaveRepeatedUInt32(Stream: TProtoStreamWriter;
        const Data: specialize TSimpleTypeList<UInt32>;
        const TagID: Integer);
    procedure SaveRepeatedUInt64(Stream: TProtoStreamWriter;
        const Data: specialize TSimpleTypeList<UInt64>;
        const TagID: Integer);
    procedure SaveRepeatedAnsiString(
        Stream: TProtoStreamWriter;
       const Data: specialize TSimpleTypeList<AnsiString>;
      const TagID: Integer);
    procedure SaveRepeatedBoolean(Stream: TProtoStreamWriter;
        const Data: specialize TSimpleTypeList<Boolean>;
        const TagID: Integer);

    function LoadRepeatedSingle(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<Single>): Boolean;
    function LoadRepeatedDouble(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<Double>): Boolean;
    function LoadRepeatedInt32(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<Int32>): Boolean;
    function LoadRepeatedInt64(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<Int64>): Boolean;
    function LoadRepeatedUInt32(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<UInt32>): Boolean;
    function LoadRepeatedUInt64(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<UInt64>): Boolean;
    function LoadRepeatedAnsiString(Stream: TProtoStreamReader;
         Data: specialize TSimpleTypeList<AnsiString>): Boolean;
    function LoadRepeatedBoolean(Stream: TProtoStreamReader;
        Data: specialize TSimpleTypeList<Boolean>): Boolean;

    procedure SaveMessage(Stream: TProtoStreamWriter;
        const Data: TBaseMessage;
        const TagID: Integer);
    function LoadMessage(Stream: TProtoStreamReader;
        const Data: TBaseMessage): Boolean;

    procedure SaveRepeatedMessage(Stream: TProtoStreamWriter;
        const Data: specialize TObjectList<TBaseMessage>;
        const TagID: Integer);

    procedure SaveToStream(Stream: TProtoStreamWriter);  virtual; abstract;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;  virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromString(Str: AnsiString): Boolean; virtual;
    function LoadFromStream(Stream: TStream): Boolean; virtual;
    procedure SaveToStream(Stream: TStream);  virtual;

  end;


implementation

{ TBaseMessage }

procedure TBaseMessage.SaveRepeatedAnsiString(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<AnsiString>; const TagID: Integer);
var
  AnsiStringData: AnsiString;
  i: Integer;

begin
  if Data = nil then
    Exit;

  for i := 0 to Data.Count - 1 do
  begin
    AnsiStringData := Data[i];
    Stream.WriteString(TagID, AnsiStringData);

  end;

end;

procedure TBaseMessage.SaveRepeatedBoolean(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<Boolean>; const TagID: Integer);
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

function TBaseMessage.LoadRepeatedSingle(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<Single>): Boolean;
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

function TBaseMessage.LoadRepeatedDouble(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<Double>): Boolean;
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

function TBaseMessage.LoadRepeatedInt32(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<Int32>): Boolean;
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

function TBaseMessage.LoadRepeatedInt64(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<Int64>): Boolean;
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

function TBaseMessage.LoadRepeatedUInt32(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<UInt32>): Boolean;
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

function TBaseMessage.LoadRepeatedUInt64(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<UInt64>): Boolean;
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

function TBaseMessage.LoadRepeatedAnsiString(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<AnsiString>): Boolean;
var
  NewDatum: AnsiString;

begin
  if Data = nil then
    Exit;

  NewDatum := Stream.ReadAnsiString;
  Data.Add(NewDatum);

end;

function TBaseMessage.LoadRepeatedBoolean(Stream: TProtoStreamReader;
  Data: specialize TSimpleTypeList<Boolean>): Boolean;
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

procedure TBaseMessage.SaveMessage(Stream: TProtoStreamWriter;
  const Data: TBaseMessage; const TagID: Integer);
var
  SizeNode: TLinkListNode;

begin
  if Data = nil then
    Exit;

  Stream.WriteTag(TagID, WIRETYPE_LENGTH_DELIMITED);
  SizeNode := Stream.AddIntervalNode;
  Data.SaveToStream(Stream);

  SizeNode.WriteLength(SizeNode.TotalSize);

end;

function TBaseMessage.LoadMessage(Stream: TProtoStreamReader;
  const Data: TBaseMessage): Boolean;
var
  Len: Integer;

begin
  Len := Stream.ReadUInt32;

  Result := Data.LoadFromStream(Stream, Len);

end;

procedure TBaseMessage.SaveRepeatedMessage(Stream: TProtoStreamWriter;
  const Data: specialize TObjectList<TBaseMessage>;
  const TagID: Integer);
var
  BaseData: TBaseMessage;

begin
  if Data = nil then
    Exit;

  for BaseData in Data do
    BaseData.SaveToStream(Stream);

end;

procedure TBaseMessage.SaveRepeatedSingle(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<Single>; const TagID: Integer);
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

procedure TBaseMessage.SaveRepeatedDouble(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<Double>; const TagID: Integer);
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

procedure TBaseMessage.SaveRepeatedInt32(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<Int32>; const TagID: Integer);
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

procedure TBaseMessage.SaveRepeatedInt64(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<Int64>; const TagID: Integer);
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

procedure TBaseMessage.SaveRepeatedUInt32(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<UInt32>; const TagID: Integer);
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

procedure TBaseMessage.SaveRepeatedUInt64(Stream: TProtoStreamWriter;
  const Data: specialize TSimpleTypeList<UInt64>; const TagID: Integer);
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

procedure TBaseMessage.SaveSingle(Stream: TProtoStreamWriter;
  const Data: Single; const TagID: Integer);
const
  AlmostZero: Double = 1e-10;

begin
  if AlmostZero < Abs(Data) then
    Stream.WriteFloat(TagID, Data);

end;

procedure TBaseMessage.SaveDouble(Stream: TProtoStreamWriter;
  const Data: Double; const TagID: Integer);
const
  AlmostZero: Double = 1e-10;

begin
  if AlmostZero < Abs(Data) then
    Stream.WriteDouble(TagID, Data);

end;

procedure TBaseMessage.SaveInt32(Stream: TProtoStreamWriter; const Data: Int32;
  const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteInt32(TagID, Data);

end;

procedure TBaseMessage.SaveInt64(Stream: TProtoStreamWriter; const Data: Int64;
  const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteInt64(TagID, Data);

end;

procedure TBaseMessage.SaveUInt32(Stream: TProtoStreamWriter;
  const Data: UInt32; const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteUInt32(TagID, Data);

end;

procedure TBaseMessage.SaveUInt64(Stream: TProtoStreamWriter;
  const Data: UInt64; const TagID: Integer);
begin
  if Data <> 0 then
    Stream.WriteUInt64(TagID, Data);

end;

procedure TBaseMessage.SaveAnsiString(Stream: TProtoStreamWriter;
  const Data: AnsiString; const TagID: Integer);
begin
  if Data <> '' then
    Stream.WriteString(TagID, Data);

end;

procedure TBaseMessage.SaveBoolean(Stream: TProtoStreamWriter;
  const Data: Boolean; const TagID: Integer);
begin
  if Data then
    Stream.WriteBoolean(TagID, Data);

end;

function TBaseMessage.LoadSingle(Stream: TProtoStreamReader): Single;
begin
  Result := Stream.ReadSingle;

end;

function TBaseMessage.LoadDouble(Stream: TProtoStreamReader): Double;
begin
  Result := Stream.ReadDouble;
end;

function TBaseMessage.LoadInt32(Stream: TProtoStreamReader): Int32;
begin
  Result := Stream.ReadInt32;

end;

function TBaseMessage.LoadInt64(Stream: TProtoStreamReader): Int64;
begin
  Result := Stream.ReadInt64;

end;

function TBaseMessage.LoadUInt32(Stream: TProtoStreamReader): UInt32;
begin
  Result := Stream.ReadUInt32

end;

function TBaseMessage.LoadUInt64(Stream: TProtoStreamReader): UInt64;
begin
  Result := Stream.ReadUInt64
  ;
end;

function TBaseMessage.LoadAnsiString(Stream: TProtoStreamReader): AnsiString;
begin
  Result := Stream.ReadAnsiString;

end;

function TBaseMessage.LoadBoolean(Stream: TProtoStreamReader): Boolean;
begin
  Result := Stream.ReadBoolean;

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

  Self.LoadFromStream(ProtoStream, Stream.Size);

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
