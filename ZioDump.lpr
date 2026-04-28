program ziodump;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Math, ZIOStreamUnit, ProtoHelperUnit, ProtoStreamUnit, fpjson, jsonparser;

type
  TByteArray = array of Byte;
  
  { Wire types for protobuf }
  TWireType = (
    wtVarint = 0,      // int32, int64, uint32, uint64, sint32, sint64, bool, enum
    wt64Bit = 1,       // fixed64, sfixed64, double
    wtLengthDelimited = 2,  // string, bytes, embedded messages, packed repeated
    wtStartGroup = 3,  // deprecated
    wtEndGroup = 4,    // deprecated
    wt32Bit = 5        // fixed32, sfixed32, float
  );
  
  { Generic message for dumping - stores raw data }
  TGenericMessage = class(TBaseMessage)
  private
    FRawData: TByteArray;
  public
    procedure Clear; override;
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
    
    procedure DumpToConsole(MessageNumber: Integer);
    procedure DumpAsJson(MessageNumber: Integer);
    function GetDataLength: Integer;
  end;

{ TGenericMessage }

procedure TGenericMessage.Clear;
begin
  SetLength(FRawData, 0);
end;

procedure TGenericMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  // Not used for reading
end;

function TGenericMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  i: Integer;
begin
  // Read raw bytes one by one using Stream's methods
  SetLength(FRawData, Len);
  
  for i := 0 to Len - 1 do
  begin
    FRawData[i] := Stream.ReadByte;
  end;
  
  Result := True;
end;

function TGenericMessage.GetDataLength: Integer;
begin
  Result := Length(FRawData);
end;

{ Protobuf parsing helpers }

function ReadVarint(const Data: TByteArray; var Pos: Integer): UInt64;
var
  Shift: Integer;
  B: Byte;
begin
  Result := 0;
  Shift := 0;
  
  while Pos < Length(Data) do
  begin
    B := Data[Pos];
    Inc(Pos);
    
    Result := Result or (UInt64(B and $7F) shl Shift);
    
    if (B and $80) = 0 then
      Break;
    
    Inc(Shift, 7);
  end;
end;

function DecodeZigZag32(n: UInt32): Int32;
begin
  Result := Int32((n shr 1) xor (-(n and 1)));
end;

function DecodeZigZag64(n: UInt64): Int64;
begin
  Result := Int64((n shr 1) xor UInt64(-(Int64(n) and 1)));
end;

function BytesToString(const Data: TByteArray; Start, Len: Integer): string;
var
  i: Integer;
  AllPrintable: Boolean;
begin
  // Check if all bytes are printable ASCII
  AllPrintable := True;
  for i := Start to Start + Len - 1 do
  begin
    if (Data[i] < 32) or (Data[i] > 126) then
    begin
      AllPrintable := False;
      Break;
    end;
  end;
  
  if AllPrintable then
  begin
    // Return as string
    Result := '';
    for i := Start to Start + Len - 1 do
      Result := Result + Chr(Data[i]);
  end
  else
  begin
    // Return as hex
    Result := '';
    for i := Start to Start + Len - 1 do
      Result := Result + Format('%2.2x', [Data[i]]);
  end;
end;

function ParseProtobufToJson(const Data: TByteArray): TJSONObject;
var
  Pos: Integer;
  Tag: UInt64;
  FieldNumber: Integer;
  WireType: Integer;
  Value: UInt64;
  Len: Integer;
  StrValue: string;
  FieldName: string;
  FixedVal32: UInt32;
  FixedVal64: UInt64;
  DoubleVal: Double;
  FloatVal: Single;
  ExistingValue: TJSONData;
  ArrayVal: TJSONArray;
begin
  Result := TJSONObject.Create;
  Pos := 0;
  
  while Pos < Length(Data) do
  begin
    // Read tag
    Tag := ReadVarint(Data, Pos);
    FieldNumber := Integer(Tag shr 3);
    WireType := Integer(Tag and $07);
    
    FieldName := IntToStr(FieldNumber);
    
    case WireType of
      0: // Varint
      begin
        Value := ReadVarint(Data, Pos);
        
        // Check if field already exists (repeated field)
        ExistingValue := Result.Find(FieldName);
        if ExistingValue <> nil then
        begin
          if ExistingValue is TJSONArray then
            TJSONArray(ExistingValue).Add(Int64(Value))
          else
          begin
            // Convert to array
            ArrayVal := TJSONArray.Create;
            ArrayVal.Add(ExistingValue.Clone);
            ArrayVal.Add(Int64(Value));
            Result.Delete(FieldName);
            Result.Add(FieldName, ArrayVal);
          end;
        end
        else
          Result.Add(FieldName, Int64(Value));
      end;
      
      1: // 64-bit
      begin
        if Pos + 8 > Length(Data) then
          Break;
        
        Move(Data[Pos], FixedVal64, 8);
        Inc(Pos, 8);
        
        ExistingValue := Result.Find(FieldName);
        if ExistingValue <> nil then
        begin
          if ExistingValue is TJSONArray then
            TJSONArray(ExistingValue).Add(Int64(FixedVal64))
          else
          begin
            ArrayVal := TJSONArray.Create;
            ArrayVal.Add(ExistingValue.Clone);
            ArrayVal.Add(Int64(FixedVal64));
            Result.Delete(FieldName);
            Result.Add(FieldName, ArrayVal);
          end;
        end
        else
          Result.Add(FieldName, Int64(FixedVal64));
      end;
      
      2: // Length-delimited
      begin
        Len := Integer(ReadVarint(Data, Pos));
        if Pos + Len > Length(Data) then
          Len := Length(Data) - Pos;
        
        StrValue := BytesToString(Data, Pos, Len);
        Inc(Pos, Len);
        
        ExistingValue := Result.Find(FieldName);
        if ExistingValue <> nil then
        begin
          if ExistingValue is TJSONArray then
            TJSONArray(ExistingValue).Add(StrValue)
          else
          begin
            ArrayVal := TJSONArray.Create;
            ArrayVal.Add(ExistingValue.Clone);
            ArrayVal.Add(StrValue);
            Result.Delete(FieldName);
            Result.Add(FieldName, ArrayVal);
          end;
        end
        else
          Result.Add(FieldName, StrValue);
      end;
      
      5: // 32-bit
      begin
        if Pos + 4 > Length(Data) then
          Break;
        
        Move(Data[Pos], FixedVal32, 4);
        Inc(Pos, 4);
        
        ExistingValue := Result.Find(FieldName);
        if ExistingValue <> nil then
        begin
          if ExistingValue is TJSONArray then
            TJSONArray(ExistingValue).Add(Int64(FixedVal32))
          else
          begin
            ArrayVal := TJSONArray.Create;
            ArrayVal.Add(ExistingValue.Clone);
            ArrayVal.Add(Int64(FixedVal32));
            Result.Delete(FieldName);
            Result.Add(FieldName, ArrayVal);
          end;
        end
        else
          Result.Add(FieldName, Int64(FixedVal32));
      end;
      
      else
        // Unknown wire type, skip
        Break;
    end;
  end;
end;

procedure TGenericMessage.DumpAsJson(MessageNumber: Integer);
var
  JsonObj: TJSONObject;
begin
  if Length(FRawData) = 0 then
  begin
    WriteLn('{}');
    Exit;
  end;
  
  JsonObj := ParseProtobufToJson(FRawData);
  try
    WriteLn(JsonObj.AsJSON);
  finally
    JsonObj.Free;
  end;
end;

procedure TGenericMessage.DumpToConsole(MessageNumber: Integer);
var
  i, j, LineBytes: Integer;
  HexPart, AsciiPart: string;
begin
  WriteLn('Message #', MessageNumber, ' (', Length(FRawData), ' bytes):');
  WriteLn('  Hex dump:');
  
  if Length(FRawData) = 0 then
  begin
    WriteLn('    (empty)');
    Exit;
  end;
  
  // Hex dump with ASCII representation
  i := 0;
  while i < Length(FRawData) do
  begin
    HexPart := '';
    AsciiPart := '';
    LineBytes := Min(16, Length(FRawData) - i);
    
    for j := 0 to LineBytes - 1 do
    begin
      HexPart := HexPart + Format('%2.2x ', [FRawData[i + j]]);
      
      // ASCII representation
      if (FRawData[i + j] >= 32) and (FRawData[i + j] <= 126) then
        AsciiPart := AsciiPart + Chr(FRawData[i + j])
      else
        AsciiPart := AsciiPart + '.';
    end;
    
    // Pad hex part if needed
    while Length(HexPart) < 48 do
      HexPart := HexPart + ' ';
    
    WriteLn('    ', Format('%8.8x', [i]), ': ', HexPart, ' |', AsciiPart, '|');
    Inc(i, LineBytes);
  end;
  
  WriteLn;
end;

{ Utility procedures }

procedure PrintUsage;
begin
  WriteLn('Usage: ziodump <pattern>');
  WriteLn;
  WriteLn('Dumps the contents of ZIO files matching the pattern.');
  WriteLn;
  WriteLn('Pattern format:');
  WriteLn('  - path@N           : N sharded files (e.g., "data/output@4")');
  WriteLn('  - single_file.zio  : Single ZIO file');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  ziodump data/output@4');
  WriteLn('  ziodump mydata.zio');
  WriteLn;
  WriteLn('The program will read and display all messages in the ZIO file(s).');
end;

procedure DumpShardedFiles(const Pattern: string);
var
  Pat: TPattern;
  Reader: specialize TZioReader<TGenericMessage>;
  Msg: TGenericMessage;
  MessageCount: Integer;
  i: Integer;
begin
  WriteLn('Reading sharded ZIO files: ', Pattern);
  WriteLn;
  
  try
    Pat := TPattern.Create(Pattern);
    try
      WriteLn('Base path: ', Pat.BasePath);
      WriteLn('Number of shards: ', Pat.NumShards);
      WriteLn('Shard files:');
      for i := 0 to Pat.NumShards - 1 do
        WriteLn('  [', i, '] ', Pat.GetShardPath(i));
      WriteLn;
      
      Reader := specialize TZioReader<TGenericMessage>.Create(Pat);
      try
        MessageCount := 0;
        Msg := TGenericMessage.Create;
        try
          while Reader.ReadMessage(Msg) do
          begin
            Inc(MessageCount);
            Msg.DumpAsJson(MessageCount);
          end;
        finally
          Msg.Free;
        end;
        
        WriteLn('Total messages read: ', MessageCount);
      finally
        Reader.Free;
      end;
    finally
      Pat.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end;

procedure DumpSingleFile(const FilePath: string);
var
  FileStream: TFileStream;
  ZStream: TZioStream;
  Msg: TGenericMessage;
  MessageCount: Integer;
begin
  WriteLn('Reading single ZIO file: ', FilePath);
  WriteLn;
  
  if not FileExists(FilePath) then
  begin
    WriteLn('ERROR: File not found: ', FilePath);
    Halt(1);
  end;
  
  try
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      ZStream := TZioStream.Create(FileStream, False);
      try
        WriteLn('File size: ', FileStream.Size, ' bytes');
        WriteLn;
        
        MessageCount := 0;
        Msg := TGenericMessage.Create;
        try
          while ZStream.ReadMessage(Msg) do
          begin
            Inc(MessageCount);
            Msg.DumpAsJson(MessageCount);
          end;
        finally
          Msg.Free;
        end;
        
        WriteLn('Total messages read: ', MessageCount);
      finally
        ZStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end;

{ Main }

var
  Pattern: string;

begin
  WriteLn('===========================================');
  WriteLn('  ZIO Dump Utility');
  WriteLn('===========================================');
  WriteLn;
  
  if ParamCount <> 1 then
  begin
    PrintUsage;
    Halt(1);
  end;
  
  Pattern := ParamStr(1);
  
  // Check if it's a sharded pattern (contains '@')
  if Pos('@', Pattern) > 0 then
    DumpShardedFiles(Pattern)
  else
    DumpSingleFile(Pattern);
  
  WriteLn;
  WriteLn('Done.');
end.
