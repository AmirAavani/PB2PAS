program ziodump;

{$mode objfpc}{$H+}

uses
  cthreads, Classes, SysUtils, Math, ZIOStreamUnit, ProtoHelperUnit, ProtoStreamUnit, 
  fpjson, jsonparser, PBParserUnit, PBDefinitionUnit, ALoggerUnit, ParamsUnit,
  ParamManagerUnit;

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
    FRootMessageDef: TMessage;  // Schema definition if available
    FProtoMap: TProtoMap;        // All proto definitions
  public
    procedure Clear; override;
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
    
    procedure DumpToConsole(MessageNumber: Integer);
    procedure DumpAsJson(MessageNumber: Integer);
    function GetDataLength: Integer;
    
    property RootMessageDef: TMessage read FRootMessageDef write FRootMessageDef;
    property ProtoMap: TProtoMap read FProtoMap write FProtoMap;
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
  // Check if all bytes are printable ASCII or common whitespace
  AllPrintable := True;
  for i := Start to Start + Len - 1 do
  begin
    // Allow printable ASCII (32-126) and common whitespace (tab, newline, carriage return)
    if not ((Data[i] >= 32) and (Data[i] <= 126)) and 
       not ((Data[i] = 9) or (Data[i] = 10) or (Data[i] = 13)) then
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

function IsLikelyString(const Data: TByteArray; Start, Len: Integer): Boolean;
var
  i: Integer;
  PrintableCount: Integer;
begin
  // If length is 0, not a string
  if Len = 0 then
    Exit(False);
  
  // Count printable ASCII characters
  PrintableCount := 0;
  for i := Start to Start + Len - 1 do
  begin
    if (Data[i] >= 32) and (Data[i] <= 126) then
      Inc(PrintableCount)
    // Also allow common whitespace: tab, newline, carriage return
    else if (Data[i] = 9) or (Data[i] = 10) or (Data[i] = 13) then
      Inc(PrintableCount);
  end;
  
  // If > 80% printable, likely a string
  Result := (PrintableCount * 100 div Len) > 80;
end;

function TryParsePackedVarint(const Data: TByteArray; StartPos, Len: Integer): TJSONArray;
var
  Pos, EndPos: Integer;
  Value: UInt64;
begin
  Result := TJSONArray.Create;
  Pos := StartPos;
  EndPos := StartPos + Len;
  
  try
    // Try to parse as packed varints
    while Pos < EndPos do
    begin
      Value := ReadVarint(Data, Pos);
      Result.Add(Int64(Value));
    end;
    
    // If we didn't consume exactly Len bytes, it's not packed varints
    if Pos <> EndPos then
    begin
      Result.Free;
      Result := nil;
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function TryParsePackedFixed32(const Data: TByteArray; StartPos, Len: Integer): TJSONArray;
var
  Pos, EndPos: Integer;
  FixedVal32: UInt32;
begin
  // Length must be a multiple of 4
  if (Len mod 4) <> 0 then
    Exit(nil);
  
  Result := TJSONArray.Create;
  Pos := StartPos;
  EndPos := StartPos + Len;
  
  try
    while Pos < EndPos do
    begin
      if Pos + 4 > EndPos then
      begin
        Result.Free;
        Exit(nil);
      end;
      
      Move(Data[Pos], FixedVal32, 4);
      Inc(Pos, 4);
      Result.Add(Int64(FixedVal32));
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function TryParsePackedFixed64(const Data: TByteArray; StartPos, Len: Integer): TJSONArray;
var
  Pos, EndPos: Integer;
  FixedVal64: UInt64;
begin
  // Length must be a multiple of 8
  if (Len mod 8) <> 0 then
    Exit(nil);
  
  Result := TJSONArray.Create;
  Pos := StartPos;
  EndPos := StartPos + Len;
  
  try
    while Pos < EndPos do
    begin
      if Pos + 8 > EndPos then
      begin
        Result.Free;
        Exit(nil);
      end;
      
      Move(Data[Pos], FixedVal64, 8);
      Inc(Pos, 8);
      Result.Add(Int64(FixedVal64));
    end;
  except
    Result.Free;
    Result := nil;
  end;
end;

function ParseProtobufToJson(const Data: TByteArray; MessageDef: TMessage; ProtoMap: TProtoMap): TJSONObject; forward;

function IsMessageField(FieldDef: TMessageField; MessageDef: TMessage; ProtoMap: TProtoMap; out NestedMsgDef: TMessage): Boolean;
var
  FieldTypeName: AnsiString;
  Proto: TProto;
  it: TProtoMap.TPairEnumerator;
begin
  Result := False;
  NestedMsgDef := nil;
  
  if (FieldDef = nil) or (FieldDef.FieldType = nil) then
    Exit;
  
  FieldTypeName := FieldDef.FieldType.Name;
  
  // Check if it's a simple/primitive type (lowercase)
  if (FieldTypeName = 'double') or (FieldTypeName = 'float') or
     (FieldTypeName = 'int32') or (FieldTypeName = 'int64') or
     (FieldTypeName = 'uint32') or (FieldTypeName = 'uint64') or
     (FieldTypeName = 'sint32') or (FieldTypeName = 'sint64') or
     (FieldTypeName = 'fixed32') or (FieldTypeName = 'fixed64') or
     (FieldTypeName = 'sfixed32') or (FieldTypeName = 'sfixed64') or
     (FieldTypeName = 'bool') or (FieldTypeName = 'string') or
     (FieldTypeName = 'bytes') then
    Exit(False);
  
  // It's a message type - find the definition
  Result := True;
  
  // Look in current message first
  if MessageDef <> nil then
    NestedMsgDef := MessageDef.Messages.ByName[FieldTypeName];
  
  // Look in all protos
  if (NestedMsgDef = nil) and (ProtoMap <> nil) then
  begin
    it := ProtoMap.GetEnumerator;
    while it.MoveNext do
    begin
      Proto := it.Current.Value;
      NestedMsgDef := Proto.Messages.ByName[FieldTypeName];
      if NestedMsgDef <> nil then
        Break;
    end;
    it.Free;
  end;
end;

function ParseProtobufToJson(const Data: TByteArray; MessageDef: TMessage; ProtoMap: TProtoMap): TJSONObject;
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
  PackedArray: TJSONArray;
  NestedMsg: TJSONObject;
  FieldDef: TMessageField;
  Field: TMessageField;
  SubData: TByteArray;
  i: Integer;
  NestedMsgDef: TMessage;
begin
  Result := TJSONObject.Create;
  Pos := 0;
  
  while Pos < Length(Data) do
  begin
    // Read tag
    Tag := ReadVarint(Data, Pos);
    FieldNumber := Integer(Tag shr 3);
    WireType := Integer(Tag and $07);
    
    // Look up field definition in schema and set field name
    FieldDef := nil;
    if MessageDef <> nil then
    begin
      for Field in MessageDef.Fields do
      begin
        if Field.FieldNumber = FieldNumber then
        begin
          FieldDef := Field;
          Break;
        end;
      end;
    end;
    
    // Use field name from schema if available, otherwise use field number
    if FieldDef <> nil then
      FieldName := FieldDef.Name
    else
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
      
      2: // Length-delimited (strings, bytes, embedded messages, or packed repeated)
      begin
        Len := Integer(ReadVarint(Data, Pos));
        if Pos + Len > Length(Data) then
          Len := Length(Data) - Pos;
        
        // FieldDef was already looked up above
        
        // Check if it's a message type using schema
        if IsMessageField(FieldDef, MessageDef, ProtoMap, NestedMsgDef) and (NestedMsgDef <> nil) then
        begin
          // It's a nested message - parse recursively
          SetLength(SubData, Len);
          for i := 0 to Len - 1 do
            SubData[i] := Data[Pos + i];
          
          NestedMsg := ParseProtobufToJson(SubData, NestedMsgDef, ProtoMap);
          
          ExistingValue := Result.Find(FieldName);
          if ExistingValue <> nil then
          begin
            if ExistingValue is TJSONArray then
              TJSONArray(ExistingValue).Add(NestedMsg)
            else
            begin
              ArrayVal := TJSONArray.Create;
              ArrayVal.Add(ExistingValue.Clone);
              ArrayVal.Add(NestedMsg);
              Result.Delete(FieldName);
              Result.Add(FieldName, ArrayVal);
            end;
          end
          else
            Result.Add(FieldName, NestedMsg);
        end
        else if (FieldDef <> nil) and (FieldDef.FieldType <> nil) and 
                ((FieldDef.FieldType.Name = 'string') or (FieldDef.FieldType.Name = 'bytes')) then
        begin
          // Schema says it's a string/bytes field - just convert directly
          StrValue := BytesToString(Data, Pos, Len);
          
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
        end
        else if (FieldDef <> nil) and (FieldDef.FieldType <> nil) and FieldDef.FieldType.IsRepeated then
        begin
          // Schema says it's a repeated numeric field - try packed repeated
          PackedArray := nil;
          
          // Try different packed formats based on field type
          if (FieldDef.FieldType.Name = 'int32') or (FieldDef.FieldType.Name = 'int64') or
             (FieldDef.FieldType.Name = 'uint32') or (FieldDef.FieldType.Name = 'uint64') or
             (FieldDef.FieldType.Name = 'sint32') or (FieldDef.FieldType.Name = 'sint64') or
             (FieldDef.FieldType.Name = 'bool') then
            PackedArray := TryParsePackedVarint(Data, Pos, Len)
          else if (FieldDef.FieldType.Name = 'fixed32') or (FieldDef.FieldType.Name = 'sfixed32') or
                  (FieldDef.FieldType.Name = 'float') then
            PackedArray := TryParsePackedFixed32(Data, Pos, Len)
          else if (FieldDef.FieldType.Name = 'fixed64') or (FieldDef.FieldType.Name = 'sfixed64') or
                  (FieldDef.FieldType.Name = 'double') then
            PackedArray := TryParsePackedFixed64(Data, Pos, Len);
          
          if PackedArray <> nil then
          begin
            ExistingValue := Result.Find(FieldName);
            if ExistingValue <> nil then
            begin
              if ExistingValue is TJSONArray then
              begin
                for Value := 0 to PackedArray.Count - 1 do
                  TJSONArray(ExistingValue).Add(PackedArray.Items[Value].Clone);
                PackedArray.Free;
              end
              else
              begin
                ArrayVal := TJSONArray.Create;
                ArrayVal.Add(ExistingValue.Clone);
                for Value := 0 to PackedArray.Count - 1 do
                  ArrayVal.Add(PackedArray.Items[Value].Clone);
                Result.Delete(FieldName);
                Result.Add(FieldName, ArrayVal);
                PackedArray.Free;
              end;
            end
            else
              Result.Add(FieldName, PackedArray);
          end
          else
          begin
            // Fallback to string if packed parsing fails
            StrValue := BytesToString(Data, Pos, Len);
            Result.Add(FieldName, StrValue);
          end;
        end
        else
        begin
          // No schema or unknown field - use heuristics
          if IsLikelyString(Data, Pos, Len) then
            PackedArray := nil
          else
          begin
            PackedArray := TryParsePackedVarint(Data, Pos, Len);
            if PackedArray = nil then
              PackedArray := TryParsePackedFixed32(Data, Pos, Len);
            if PackedArray = nil then
              PackedArray := TryParsePackedFixed64(Data, Pos, Len);
          end;
          
          if PackedArray <> nil then
          begin
            ExistingValue := Result.Find(FieldName);
            if ExistingValue <> nil then
            begin
              if ExistingValue is TJSONArray then
              begin
                for Value := 0 to PackedArray.Count - 1 do
                  TJSONArray(ExistingValue).Add(PackedArray.Items[Value].Clone);
                PackedArray.Free;
              end
              else
              begin
                ArrayVal := TJSONArray.Create;
                ArrayVal.Add(ExistingValue.Clone);
                for Value := 0 to PackedArray.Count - 1 do
                  ArrayVal.Add(PackedArray.Items[Value].Clone);
                Result.Delete(FieldName);
                Result.Add(FieldName, ArrayVal);
                PackedArray.Free;
              end;
            end
            else
              Result.Add(FieldName, PackedArray);
          end
          else
          begin
            StrValue := BytesToString(Data, Pos, Len);
            
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
        end;
        
        Inc(Pos, Len);
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
  
  JsonObj := ParseProtobufToJson(FRawData, FRootMessageDef, FProtoMap);
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
  WriteLn('Usage: ziodump InputFileName=<pattern> [ProtoFile=<file>]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  InputFileName=<pattern>  : ZIO file pattern to dump');
  WriteLn('                             - path@N for N sharded files');
  WriteLn('                             - single_file.zio for single file');
  WriteLn('  ProtoFile=<file>         : Optional .proto file for schema');
  WriteLn('  Verbosity=<level>        : Log verbosity level (default: 0)');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  ziodump InputFileName=data/output@4 ProtoFile=schema.proto');
  WriteLn('  ziodump InputFileName=mydata.zio');
  WriteLn;
  WriteLn('The program will read and display all messages in the ZIO file(s).');
end;

procedure DumpShardedFiles(const Pattern: string; ProtoMap: TProtoMap);
var
  Pat: TPattern;
  Reader: specialize TZioReader<TGenericMessage>;
  Msg: TGenericMessage;
  MessageCount: Integer;
  i: Integer;
  RootMessage: TMessage;
  Proto: TProto;
  it: TProtoMap.TPairEnumerator;
begin
  WriteLn('Reading sharded ZIO files: ', Pattern);
  WriteLn;
  
  // Find root message (first message in first proto)
  RootMessage := nil;
  if ProtoMap <> nil then
  begin
    it := ProtoMap.GetEnumerator;
    while it.MoveNext do
    begin
      Proto := it.Current.Value;
      if (Proto.Messages <> nil) and (Proto.Messages.Count > 0) then
      begin
        RootMessage := Proto.Messages[0];
        WriteLn('Using root message type: ', RootMessage.Name);
        Break;
      end;
    end;
    it.Free;
  end;
  
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
        Msg.RootMessageDef := RootMessage;
        Msg.ProtoMap := ProtoMap;
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

procedure DumpSingleFile(const FilePath: string; ProtoMap: TProtoMap);
var
  FileStream: TFileStream;
  ZStream: TZioStream;
  Msg: TGenericMessage;
  MessageCount: Integer;
  RootMessage: TMessage;
  Proto: TProto;
  it: TProtoMap.TPairEnumerator;
begin
  WriteLn('Reading single ZIO file: ', FilePath);
  WriteLn;
  
  if not FileExists(FilePath) then
  begin
    WriteLn('ERROR: File not found: ', FilePath);
    Halt(1);
  end;
  
  // Find root message (first message in first proto)
  RootMessage := nil;
  if ProtoMap <> nil then
  begin
    it := ProtoMap.GetEnumerator;
    while it.MoveNext do
    begin
      Proto := it.Current.Value;
      if (Proto.Messages <> nil) and (Proto.Messages.Count > 0) then
      begin
        RootMessage := Proto.Messages[0];
        WriteLn('Using root message type: ', RootMessage.Name);
        Break;
      end;
    end;
    it.Free;
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
        Msg.RootMessageDef := RootMessage;
        Msg.ProtoMap := ProtoMap;
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
  Params: TParam;
  Pattern: string;
  ProtoMap: TProtoMap;

begin
  WriteLn('===========================================');
  WriteLn('  ZIO Dump Utility');
  WriteLn('===========================================');
  WriteLn;
  
  Params := TParam.Create;
  try
    ParamManagerUnit.InitAndParse('Verbosity=0', Params);
    ParamManagerUnit.InitFromParameters(Params);
    
    ALoggerUnit.InitLogger(Params.Verbosity.Value);
    
    if Params.InputFileName.Value = '' then
    begin
      PrintUsage;
      Halt(1);
    end;
    
    Pattern := Params.InputFileName.Value;
    
    // Parse proto file if provided
    ProtoMap := nil;
    if Params.ProtoFile.Value <> '' then
    begin
      WriteLn('Loading proto definitions from: ', Params.ProtoFile.Value);
      try
        ProtoMap := TBaseProtoParser.ParseAll(Params.ProtoFile.Value);
        WriteLn('Proto file loaded successfully.');
        WriteLn;
      except
        on E: Exception do
        begin
          WriteLn('WARNING: Failed to load proto file: ', E.Message);
          WriteLn('Continuing without type information...');
          WriteLn;
          ProtoMap := nil;
        end;
      end;
    end;
    
    try
      // Check if it's a sharded pattern (contains '@')
      if Pos('@', Pattern) > 0 then
        DumpShardedFiles(Pattern, ProtoMap)
      else
        DumpSingleFile(Pattern, ProtoMap);
      
      WriteLn;
      WriteLn('Done.');
    finally
      if ProtoMap <> nil then
        ProtoMap.Free;
    end;
  finally
    Params.Free;
  end;
end.
