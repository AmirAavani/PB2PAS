unit SampleUnit;
{$Mode objfpc}
interface

uses 
    classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit;

type


  TParams = Class(TBaseMessage)
  // Methods for repeated string param_str = 1;
  private
    FParamStr: specialize TSimpleTypeList<AnsiString>;

    // Getter Functions
    function GetParamStr(Index: Integer): AnsiString;
    function GetAllParamStr: specialize TSimpleTypeList<AnsiString>;
    function GetOrCreateAllParamStr: specialize TSimpleTypeList<AnsiString>;

  public
    property ParamStr[Index: Integer]: AnsiString read GetParamStr;
    property ConstAllParamStr: specialize TSimpleTypeList<AnsiString> read GetAllParamStr;
    property AllParamStr: specialize TSimpleTypeList<AnsiString> read GetOrCreateAllParamStr;

  // Declarations for int32 a32 = 2;
  private
    FA32: Int32;
  public
    property A32: Int32 read FA32 write FA32;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public 
    constructor Create;
    constructor Create(aParamStr: specialize TSimpleTypeList<AnsiString>; aA32: Int32);
    destructor Destroy; override;
    function ToString: AnsiString; override;
 
  end;

  TTestID = Class(TBaseMessage)
  // Methods for repeated uint64 uid64s = 1;
  private
    FUid64s: specialize TSimpleTypeList<UInt64>;

    // Getter Functions
    function GetUid64s(Index: Integer): UInt64;
    function GetAllUid64s: specialize TSimpleTypeList<UInt64>;
    function GetOrCreateAllUid64s: specialize TSimpleTypeList<UInt64>;

  public
    property Uid64s[Index: Integer]: UInt64 read GetUid64s;
    property ConstAllUid64s: specialize TSimpleTypeList<UInt64> read GetAllUid64s;
    property AllUid64s: specialize TSimpleTypeList<UInt64> read GetOrCreateAllUid64s;

  // Methods for repeated uint32 uid32s = 2;
  private
    FUid32s: specialize TSimpleTypeList<UInt32>;

    // Getter Functions
    function GetUid32s(Index: Integer): UInt32;
    function GetAllUid32s: specialize TSimpleTypeList<UInt32>;
    function GetOrCreateAllUid32s: specialize TSimpleTypeList<UInt32>;

  public
    property Uid32s[Index: Integer]: UInt32 read GetUid32s;
    property ConstAllUid32s: specialize TSimpleTypeList<UInt32> read GetAllUid32s;
    property AllUid32s: specialize TSimpleTypeList<UInt32> read GetOrCreateAllUid32s;

  // Methods for repeated int64 id64s = 3;
  private
    FId64s: specialize TSimpleTypeList<Int64>;

    // Getter Functions
    function GetId64s(Index: Integer): Int64;
    function GetAllId64s: specialize TSimpleTypeList<Int64>;
    function GetOrCreateAllId64s: specialize TSimpleTypeList<Int64>;

  public
    property Id64s[Index: Integer]: Int64 read GetId64s;
    property ConstAllId64s: specialize TSimpleTypeList<Int64> read GetAllId64s;
    property AllId64s: specialize TSimpleTypeList<Int64> read GetOrCreateAllId64s;

  // Declarations for uint64 uid64 = 5;
  private
    FUid64: UInt64;
  public
    property Uid64: UInt64 read FUid64 write FUid64;

  // Declarations for uint32 uid32 = 6;
  private
    FUid32: UInt32;
  public
    property Uid32: UInt32 read FUid32 write FUid32;

  // Declarations for int64 id64 = 7;
  private
    FId64: Int64;
  public
    property Id64: Int64 read FId64 write FId64;

  // Declarations for int32 id32 = 8;
  private
    FId32: Int32;
  public
    property Id32: Int32 read FId32 write FId32;

  // Declarations for string idStr = 10;
  private
    FIdStr: AnsiString;
  public
    property IdStr: AnsiString read FIdStr write FIdStr;

  // Declarations for Params parameter = 11;
  private
    FParameter: TParams;
  public
    property Parameter: TParams read FParameter write FParameter;

  // Methods for repeated Params parameters = 12;
  private
    FParameters: specialize TObjectList<TParams>;

    // Getter Functions
    function GetParameters(Index: Integer): TParams;
    function GetAllParameters: specialize TObjectList<TParams>;
    function GetOrCreateAllParameters: specialize TObjectList<TParams>;

  public
    property Parameters[Index: Integer]: TParams read GetParameters;
    property ConstAllParameters: specialize TObjectList<TParams> read GetAllParameters;
    property AllParameters: specialize TObjectList<TParams> read GetOrCreateAllParameters;

  // Declarations for bool idBool = 13;
  private
    FIdBool: Boolean;
  public
    property IdBool: Boolean read FIdBool write FIdBool;

  // Methods for repeated float idFloats = 14;
  private
    FIdFloats: specialize TSimpleTypeList<Single>;

    // Getter Functions
    function GetIdFloats(Index: Integer): Single;
    function GetAllIdFloats: specialize TSimpleTypeList<Single>;
    function GetOrCreateAllIdFloats: specialize TSimpleTypeList<Single>;

  public
    property IdFloats[Index: Integer]: Single read GetIdFloats;
    property ConstAllIdFloats: specialize TSimpleTypeList<Single> read GetAllIdFloats;
    property AllIdFloats: specialize TSimpleTypeList<Single> read GetOrCreateAllIdFloats;

  // Declarations for float idFloat = 15;
  private
    FIdFloat: Single;
  public
    property IdFloat: Single read FIdFloat write FIdFloat;

  // Declarations for double idDouble = 16;
  private
    FIdDouble: Double;
  public
    property IdDouble: Double read FIdDouble write FIdDouble;

  // Methods for repeated string idStrs = 17;
  private
    FIdStrs: specialize TSimpleTypeList<AnsiString>;

    // Getter Functions
    function GetIdStrs(Index: Integer): AnsiString;
    function GetAllIdStrs: specialize TSimpleTypeList<AnsiString>;
    function GetOrCreateAllIdStrs: specialize TSimpleTypeList<AnsiString>;

  public
    property IdStrs[Index: Integer]: AnsiString read GetIdStrs;
    property ConstAllIdStrs: specialize TSimpleTypeList<AnsiString> read GetAllIdStrs;
    property AllIdStrs: specialize TSimpleTypeList<AnsiString> read GetOrCreateAllIdStrs;

  // Methods for repeated bool idBools = 18;
  private
    FIdBools: specialize TSimpleTypeList<Boolean>;

    // Getter Functions
    function GetIdBools(Index: Integer): Boolean;
    function GetAllIdBools: specialize TSimpleTypeList<Boolean>;
    function GetOrCreateAllIdBools: specialize TSimpleTypeList<Boolean>;

  public
    property IdBools[Index: Integer]: Boolean read GetIdBools;
    property ConstAllIdBools: specialize TSimpleTypeList<Boolean> read GetAllIdBools;
    property AllIdBools: specialize TSimpleTypeList<Boolean> read GetOrCreateAllIdBools;

  // Methods for repeated double idDoubles = 19;
  private
    FIdDoubles: specialize TSimpleTypeList<Double>;

    // Getter Functions
    function GetIdDoubles(Index: Integer): Double;
    function GetAllIdDoubles: specialize TSimpleTypeList<Double>;
    function GetOrCreateAllIdDoubles: specialize TSimpleTypeList<Double>;

  public
    property IdDoubles[Index: Integer]: Double read GetIdDoubles;
    property ConstAllIdDoubles: specialize TSimpleTypeList<Double> read GetAllIdDoubles;
    property AllIdDoubles: specialize TSimpleTypeList<Double> read GetOrCreateAllIdDoubles;

  // Methods for repeated int32 id32s = 40;
  private
    FId32s: specialize TSimpleTypeList<Int32>;

    // Getter Functions
    function GetId32s(Index: Integer): Int32;
    function GetAllId32s: specialize TSimpleTypeList<Int32>;
    function GetOrCreateAllId32s: specialize TSimpleTypeList<Int32>;

  public
    property Id32s[Index: Integer]: Int32 read GetId32s;
    property ConstAllId32s: specialize TSimpleTypeList<Int32> read GetAllId32s;
    property AllId32s: specialize TSimpleTypeList<Int32> read GetOrCreateAllId32s;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public 
    constructor Create;
    constructor Create(aUid64s: specialize TSimpleTypeList<UInt64>; aUid32s: specialize TSimpleTypeList<UInt32>; aId64s: specialize TSimpleTypeList<Int64>; aUid64: UInt64; aUid32: UInt32; aId64: Int64; aId32: Int32; aIdStr: AnsiString; aParameter: TParams; aParameters: specialize TObjectList<TParams>; aIdBool: Boolean; aIdFloats: specialize TSimpleTypeList<Single>; aIdFloat: Single; aIdDouble: Double; aIdStrs: specialize TSimpleTypeList<AnsiString>; aIdBools: specialize TSimpleTypeList<Boolean>; aIdDoubles: specialize TSimpleTypeList<Double>; aId32s: specialize TSimpleTypeList<Int32>);
    destructor Destroy; override;
    function ToString: AnsiString; override;
 
  end;


implementation

uses strutils;

 { TParams }

// Methods for repeated string param_str = 1;
// Getter Functions

function TParams.GetParamStr(Index: Integer): AnsiString;
begin
  Result := FParamStr[Index];

end;

function TParams.GetAllParamStr: specialize TSimpleTypeList<AnsiString>;
begin
  if Self = nil then
    Exit(nil);
  Result := FParamStr;

end;

function TParams.GetOrCreateAllParamStr: specialize TSimpleTypeList<AnsiString>;

begin
  if FParamStr = nil then
    FParamStr := specialize TSimpleTypeList<AnsiString>.Create('%s');
  Result := FParamStr;

end;


constructor TParams.Create;
begin
  inherited Create;

  FA32 := 0;
end;

constructor TParams.Create(aParamStr: specialize TSimpleTypeList<AnsiString>; aA32: Int32);
begin
  inherited Create;

  FParamStr := aParamStr; 
  FA32 := aA32; 

end;

destructor TParams.Destroy;
begin
  FParamStr.Free;

  inherited;
end;

function TParams.ToString: AnsiString;
begin
  Result := '';

  if FParamStr <> nil then
  begin
    Result += 'param_str = ';
    Result += FParamStr.ToString;
    Result += sLineBreak;
  end;

  if FA32 <> 0 then
  begin
    Result += Format('a32: %d ', [FA32]);
    Result += sLineBreak;
  end;


end;

procedure TParams.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveRepeatedAnsiString(Stream, FParamStr, 1);

  SaveInt32(Stream, FA32, 2);

end;

function TParams.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedAnsiString(Stream, GetOrCreateAllParamStr);
    end;


    2: FA32 := LoadInt32(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;
end;


 { TTestID }

// Methods for repeated uint64 uid64s = 1;
// Getter Functions

function TTestID.GetUid64s(Index: Integer): UInt64;
begin
  Result := FUid64s[Index];

end;

function TTestID.GetAllUid64s: specialize TSimpleTypeList<UInt64>;
begin
  if Self = nil then
    Exit(nil);
  Result := FUid64s;

end;

function TTestID.GetOrCreateAllUid64s: specialize TSimpleTypeList<UInt64>;

begin
  if FUid64s = nil then
    FUid64s := specialize TSimpleTypeList<UInt64>.Create('%u');
  Result := FUid64s;

end;


// Methods for repeated uint32 uid32s = 2;
// Getter Functions

function TTestID.GetUid32s(Index: Integer): UInt32;
begin
  Result := FUid32s[Index];

end;

function TTestID.GetAllUid32s: specialize TSimpleTypeList<UInt32>;
begin
  if Self = nil then
    Exit(nil);
  Result := FUid32s;

end;

function TTestID.GetOrCreateAllUid32s: specialize TSimpleTypeList<UInt32>;

begin
  if FUid32s = nil then
    FUid32s := specialize TSimpleTypeList<UInt32>.Create('%u');
  Result := FUid32s;

end;


// Methods for repeated int64 id64s = 3;
// Getter Functions

function TTestID.GetId64s(Index: Integer): Int64;
begin
  Result := FId64s[Index];

end;

function TTestID.GetAllId64s: specialize TSimpleTypeList<Int64>;
begin
  if Self = nil then
    Exit(nil);
  Result := FId64s;

end;

function TTestID.GetOrCreateAllId64s: specialize TSimpleTypeList<Int64>;

begin
  if FId64s = nil then
    FId64s := specialize TSimpleTypeList<Int64>.Create('%d');
  Result := FId64s;

end;


// Methods for repeated Params parameters = 12;
// Getter Functions
function TTestID.GetParameters(Index: Integer): TParams;
begin
  Result := FParameters[Index];end;

function TTestID.GetAllParameters: specialize TObjectList<TParams>;
begin
  if Self = nil then
    Exit(nil);
  if FParameters = nil then
    Exit(nil);
  Result := FParameters; 
end;

function TTestID.GetOrCreateAllParameters: specialize TObjectList<TParams>;

begin
  if Self = nil then
    Exit(nil);
  if Self.FParameters = nil then
    FParameters := specialize TObjectList<TParams>.Create;
  Result := FParameters; 
end;


// Methods for repeated float idFloats = 14;
// Getter Functions

function TTestID.GetIdFloats(Index: Integer): Single;
begin
  Result := FIdFloats[Index];

end;

function TTestID.GetAllIdFloats: specialize TSimpleTypeList<Single>;
begin
  if Self = nil then
    Exit(nil);
  Result := FIdFloats;

end;

function TTestID.GetOrCreateAllIdFloats: specialize TSimpleTypeList<Single>;

begin
  if FIdFloats = nil then
    FIdFloats := specialize TSimpleTypeList<Single>.Create('%e');
  Result := FIdFloats;

end;


// Methods for repeated string idStrs = 17;
// Getter Functions

function TTestID.GetIdStrs(Index: Integer): AnsiString;
begin
  Result := FIdStrs[Index];

end;

function TTestID.GetAllIdStrs: specialize TSimpleTypeList<AnsiString>;
begin
  if Self = nil then
    Exit(nil);
  Result := FIdStrs;

end;

function TTestID.GetOrCreateAllIdStrs: specialize TSimpleTypeList<AnsiString>;

begin
  if FIdStrs = nil then
    FIdStrs := specialize TSimpleTypeList<AnsiString>.Create('%s');
  Result := FIdStrs;

end;


// Methods for repeated bool idBools = 18;
// Getter Functions

function TTestID.GetIdBools(Index: Integer): Boolean;
begin
  Result := FIdBools[Index];

end;

function TTestID.GetAllIdBools: specialize TSimpleTypeList<Boolean>;
begin
  if Self = nil then
    Exit(nil);
  Result := FIdBools;

end;

function TTestID.GetOrCreateAllIdBools: specialize TSimpleTypeList<Boolean>;

begin
  if FIdBools = nil then
    FIdBools := TBooleanList.Create('%d');
  Result := FIdBools;

end;


// Methods for repeated double idDoubles = 19;
// Getter Functions

function TTestID.GetIdDoubles(Index: Integer): Double;
begin
  Result := FIdDoubles[Index];

end;

function TTestID.GetAllIdDoubles: specialize TSimpleTypeList<Double>;
begin
  if Self = nil then
    Exit(nil);
  Result := FIdDoubles;

end;

function TTestID.GetOrCreateAllIdDoubles: specialize TSimpleTypeList<Double>;

begin
  if FIdDoubles = nil then
    FIdDoubles := specialize TSimpleTypeList<Double>.Create('%e');
  Result := FIdDoubles;

end;


// Methods for repeated int32 id32s = 40;
// Getter Functions

function TTestID.GetId32s(Index: Integer): Int32;
begin
  Result := FId32s[Index];

end;

function TTestID.GetAllId32s: specialize TSimpleTypeList<Int32>;
begin
  if Self = nil then
    Exit(nil);
  Result := FId32s;

end;

function TTestID.GetOrCreateAllId32s: specialize TSimpleTypeList<Int32>;

begin
  if FId32s = nil then
    FId32s := specialize TSimpleTypeList<Int32>.Create('%d');
  Result := FId32s;

end;


constructor TTestID.Create;
begin
  inherited Create;

  FUid64 := 0;
  FUid32 := 0;
  FId64 := 0;
  FId32 := 0;
  FIdStr := '';
  FIdBool := False;
  FIdFloat := 0.0;
  FIdDouble := 0.0;
end;

constructor TTestID.Create(aUid64s: specialize TSimpleTypeList<UInt64>; aUid32s: specialize TSimpleTypeList<UInt32>; aId64s: specialize TSimpleTypeList<Int64>; aUid64: UInt64; aUid32: UInt32; aId64: Int64; aId32: Int32; aIdStr: AnsiString; aParameter: TParams; aParameters: specialize TObjectList<TParams>; aIdBool: Boolean; aIdFloats: specialize TSimpleTypeList<Single>; aIdFloat: Single; aIdDouble: Double; aIdStrs: specialize TSimpleTypeList<AnsiString>; aIdBools: specialize TSimpleTypeList<Boolean>; aIdDoubles: specialize TSimpleTypeList<Double>; aId32s: specialize TSimpleTypeList<Int32>);
begin
  inherited Create;

  FUid64s := aUid64s; 
  FUid32s := aUid32s; 
  FId64s := aId64s; 
  FUid64 := aUid64; 
  FUid32 := aUid32; 
  FId64 := aId64; 
  FId32 := aId32; 
  FIdStr := aIdStr; 
  FParameter := aParameter; 
  FParameters := aParameters; 
  FIdBool := aIdBool; 
  FIdFloats := aIdFloats; 
  FIdFloat := aIdFloat; 
  FIdDouble := aIdDouble; 
  FIdStrs := aIdStrs; 
  FIdBools := aIdBools; 
  FIdDoubles := aIdDoubles; 
  FId32s := aId32s; 

end;

destructor TTestID.Destroy;
begin
  FUid64s.Free;
  FUid32s.Free;
  FId64s.Free;
  FParameter.Free;
  FParameters.Free;
  FIdFloats.Free;
  FIdStrs.Free;
  FIdBools.Free;
  FIdDoubles.Free;
  FId32s.Free;

  inherited;
end;

function TTestID.ToString: AnsiString;
var
  BaseMessage: TBaseMessage;

begin
  Result := '';

  if FUid64s <> nil then
  begin
    Result += 'uid64s = ';
    Result += FUid64s.ToString;
    Result += sLineBreak;
  end;

  if FUid32s <> nil then
  begin
    Result += 'uid32s = ';
    Result += FUid32s.ToString;
    Result += sLineBreak;
  end;

  if FId64s <> nil then
  begin
    Result += 'id64s = ';
    Result += FId64s.ToString;
    Result += sLineBreak;
  end;

  if FUid64 <> 0 then
  begin
    Result += Format('uid64: %u ', [FUid64]);
    Result += sLineBreak;
  end;

  if FUid32 <> 0 then
  begin
    Result += Format('uid32: %u ', [FUid32]);
    Result += sLineBreak;
  end;

  if FId64 <> 0 then
  begin
    Result += Format('id64: %d ', [FId64]);
    Result += sLineBreak;
  end;

  if FId32 <> 0 then
  begin
    Result += Format('id32: %d ', [FId32]);
    Result += sLineBreak;
  end;

  if FIdStr <> '' then
  begin
    Result += Format('idStr: %s ', [FIdStr]);
    Result += sLineBreak;
  end;

  if FParameter <> nil then
  begin
    Result += 'parameter = ';
    Result += FParameter.ToString;
    Result += sLineBreak;
  end;

    if FParameters <> nil then
    begin
      Result += 'parameters  = ';
      for BaseMessage in FParameters do
        Result += Format('[%s]', [BaseMessage.ToString]);
      Result += sLineBreak;
    end;
  if FIdBool then
    Result += Format('FIdBool: %s', [IfThen(FIdBool, 'True', 'False')]) + sLineBreak;

  if FIdFloats <> nil then
  begin
    Result += 'idFloats = ';
    Result += FIdFloats.ToString;
    Result += sLineBreak;
  end;

  if FIdFloat <> 0.0 then
  begin
    Result += Format('idFloat: %e ', [FIdFloat]);
    Result += sLineBreak;
  end;

  if FIdDouble <> 0.0 then
  begin
    Result += Format('idDouble: %e ', [FIdDouble]);
    Result += sLineBreak;
  end;

  if FIdStrs <> nil then
  begin
    Result += 'idStrs = ';
    Result += FIdStrs.ToString;
    Result += sLineBreak;
  end;

  if FIdBools <> nil then
  begin
    Result += 'idBools = ';
    Result += FIdBools.ToString;
    Result += sLineBreak;
  end;

  if FIdDoubles <> nil then
  begin
    Result += 'idDoubles = ';
    Result += FIdDoubles.ToString;
    Result += sLineBreak;
  end;

  if FId32s <> nil then
  begin
    Result += 'id32s = ';
    Result += FId32s.ToString;
    Result += sLineBreak;
  end;


end;

procedure TTestID.SaveToStream(Stream: TProtoStreamWriter);
var
  SizeNode: TLinkListNode;
  BaseMessage: TBaseMessage;

begin
  SaveRepeatedUInt64(Stream, FUid64s, 1);

  SaveRepeatedUInt32(Stream, FUid32s, 2);

  SaveRepeatedInt64(Stream, FId64s, 3);

  SaveUInt64(Stream, FUid64, 5);

  SaveUInt32(Stream, FUid32, 6);

  SaveInt64(Stream, FId64, 7);

  SaveInt32(Stream, FId32, 8);

  SaveAnsiString(Stream, FIdStr, 10);

  SaveMessage(Stream, FParameter, 11);

  if FParameters <> nil then
    for BaseMessage in FParameters do
      SaveMessage(Stream, BaseMessage, 12);


  SaveBoolean(Stream, FIdBool, 13);

  SaveRepeatedSingle(Stream, FIdFloats, 14);

  SaveSingle(Stream, FIdFloat, 15);

  SaveDouble(Stream, FIdDouble, 16);

  SaveRepeatedAnsiString(Stream, FIdStrs, 17);

  SaveRepeatedBoolean(Stream, FIdBools, 18);

  SaveRepeatedDouble(Stream, FIdDoubles, 19);

  SaveRepeatedInt32(Stream, FId32s, 40);

end;

function TTestID.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

  BaseMessage: TBaseMessage;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedUInt64(Stream, GetOrCreateAllUid64s);
    end;


    2:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedUInt32(Stream, GetOrCreateAllUid32s);
    end;


    3:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedInt64(Stream, GetOrCreateAllId64s);
    end;


    5: FUid64 := LoadUInt64(Stream);


    6: FUid32 := LoadUInt32(Stream);


    7: FId64 := LoadInt64(Stream);


    8: FId32 := LoadInt32(Stream);


    10: FIdStr := LoadAnsiString(Stream);


    11:
    begin
      if WireType <> 2 then
        Exit(False);
      FParameter := TParams.Create;
      if not LoadMessage(Stream, FParameter) then
        Exit(False);
    end;


    12:
    begin
      if WireType <> 2 then
        Exit(False);
      GetOrCreateAllParameters.Add(TParams.Create);
      if not LoadMessage(Stream, FParameters.Last) then
        Exit(False);
    end;



    13: FIdBool := LoadBoolean(Stream);


    14:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedSingle(Stream, GetOrCreateAllIdFloats);
    end;


    15: FIdFloat := LoadSingle(Stream);


    16: FIdDouble := LoadDouble(Stream);


    17:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedAnsiString(Stream, GetOrCreateAllIdStrs);
    end;


    18:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedBoolean(Stream, GetOrCreateAllIdBools);
    end;


    19:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedDouble(Stream, GetOrCreateAllIdDoubles);
    end;


    40:
    begin

      if WireType <> 2 then
        Exit(False);
      LoadRepeatedInt32(Stream, GetOrCreateAllId32s);
    end;


    end;
  end;

  Result := StartPos + Len = Stream.Position;
end;

end.
