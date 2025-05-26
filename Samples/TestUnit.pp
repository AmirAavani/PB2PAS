unit TestUnit;
{$Mode objfpc}

interface

uses
 TestDep1Unit, classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TMyMessage = class;

  // message MyMessage
  { TMyMessage }
  TMyMessage = class(TBaseMessage)
  type
    // AnEnum
    TAnEnum = (
      ANENUM_NONE = 0,
      ANENUM_V1 = 1,
      ANENUM_V4 = 4
    );

  // Forward Declarations.
  public type
    TMyOneOf = class;
  public type
    TA = class;
    TSubMessage = class;

  public type
    TMyOneOf = Class(TBaseOneOf)
    private
      function GetAnEnum: TAnEnum;
      procedure SetAnEnum(_AnEnum: TAnEnum);
      function GetDeptestAnEnum: TestDep1Unit.TAnEnum;
      procedure SetDeptestAnEnum(_DeptestAnEnum: TestDep1Unit.TAnEnum);
      function GetIsCountry: AnsiString;
      procedure SetIsCountry(_IsCountry: AnsiString);
      function GetIsState: AnsiString;
      procedure SetIsState(_IsState: AnsiString);
      function GetIsCounty: AnsiString;
      procedure SetIsCounty(_IsCounty: AnsiString);
      function GetIsCity: AnsiString;
      procedure SetIsCity(_IsCity: AnsiString);
      function GetIsStreet: AnsiString;
      procedure SetIsStreet(_IsStreet: AnsiString);
      function GetIsStr: AnsiString;
      procedure SetIsStr(_IsStr: AnsiString);
      function GetIsI: Int32;
      procedure SetIsI(_IsI: Int32);
      function GetIsF: Single;
      procedure SetIsF(_IsF: Single);
      function GetIsD: Double;
      procedure SetIsD(_IsD: Double);
      function GetIsA: TA;
      procedure SetIsA(_IsA: TA);
      function GetIsB: Boolean;
      procedure SetIsB(_IsB: Boolean);

    public
      property AnEnum: TAnEnum read GetAnEnum write SetAnEnum;
      property DeptestAnEnum: TestDep1Unit.TAnEnum read GetDeptestAnEnum write SetDeptestAnEnum;
      property IsCountry: AnsiString read GetIsCountry write SetIsCountry;
      property IsState: AnsiString read GetIsState write SetIsState;
      property IsCounty: AnsiString read GetIsCounty write SetIsCounty;
      property IsCity: AnsiString read GetIsCity write SetIsCity;
      property IsStreet: AnsiString read GetIsStreet write SetIsStreet;
      property IsStr: AnsiString read GetIsStr write SetIsStr;
      property IsI: Int32 read GetIsI write SetIsI;
      property IsF: Single read GetIsF write SetIsF;
      property IsD: Double read GetIsD write SetIsD;
      property IsA: TA read GetIsA write SetIsA;
      property IsB: Boolean read GetIsB write SetIsB;

      constructor Create;
      destructor Destroy; override;
      procedure Clear; override;

    end;

  public type
    // message A
    { TA }
    TA = class(TBaseMessage)
    // Forward Declarations.

    private
      FAId: Int32;

    public
      function GetAId: Int32;

    public
      // int32 a_id = 1;
      property AId: Int32 read FAId write FAId;

    private
      FASubMessage: TSubMessage;

    public
      function GetASubMessage: TSubMessage;
      function GetOrCreateASubMessage: TSubMessage;

    public
      // SubMessage a_sub_message = 2;
      property ASubMessage: TSubMessage read FASubMessage write FASubMessage;
      property ConstASubMessage: TSubMessage read GetASubMessage;
      property MutableASubMessage: TSubMessage read GetOrCreateASubMessage;

    protected 
      procedure SaveToStream(Stream: TProtoStreamWriter); override;
      function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear; override;

    public // functions
    end;

  public type
    // message SubMessage
    { TSubMessage }
    TSubMessage = class(TBaseMessage)
    // Forward Declarations.

    private
      FId: Int32;

    public
      function GetId: Int32;

    public
      // int32 id = 1;
      property Id: Int32 read FId write FId;

    protected 
      procedure SaveToStream(Stream: TProtoStreamWriter); override;
      function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear; override;

    public // functions
    end;

  public type
    TInt32ToAMap = class(specialize TMapSimpleKeyObjectValue<Int32, TA>)

  private
    function LoadFromStream(Stream: TProtoStreamReader): Boolean;
    procedure SaveToStream(Stream: TProtoStreamWriter);

  end;

  public type
    TInt32ToSubMessageMap = class(specialize TMapSimpleKeyObjectValue<Int32, TSubMessage>)

  private
    function LoadFromStream(Stream: TProtoStreamReader): Boolean;
    procedure SaveToStream(Stream: TProtoStreamWriter);

  end;

  public type
    TInt32ToInt32Map = class(specialize TMap<Int32, Int32>)

  private
    function LoadFromStream(Stream: TProtoStreamReader): Boolean;
    procedure SaveToStream(Stream: TProtoStreamWriter);

  end;

  private
    FA: TA;

  public
    function GetA: TA;
    function GetOrCreateA: TA;

  public
    // A a = 1100;
    property A: TA read FA write FA;
    property ConstA: TA read GetA;
    property MutableA: TA read GetOrCreateA;

  private
    FIdAMap: TInt32ToAMap;

  public
    function GetIdAMap: TInt32ToAMap;
    function GetOrCreateIdAMap: TInt32ToAMap;

  public
    // map<int32, A> id_a_map = 1101;
    property IdAMap: TInt32ToAMap read FIdAMap write FIdAMap;
    property ConstIdAMap: TInt32ToAMap read GetIdAMap;
    property MutableIdAMap: TInt32ToAMap read GetOrCreateIdAMap;

  private
    FMyOneOf: TMyOneOf;

  public
    function GetMyOneOf: TMyOneOf;
    function GetOrCreateMyOneOf: TMyOneOf;

  public
    // MyOneOf MyOneOf = -1;
    property MyOneOf: TMyOneOf read FMyOneOf write FMyOneOf;
    property ConstMyOneOf: TMyOneOf read GetMyOneOf;
    property MutableMyOneOf: TMyOneOf read GetOrCreateMyOneOf;

  public type
    TRepStr =  TAnsiStrings;

  private
    FRepStr: TRepStr;


  public
    function GetRepStr: TRepStr;
    function GetOrCreateRepStr: TRepStr;

  public
    // repeated string rep_str = 122;
    property RepStr: TRepStr read FRepStr write FRepStr;
    property ConstRepStr: TRepStr read GetRepStr;
    property MutableRepStr: TRepStr read GetOrCreateRepStr write FRepStr;

  public type
    TRepI =  TInt32s;

  private
    FRepI: TRepI;


  public
    function GetRepI: TRepI;
    function GetOrCreateRepI: TRepI;

  public
    // repeated int32 rep_i = 123;
    property RepI: TRepI read FRepI write FRepI;
    property ConstRepI: TRepI read GetRepI;
    property MutableRepI: TRepI read GetOrCreateRepI write FRepI;

  public type
    TRefF =  TSingles;

  private
    FRefF: TRefF;


  public
    function GetRefF: TRefF;
    function GetOrCreateRefF: TRefF;

  public
    // repeated float ref_f = 124;
    property RefF: TRefF read FRefF write FRefF;
    property ConstRefF: TRefF read GetRefF;
    property MutableRefF: TRefF read GetOrCreateRefF write FRefF;

  public type
    TRepD =  TDoubles;

  private
    FRepD: TRepD;


  public
    function GetRepD: TRepD;
    function GetOrCreateRepD: TRepD;

  public
    // repeated double rep_d = 125;
    property RepD: TRepD read FRepD write FRepD;
    property ConstRepD: TRepD read GetRepD;
    property MutableRepD: TRepD read GetOrCreateRepD write FRepD;

  public type
    TRepA = specialize TObjectList<TA>;

  private
    FRepA: TRepA;

  public
    function GetRepA: TRepA;
    function GetOrCreateRepA: TRepA;

  public
    // repeated A rep_a = 126;
    property RepA: TRepA read FRepA write FRepA;
    property ConstRepA: TRepA read GetRepA;
    property MutableRepA: TRepA read GetOrCreateRepA write FRepA;

  private
    FDd: Double;

  public
    function GetDd: Double;

  public
    // double dd = 200;
    property Dd: Double read FDd write FDd;

  private
    FF: Single;

  public
    function GetF: Single;

  public
    // float f = 201;
    property F: Single read FF write FF;

  private
    FI32: Int32;

  public
    function GetI32: Int32;

  public
    // int32 i32 = 202;
    property I32: Int32 read FI32 write FI32;

  private
    FI64: Int64;

  public
    function GetI64: Int64;

  public
    // int64 i64 = 203;
    property I64: Int64 read FI64 write FI64;

  private
    FUi32: UInt32;

  public
    function GetUi32: UInt32;

  public
    // uint32 ui32 = 204;
    property Ui32: UInt32 read FUi32 write FUi32;

  private
    FUi64: UInt64;

  public
    function GetUi64: UInt64;

  public
    // uint64 ui64 = 205;
    property Ui64: UInt64 read FUi64 write FUi64;

  private
    FSi32: Int32;

  public
    function GetSi32: Int32;

  public
    // sint32 si32 = 206;
    property Si32: Int32 read FSi32 write FSi32;

  private
    FSi64: Int64;

  public
    function GetSi64: Int64;

  public
    // sint64 si64 = 207;
    property Si64: Int64 read FSi64 write FSi64;

  private
    FF32: UInt32;

  public
    function GetF32: UInt32;

  public
    // fixed32 f32 = 208;
    property F32: UInt32 read FF32 write FF32;

  private
    FF64: UInt64;

  public
    function GetF64: UInt64;

  public
    // fixed64 f64 = 209;
    property F64: UInt64 read FF64 write FF64;

  private
    FS32: Int32;

  public
    function GetS32: Int32;

  public
    // sfixed32 s32 = 210;
    property S32: Int32 read FS32 write FS32;

  private
    FS64: Int64;

  public
    function GetS64: Int64;

  public
    // sfixed64 s64 = 211;
    property S64: Int64 read FS64 write FS64;

  private
    FB: Boolean;

  public
    function GetB: Boolean;

  public
    // bool _b = 212;
    property B: Boolean read FB write FB;

  private
    FAnS: AnsiString;

  public
    function GetAnS: AnsiString;

  public
    // string an_s = 213;
    property AnS: AnsiString read FAnS write FAnS;

  public type
    TRdd =  TDoubles;

  private
    FRdd: TRdd;


  public
    function GetRdd: TRdd;
    function GetOrCreateRdd: TRdd;

  public
    // repeated double rdd = 300;
    property Rdd: TRdd read FRdd write FRdd;
    property ConstRdd: TRdd read GetRdd;
    property MutableRdd: TRdd read GetOrCreateRdd write FRdd;

  public type
    TRf =  TSingles;

  private
    FRf: TRf;


  public
    function GetRf: TRf;
    function GetOrCreateRf: TRf;

  public
    // repeated float rf = 301;
    property Rf: TRf read FRf write FRf;
    property ConstRf: TRf read GetRf;
    property MutableRf: TRf read GetOrCreateRf write FRf;

  public type
    TRi32 =  TInt32s;

  private
    FRi32: TRi32;


  public
    function GetRi32: TRi32;
    function GetOrCreateRi32: TRi32;

  public
    // repeated int32 ri32 = 302;
    property Ri32: TRi32 read FRi32 write FRi32;
    property ConstRi32: TRi32 read GetRi32;
    property MutableRi32: TRi32 read GetOrCreateRi32 write FRi32;

  public type
    TRi64 =  TInt64s;

  private
    FRi64: TRi64;


  public
    function GetRi64: TRi64;
    function GetOrCreateRi64: TRi64;

  public
    // repeated int64 ri64 = 303;
    property Ri64: TRi64 read FRi64 write FRi64;
    property ConstRi64: TRi64 read GetRi64;
    property MutableRi64: TRi64 read GetOrCreateRi64 write FRi64;

  public type
    TRui32 =  TUInt32s;

  private
    FRui32: TRui32;


  public
    function GetRui32: TRui32;
    function GetOrCreateRui32: TRui32;

  public
    // repeated uint32 rui32 = 304;
    property Rui32: TRui32 read FRui32 write FRui32;
    property ConstRui32: TRui32 read GetRui32;
    property MutableRui32: TRui32 read GetOrCreateRui32 write FRui32;

  public type
    TRui64 =  TUInt64s;

  private
    FRui64: TRui64;


  public
    function GetRui64: TRui64;
    function GetOrCreateRui64: TRui64;

  public
    // repeated uint64 rui64 = 305;
    property Rui64: TRui64 read FRui64 write FRui64;
    property ConstRui64: TRui64 read GetRui64;
    property MutableRui64: TRui64 read GetOrCreateRui64 write FRui64;

  public type
    TRsi32 =  TInt32s;

  private
    FRsi32: TRsi32;


  public
    function GetRsi32: TRsi32;
    function GetOrCreateRsi32: TRsi32;

  public
    // repeated sint32 rsi32 = 306;
    property Rsi32: TRsi32 read FRsi32 write FRsi32;
    property ConstRsi32: TRsi32 read GetRsi32;
    property MutableRsi32: TRsi32 read GetOrCreateRsi32 write FRsi32;

  public type
    TRsi64 =  TInt64s;

  private
    FRsi64: TRsi64;


  public
    function GetRsi64: TRsi64;
    function GetOrCreateRsi64: TRsi64;

  public
    // repeated sint64 rsi64 = 307;
    property Rsi64: TRsi64 read FRsi64 write FRsi64;
    property ConstRsi64: TRsi64 read GetRsi64;
    property MutableRsi64: TRsi64 read GetOrCreateRsi64 write FRsi64;

  public type
    TRf32 =  TUInt32s;

  private
    FRf32: TRf32;


  public
    function GetRf32: TRf32;
    function GetOrCreateRf32: TRf32;

  public
    // repeated fixed32 rf32 = 308;
    property Rf32: TRf32 read FRf32 write FRf32;
    property ConstRf32: TRf32 read GetRf32;
    property MutableRf32: TRf32 read GetOrCreateRf32 write FRf32;

  public type
    TRf64 =  TUInt64s;

  private
    FRf64: TRf64;


  public
    function GetRf64: TRf64;
    function GetOrCreateRf64: TRf64;

  public
    // repeated fixed64 rf64 = 309;
    property Rf64: TRf64 read FRf64 write FRf64;
    property ConstRf64: TRf64 read GetRf64;
    property MutableRf64: TRf64 read GetOrCreateRf64 write FRf64;

  public type
    TRs32 =  TInt32s;

  private
    FRs32: TRs32;


  public
    function GetRs32: TRs32;
    function GetOrCreateRs32: TRs32;

  public
    // repeated sfixed32 rs32 = 310;
    property Rs32: TRs32 read FRs32 write FRs32;
    property ConstRs32: TRs32 read GetRs32;
    property MutableRs32: TRs32 read GetOrCreateRs32 write FRs32;

  public type
    TRs64 =  TInt64s;

  private
    FRs64: TRs64;


  public
    function GetRs64: TRs64;
    function GetOrCreateRs64: TRs64;

  public
    // repeated sfixed64 rs64 = 311;
    property Rs64: TRs64 read FRs64 write FRs64;
    property ConstRs64: TRs64 read GetRs64;
    property MutableRs64: TRs64 read GetOrCreateRs64 write FRs64;

  public type
    TRb =  TBooleans;

  private
    FRb: TRb;


  public
    function GetRb: TRb;
    function GetOrCreateRb: TRb;

  public
    // repeated bool rb = 312;
    property Rb: TRb read FRb write FRb;
    property ConstRb: TRb read GetRb;
    property MutableRb: TRb read GetOrCreateRb write FRb;

  public type
    TRs =  TAnsiStrings;

  private
    FRs: TRs;


  public
    function GetRs: TRs;
    function GetOrCreateRs: TRs;

  public
    // repeated string rs = 313;
    property Rs: TRs read FRs write FRs;
    property ConstRs: TRs read GetRs;
    property MutableRs: TRs read GetOrCreateRs write FRs;

  public type
    TAenums =  TInt32s;

  private
    FAenums: TAenums;


  public
    function GetAenums: TAenums;
    function GetOrCreateAenums: TAenums;

  public
    // repeated AnEnum aenums = 314;
    property Aenums: TAenums read FAenums write FAenums;
    property ConstAenums: TAenums read GetAenums;
    property MutableAenums: TAenums read GetOrCreateAenums write FAenums;

  private
    FIdSubMessageMap: TInt32ToSubMessageMap;

  public
    function GetIdSubMessageMap: TInt32ToSubMessageMap;
    function GetOrCreateIdSubMessageMap: TInt32ToSubMessageMap;

  public
    // map<int32, SubMessage> id_sub_message_map = 420;
    property IdSubMessageMap: TInt32ToSubMessageMap read FIdSubMessageMap write FIdSubMessageMap;
    property ConstIdSubMessageMap: TInt32ToSubMessageMap read GetIdSubMessageMap;
    property MutableIdSubMessageMap: TInt32ToSubMessageMap read GetOrCreateIdSubMessageMap;

  private
    FAnIntIntMap: TInt32ToInt32Map;

  public
    function GetAnIntIntMap: TInt32ToInt32Map;
    function GetOrCreateAnIntIntMap: TInt32ToInt32Map;

  public
    // map<int32, int32> an_int_int_map = 421;
    property AnIntIntMap: TInt32ToInt32Map read FAnIntIntMap write FAnIntIntMap;
    property ConstAnIntIntMap: TInt32ToInt32Map read GetAnIntIntMap;
    property MutableAnIntIntMap: TInt32ToInt32Map read GetOrCreateAnIntIntMap;

  private
    FBIntIntMap: TInt32ToInt32Map;

  public
    function GetBIntIntMap: TInt32ToInt32Map;
    function GetOrCreateBIntIntMap: TInt32ToInt32Map;

  public
    // map<int32, int32> b_int_int_map = 429;
    property BIntIntMap: TInt32ToInt32Map read FBIntIntMap write FBIntIntMap;
    property ConstBIntIntMap: TInt32ToInt32Map read GetBIntIntMap;
    property MutableBIntIntMap: TInt32ToInt32Map read GetOrCreateBIntIntMap;

  private
    FAenum: TAnEnum;

  public
    function GetAenum: TAnEnum;

  public
    // AnEnum aenum = 430;
    property Aenum: TAnEnum read FAenum write FAenum;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
  end;



implementation

function TMyOneOf.GetAnEnum: TAnEnum;
begin
  if Self.GetPointerByIndex(0) = nil then
  begin
    Exit(TAnEnum(0));

  end;

  Result := TAnEnum(PInt32(Self.GetPointerByIndex(0))^);

end;

procedure TMyOneOf.SetAnEnum(_AnEnum: TAnEnum);
var
  PData: PInt32;

begin
  if _AnEnum = TAnEnum(0) then
  begin
    Exit;
  end;

  PData := new(PInt32);
  PData^ := UInt64(_AnEnum);
  Self.SetPointerByIndex(0, PData);

end;

function TMyOneOf.GetDeptestAnEnum: TestDep1Unit.TAnEnum;
begin
  if Self.GetPointerByIndex(1) = nil then
  begin
    Exit(TestDep1Unit.TAnEnum(0));

  end;

  Result := TestDep1Unit.TAnEnum(PInt32(Self.GetPointerByIndex(1))^);

end;

procedure TMyOneOf.SetDeptestAnEnum(_DeptestAnEnum: TestDep1Unit.TAnEnum);
var
  PData: PInt32;

begin
  if _DeptestAnEnum = TestDep1Unit.TAnEnum(0) then
  begin
    Exit;
  end;

  PData := new(PInt32);
  PData^ := UInt64(_DeptestAnEnum);
  Self.SetPointerByIndex(1, PData);

end;

function TMyOneOf.GetIsCountry: AnsiString;
begin
  if Self.GetPointerByIndex(2) = nil then
  begin
    Exit('');

  end;

  Result := AnsiString(PAnsiString(Self.GetPointerByIndex(2))^);

end;

procedure TMyOneOf.SetIsCountry(_IsCountry: AnsiString);
var
  PData: PAnsiString;

begin
  if _IsCountry = '' then
  begin
    Exit;
  end;

  PData := new(PAnsiString);
  PData^ := _IsCountry;
  Self.SetPointerByIndex(2, PData);

end;

function TMyOneOf.GetIsState: AnsiString;
begin
  if Self.GetPointerByIndex(3) = nil then
  begin
    Exit('');

  end;

  Result := AnsiString(PAnsiString(Self.GetPointerByIndex(3))^);

end;

procedure TMyOneOf.SetIsState(_IsState: AnsiString);
var
  PData: PAnsiString;

begin
  if _IsState = '' then
  begin
    Exit;
  end;

  PData := new(PAnsiString);
  PData^ := _IsState;
  Self.SetPointerByIndex(3, PData);

end;

function TMyOneOf.GetIsCounty: AnsiString;
begin
  if Self.GetPointerByIndex(4) = nil then
  begin
    Exit('');

  end;

  Result := AnsiString(PAnsiString(Self.GetPointerByIndex(4))^);

end;

procedure TMyOneOf.SetIsCounty(_IsCounty: AnsiString);
var
  PData: PAnsiString;

begin
  if _IsCounty = '' then
  begin
    Exit;
  end;

  PData := new(PAnsiString);
  PData^ := _IsCounty;
  Self.SetPointerByIndex(4, PData);

end;

function TMyOneOf.GetIsCity: AnsiString;
begin
  if Self.GetPointerByIndex(5) = nil then
  begin
    Exit('');

  end;

  Result := AnsiString(PAnsiString(Self.GetPointerByIndex(5))^);

end;

procedure TMyOneOf.SetIsCity(_IsCity: AnsiString);
var
  PData: PAnsiString;

begin
  if _IsCity = '' then
  begin
    Exit;
  end;

  PData := new(PAnsiString);
  PData^ := _IsCity;
  Self.SetPointerByIndex(5, PData);

end;

function TMyOneOf.GetIsStreet: AnsiString;
begin
  if Self.GetPointerByIndex(6) = nil then
  begin
    Exit('');

  end;

  Result := AnsiString(PAnsiString(Self.GetPointerByIndex(6))^);

end;

procedure TMyOneOf.SetIsStreet(_IsStreet: AnsiString);
var
  PData: PAnsiString;

begin
  if _IsStreet = '' then
  begin
    Exit;
  end;

  PData := new(PAnsiString);
  PData^ := _IsStreet;
  Self.SetPointerByIndex(6, PData);

end;

function TMyOneOf.GetIsStr: AnsiString;
begin
  if Self.GetPointerByIndex(7) = nil then
  begin
    Exit('');

  end;

  Result := AnsiString(PAnsiString(Self.GetPointerByIndex(7))^);

end;

procedure TMyOneOf.SetIsStr(_IsStr: AnsiString);
var
  PData: PAnsiString;

begin
  if _IsStr = '' then
  begin
    Exit;
  end;

  PData := new(PAnsiString);
  PData^ := _IsStr;
  Self.SetPointerByIndex(7, PData);

end;

function TMyOneOf.GetIsI: Int32;
begin
  if Self.GetPointerByIndex(8) = nil then
  begin
    Exit(0);

  end;

  Result := Int32(PInt32(Self.GetPointerByIndex(8))^);

end;

procedure TMyOneOf.SetIsI(_IsI: Int32);
var
  PData: PInt32;

begin
  if _IsI = 0 then
  begin
    Exit;
  end;

  PData := new(PInt32);
  PData^ := _IsI;
  Self.SetPointerByIndex(8, PData);

end;

function TMyOneOf.GetIsF: Single;
begin
  if Self.GetPointerByIndex(9) = nil then
  begin
    Exit(0);

  end;

  Result := Single(PSingle(Self.GetPointerByIndex(9))^);

end;

procedure TMyOneOf.SetIsF(_IsF: Single);
var
  PData: PSingle;

begin
  if _IsF = 0 then
  begin
    Exit;
  end;

  PData := new(PSingle);
  PData^ := _IsF;
  Self.SetPointerByIndex(9, PData);

end;

function TMyOneOf.GetIsD: Double;
begin
  if Self.GetPointerByIndex(10) = nil then
  begin
    Exit(0);

  end;

  Result := Double(PDouble(Self.GetPointerByIndex(10))^);

end;

procedure TMyOneOf.SetIsD(_IsD: Double);
var
  PData: PDouble;

begin
  if _IsD = 0 then
  begin
    Exit;
  end;

  PData := new(PDouble);
  PData^ := _IsD;
  Self.SetPointerByIndex(10, PData);

end;

function TMyOneOf.GetIsA: TA;
begin
  if Self.GetPointerByIndex(11) = nil then
  begin
    Exit(nil);

  end;

  Result := TA(Self.GetPointerByIndex(11));

end;

procedure TMyOneOf.SetIsA(_IsA: TA);
begin
  if _IsA = nil then
  begin
    Exit;
  end;

  Self.SetPointerByIndex(11, _IsA);

end;

function TMyOneOf.GetIsB: Boolean;
begin
  if Self.GetPointerByIndex(12) = nil then
  begin
    Exit(False);

  end;

  Result := Boolean(PBoolean(Self.GetPointerByIndex(12))^);

end;

procedure TMyOneOf.SetIsB(_IsB: Boolean);
var
  PData: PBoolean;

begin
  if _IsB = False then
  begin
    Exit;
  end;

  PData := new(PBoolean);
  PData^ := _IsB;
  Self.SetPointerByIndex(12, PData);

end;


constructor TMyOneOf.Create;
begin
  inherited Create;

end;

destructor TMyOneOf.Destroy;
begin
  Clear;


  inherited Destroy;

end;

procedure TMyOneOf.Clear;
begin
  if Self = nil then
  begin
    Exit

  end;

  MaybeDispose(PUInt64(GetPointerByIndex(0)));
  MaybeDispose(PUInt64(GetPointerByIndex(1)));
  MaybeDispose(PAnsiString(GetPointerByIndex(2)));
  MaybeDispose(PAnsiString(GetPointerByIndex(3)));
  MaybeDispose(PAnsiString(GetPointerByIndex(4)));
  MaybeDispose(PAnsiString(GetPointerByIndex(5)));
  MaybeDispose(PAnsiString(GetPointerByIndex(6)));
  MaybeDispose(PAnsiString(GetPointerByIndex(7)));
  MaybeDispose(PInt32(GetPointerByIndex(8)));
  MaybeDispose(PSingle(GetPointerByIndex(9)));
  MaybeDispose(PDouble(GetPointerByIndex(10)));
  GetIsA.Free;
  MaybeDispose(PBoolean(GetPointerByIndex(12)));

  inherited;

end;

function TMyMessage.TA.GetAId: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FAId; 

end;


function TMyMessage.TA.GetASubMessage: TSubMessage;
begin
  if Self = nil then
    Exit(nil);

  Result := FASubMessage; 

end;

function TMyMessage.TA.GetOrCreateASubMessage: TSubMessage;
begin
  if Self = nil then
    Exit(nil);

  if Self.FASubMessage = nil then
    FASubMessage := TSubMessage.Create;

  Result := FASubMessage; 

end;


constructor TMyMessage.TA.Create;
begin
  inherited Create;


end;


destructor TA.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TA.Clear;
begin
  FreeAndNil(FASubMessage);

  inherited;
end;

procedure TA.SaveToStream(Stream: TProtoStreamWriter);
begin
    SaveInt32(Stream, AId, 1);

    SaveMessage(Stream, ASubMessage, 2);

end;


function TA.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
      1:
        AId := LoadInt32(Stream);

      2:
      begin
        if WireType <> 2 then
          Exit(False);
        if not LoadMessage(Stream, MutableASubMessage) then
          Exit(False);
      end;

    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;


function TMyMessage.TSubMessage.GetId: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FId; 

end;


constructor TMyMessage.TSubMessage.Create;
begin
  inherited Create;


end;


destructor TSubMessage.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TSubMessage.Clear;
begin

  inherited;
end;

procedure TSubMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
    SaveInt32(Stream, Id, 1);

end;


function TSubMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
      1:
        Id := LoadInt32(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;



function TInt32ToAMap.LoadFromStream(Stream: TProtoStreamReader): Boolean;
var
  StartPos, Len, f, w, fs: Integer;
  Key: Int32;
  Value: TA;

begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;
  fs := 0;

  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(f, w);

    if f = 1 then
    begin
      Key := Loadint32(Stream)
    end
    else if f = 2 then
    begin
      Value := TA.Create;

      if not LoadMessage(Stream, Value) then
      begin
        Exit(False);
      end

    end
    else
      Exit(False);

    fs := fs xor f;
    if fs = 3 then
    begin
      Self.Add(Key, Value);
      fs := 0;

    end;

  end;

  Result := StartPos + Len = Stream.Position;
end;

procedure TInt32ToAMap.SaveToStream(Stream: TProtoStreamWriter);
var
  it: TInt32ToAMap.TPairEnumerator;
  SizeNode: TLinkListNode;

begin
  if (Self = nil) or (Self.Count = 0) then
    Exit;

  it := Self.GetEnumerator;
  while it.MoveNext do
  begin
    Stream.WriteTag(1101, WIRETYPE_LENGTH_DELIMITED);
    SizeNode := Stream.AddIntervalNode;
    SaveInt32(Stream, it.Current.Key, 1);
    SaveMessage(Stream, it.Current.Value, 2);
    SizeNode.WriteLength(SizeNode.TotalSize);

  end;
  it.Free;

end;



function TInt32ToSubMessageMap.LoadFromStream(Stream: TProtoStreamReader): Boolean;
var
  StartPos, Len, f, w, fs: Integer;
  Key: Int32;
  Value: TSubMessage;

begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;
  fs := 0;

  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(f, w);

    if f = 1 then
    begin
      Key := Loadint32(Stream)
    end
    else if f = 2 then
    begin
      Value := TSubMessage.Create;

      if not LoadMessage(Stream, Value) then
      begin
        Exit(False);
      end

    end
    else
      Exit(False);

    fs := fs xor f;
    if fs = 3 then
    begin
      Self.Add(Key, Value);
      fs := 0;

    end;

  end;

  Result := StartPos + Len = Stream.Position;
end;

procedure TInt32ToSubMessageMap.SaveToStream(Stream: TProtoStreamWriter);
var
  it: TInt32ToSubMessageMap.TPairEnumerator;
  SizeNode: TLinkListNode;

begin
  if (Self = nil) or (Self.Count = 0) then
    Exit;

  it := Self.GetEnumerator;
  while it.MoveNext do
  begin
    Stream.WriteTag(420, WIRETYPE_LENGTH_DELIMITED);
    SizeNode := Stream.AddIntervalNode;
    SaveInt32(Stream, it.Current.Key, 1);
    SaveMessage(Stream, it.Current.Value, 2);
    SizeNode.WriteLength(SizeNode.TotalSize);

  end;
  it.Free;

end;



function TInt32ToInt32Map.LoadFromStream(Stream: TProtoStreamReader): Boolean;
var
  StartPos, Len, f, w, fs: Integer;
  Key: Int32;
  Value: Int32;

begin
  Len := Stream.ReadVarUInt32;
  StartPos := Stream.Position;
  fs := 0;

  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(f, w);

    if f = 1 then
    begin
      Key := Loadint32(Stream)
    end
    else if f = 2 then
    begin
      Value := Loadint32(Stream)
    end
    else
      Exit(False);

    fs := fs xor f;
    if fs = 3 then
    begin
      Self.Add(Key, Value);
      fs := 0;

    end;

  end;

  Result := StartPos + Len = Stream.Position;
end;

procedure TInt32ToInt32Map.SaveToStream(Stream: TProtoStreamWriter);
var
  it: TInt32ToInt32Map.TPairEnumerator;
  SizeNode: TLinkListNode;

begin
  if (Self = nil) or (Self.Count = 0) then
    Exit;

  it := Self.GetEnumerator;
  while it.MoveNext do
  begin
    Stream.WriteTag(421, WIRETYPE_LENGTH_DELIMITED);
    SizeNode := Stream.AddIntervalNode;
    SaveInt32(Stream, it.Current.Key, 1);
    SaveInt32(Stream, it.Current.Value, 2);
    SizeNode.WriteLength(SizeNode.TotalSize);

  end;
  it.Free;

end;


function TMyMessage.GetA: TA;
begin
  if Self = nil then
    Exit(nil);

  Result := FA; 

end;

function TMyMessage.GetOrCreateA: TA;
begin
  if Self = nil then
    Exit(nil);

  if Self.FA = nil then
    FA := TA.Create;

  Result := FA; 

end;


function TMyMessage.GetIdAMap: TInt32ToAMap;
begin
  if Self = nil then
    Exit(nil);

  Result := FIdAMap; 

end;

function TMyMessage.GetOrCreateIdAMap: TInt32ToAMap;
begin
  if Self = nil then
    Exit(nil);

  if Self.FIdAMap = nil then
    FIdAMap := TInt32ToAMap.Create;

  Result := FIdAMap; 

end;


function TMyMessage.GetMyOneOf: TMyOneOf;
begin
  if Self = nil then
    Exit(nil);

  Result := FMyOneOf; 

end;

function TMyMessage.GetOrCreateMyOneOf: TMyOneOf;
begin
  if Self = nil then
    Exit(nil);

  if Self.FMyOneOf = nil then
    FMyOneOf := TMyOneOf.Create;

  Result := FMyOneOf; 

end;



function TMyMessage.GetRepStr: TRepStr;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepStr; 

end;

function TMyMessage.GetOrCreateRepStr: TRepStr;

begin
  if Self = nil then
    Exit(nil);

  if FRepStr = nil then
    FRepStr := TRepStr.Create;
  Result := FRepStr;

end;


function TMyMessage.GetRepI: TRepI;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepI; 

end;

function TMyMessage.GetOrCreateRepI: TRepI;

begin
  if Self = nil then
    Exit(nil);

  if FRepI = nil then
    FRepI := TRepI.Create;
  Result := FRepI;

end;


function TMyMessage.GetRefF: TRefF;
begin
  if Self = nil then
    Exit(nil);

  Result := FRefF; 

end;

function TMyMessage.GetOrCreateRefF: TRefF;

begin
  if Self = nil then
    Exit(nil);

  if FRefF = nil then
    FRefF := TRefF.Create;
  Result := FRefF;

end;


function TMyMessage.GetRepD: TRepD;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepD; 

end;

function TMyMessage.GetOrCreateRepD: TRepD;

begin
  if Self = nil then
    Exit(nil);

  if FRepD = nil then
    FRepD := TRepD.Create;
  Result := FRepD;

end;


function TMyMessage.GetRepA: TRepA;
begin
  if Self = nil then
    Exit(nil);

  Result := FRepA; 

end;

function TMyMessage.GetOrCreateRepA: TRepA;
begin
  if Self = nil then
    Exit(nil);

  if Self.FRepA = nil then
    FRepA := TRepA.Create;

  Result := FRepA; 

end;


function TMyMessage.GetDd: Double;
begin
  if Self = nil then
    Exit(0);

  Result := FDd; 

end;


function TMyMessage.GetF: Single;
begin
  if Self = nil then
    Exit(0);

  Result := FF; 

end;


function TMyMessage.GetI32: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FI32; 

end;


function TMyMessage.GetI64: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FI64; 

end;


function TMyMessage.GetUi32: UInt32;
begin
  if Self = nil then
    Exit(0);

  Result := FUi32; 

end;


function TMyMessage.GetUi64: UInt64;
begin
  if Self = nil then
    Exit(0);

  Result := FUi64; 

end;


function TMyMessage.GetSi32: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FSi32; 

end;


function TMyMessage.GetSi64: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FSi64; 

end;


function TMyMessage.GetF32: UInt32;
begin
  if Self = nil then
    Exit(0);

  Result := FF32; 

end;


function TMyMessage.GetF64: UInt64;
begin
  if Self = nil then
    Exit(0);

  Result := FF64; 

end;


function TMyMessage.GetS32: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FS32; 

end;


function TMyMessage.GetS64: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FS64; 

end;


function TMyMessage.GetB: Boolean;
begin
  if Self = nil then
    Exit(False);

  Result := FB; 

end;


function TMyMessage.GetAnS: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FAnS; 

end;



function TMyMessage.GetRdd: TRdd;
begin
  if Self = nil then
    Exit(nil);

  Result := FRdd; 

end;

function TMyMessage.GetOrCreateRdd: TRdd;

begin
  if Self = nil then
    Exit(nil);

  if FRdd = nil then
    FRdd := TRdd.Create;
  Result := FRdd;

end;


function TMyMessage.GetRf: TRf;
begin
  if Self = nil then
    Exit(nil);

  Result := FRf; 

end;

function TMyMessage.GetOrCreateRf: TRf;

begin
  if Self = nil then
    Exit(nil);

  if FRf = nil then
    FRf := TRf.Create;
  Result := FRf;

end;


function TMyMessage.GetRi32: TRi32;
begin
  if Self = nil then
    Exit(nil);

  Result := FRi32; 

end;

function TMyMessage.GetOrCreateRi32: TRi32;

begin
  if Self = nil then
    Exit(nil);

  if FRi32 = nil then
    FRi32 := TRi32.Create;
  Result := FRi32;

end;


function TMyMessage.GetRi64: TRi64;
begin
  if Self = nil then
    Exit(nil);

  Result := FRi64; 

end;

function TMyMessage.GetOrCreateRi64: TRi64;

begin
  if Self = nil then
    Exit(nil);

  if FRi64 = nil then
    FRi64 := TRi64.Create;
  Result := FRi64;

end;


function TMyMessage.GetRui32: TRui32;
begin
  if Self = nil then
    Exit(nil);

  Result := FRui32; 

end;

function TMyMessage.GetOrCreateRui32: TRui32;

begin
  if Self = nil then
    Exit(nil);

  if FRui32 = nil then
    FRui32 := TRui32.Create;
  Result := FRui32;

end;


function TMyMessage.GetRui64: TRui64;
begin
  if Self = nil then
    Exit(nil);

  Result := FRui64; 

end;

function TMyMessage.GetOrCreateRui64: TRui64;

begin
  if Self = nil then
    Exit(nil);

  if FRui64 = nil then
    FRui64 := TRui64.Create;
  Result := FRui64;

end;


function TMyMessage.GetRsi32: TRsi32;
begin
  if Self = nil then
    Exit(nil);

  Result := FRsi32; 

end;

function TMyMessage.GetOrCreateRsi32: TRsi32;

begin
  if Self = nil then
    Exit(nil);

  if FRsi32 = nil then
    FRsi32 := TRsi32.Create;
  Result := FRsi32;

end;


function TMyMessage.GetRsi64: TRsi64;
begin
  if Self = nil then
    Exit(nil);

  Result := FRsi64; 

end;

function TMyMessage.GetOrCreateRsi64: TRsi64;

begin
  if Self = nil then
    Exit(nil);

  if FRsi64 = nil then
    FRsi64 := TRsi64.Create;
  Result := FRsi64;

end;


function TMyMessage.GetRf32: TRf32;
begin
  if Self = nil then
    Exit(nil);

  Result := FRf32; 

end;

function TMyMessage.GetOrCreateRf32: TRf32;

begin
  if Self = nil then
    Exit(nil);

  if FRf32 = nil then
    FRf32 := TRf32.Create;
  Result := FRf32;

end;


function TMyMessage.GetRf64: TRf64;
begin
  if Self = nil then
    Exit(nil);

  Result := FRf64; 

end;

function TMyMessage.GetOrCreateRf64: TRf64;

begin
  if Self = nil then
    Exit(nil);

  if FRf64 = nil then
    FRf64 := TRf64.Create;
  Result := FRf64;

end;


function TMyMessage.GetRs32: TRs32;
begin
  if Self = nil then
    Exit(nil);

  Result := FRs32; 

end;

function TMyMessage.GetOrCreateRs32: TRs32;

begin
  if Self = nil then
    Exit(nil);

  if FRs32 = nil then
    FRs32 := TRs32.Create;
  Result := FRs32;

end;


function TMyMessage.GetRs64: TRs64;
begin
  if Self = nil then
    Exit(nil);

  Result := FRs64; 

end;

function TMyMessage.GetOrCreateRs64: TRs64;

begin
  if Self = nil then
    Exit(nil);

  if FRs64 = nil then
    FRs64 := TRs64.Create;
  Result := FRs64;

end;


function TMyMessage.GetRb: TRb;
begin
  if Self = nil then
    Exit(nil);

  Result := FRb; 

end;

function TMyMessage.GetOrCreateRb: TRb;

begin
  if Self = nil then
    Exit(nil);

  if FRb = nil then
    FRb := TRb.Create;
  Result := FRb;

end;


function TMyMessage.GetRs: TRs;
begin
  if Self = nil then
    Exit(nil);

  Result := FRs; 

end;

function TMyMessage.GetOrCreateRs: TRs;

begin
  if Self = nil then
    Exit(nil);

  if FRs = nil then
    FRs := TRs.Create;
  Result := FRs;

end;


function TMyMessage.GetAenums: TAenums;
begin
  if Self = nil then
    Exit(nil);

  Result := FAenums; 

end;

function TMyMessage.GetOrCreateAenums: TAenums;

begin
  if Self = nil then
    Exit(nil);

  if FAenums = nil then
    FAenums := TAenums.Create;
  Result := FAenums;

end;

function TMyMessage.GetIdSubMessageMap: TInt32ToSubMessageMap;
begin
  if Self = nil then
    Exit(nil);

  Result := FIdSubMessageMap; 

end;

function TMyMessage.GetOrCreateIdSubMessageMap: TInt32ToSubMessageMap;
begin
  if Self = nil then
    Exit(nil);

  if Self.FIdSubMessageMap = nil then
    FIdSubMessageMap := TInt32ToSubMessageMap.Create;

  Result := FIdSubMessageMap; 

end;


function TMyMessage.GetAnIntIntMap: TInt32ToInt32Map;
begin
  if Self = nil then
    Exit(nil);

  Result := FAnIntIntMap; 

end;

function TMyMessage.GetOrCreateAnIntIntMap: TInt32ToInt32Map;
begin
  if Self = nil then
    Exit(nil);

  if Self.FAnIntIntMap = nil then
    FAnIntIntMap := TInt32ToInt32Map.Create;

  Result := FAnIntIntMap; 

end;


function TMyMessage.GetBIntIntMap: TInt32ToInt32Map;
begin
  if Self = nil then
    Exit(nil);

  Result := FBIntIntMap; 

end;

function TMyMessage.GetOrCreateBIntIntMap: TInt32ToInt32Map;
begin
  if Self = nil then
    Exit(nil);

  if Self.FBIntIntMap = nil then
    FBIntIntMap := TInt32ToInt32Map.Create;

  Result := FBIntIntMap; 

end;


function TMyMessage.GetAenum: TAnEnum;
begin
  if Self = nil then
    Exit(TAnEnum(0));

  Result := FAenum; 

end;


constructor TMyMessage.Create;
begin
  inherited Create;


end;


destructor TMyMessage.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TMyMessage.Clear;
begin
  FreeAndNil(FA);
  FreeAndNil(FIdAMap);
  FreeAndNil(FMyOneOf);
  FreeAndNil(FRepStr);
  FreeAndNil(FRepI);
  FreeAndNil(FRefF);
  FreeAndNil(FRepD);
  FreeAndNil(FRepA);
  FreeAndNil(FRdd);
  FreeAndNil(FRf);
  FreeAndNil(FRi32);
  FreeAndNil(FRi64);
  FreeAndNil(FRui32);
  FreeAndNil(FRui64);
  FreeAndNil(FRsi32);
  FreeAndNil(FRsi64);
  FreeAndNil(FRf32);
  FreeAndNil(FRf64);
  FreeAndNil(FRs32);
  FreeAndNil(FRs64);
  FreeAndNil(FRb);
  FreeAndNil(FRs);
  FreeAndNil(FAenums);
  FreeAndNil(FIdSubMessageMap);
  FreeAndNil(FAnIntIntMap);
  FreeAndNil(FBIntIntMap);

  inherited;
end;

procedure TMyMessage.SaveToStream(Stream: TProtoStreamWriter);
var
  _Aenums: Int32;


begin
  SaveMessage(Stream, A, 1100);

  if IdAMap <> nil then
    IdAMap.SaveToStream(Stream);

  if MyOneOf <> nil then
  begin
    if MyOneOf.GetPointerByIndex(0) <> nil then
      SaveInt32(Stream, Ord(MyOneOf.AnEnum), 12);
    if MyOneOf.GetPointerByIndex(1) <> nil then
      SaveInt32(Stream, Ord(MyOneOf.DeptestAnEnum), 13);
    if MyOneOf.GetPointerByIndex(2) <> nil then
      SaveString(Stream, MyOneOf.IsCountry, 1);
    if MyOneOf.GetPointerByIndex(3) <> nil then
      SaveString(Stream, MyOneOf.IsState, 2);
    if MyOneOf.GetPointerByIndex(4) <> nil then
      SaveString(Stream, MyOneOf.IsCounty, 3);
    if MyOneOf.GetPointerByIndex(5) <> nil then
      SaveString(Stream, MyOneOf.IsCity, 4);
    if MyOneOf.GetPointerByIndex(6) <> nil then
      SaveString(Stream, MyOneOf.IsStreet, 5);
    if MyOneOf.GetPointerByIndex(7) <> nil then
      SaveString(Stream, MyOneOf.IsStr, 6);
    if MyOneOf.GetPointerByIndex(8) <> nil then
      SaveInt32(Stream, MyOneOf.IsI, 8);
    if MyOneOf.GetPointerByIndex(9) <> nil then
      SaveFloat(Stream, MyOneOf.IsF, 9);
    if MyOneOf.GetPointerByIndex(10) <> nil then
      SaveDouble(Stream, MyOneOf.IsD, 10);
    if MyOneOf.GetPointerByIndex(11) <> nil then
      SaveMessage(Stream, MyOneOf.IsA, 11);
    if MyOneOf.GetPointerByIndex(12) <> nil then
      SaveBool(Stream, MyOneOf.IsB, 14);

  end;

  SaveRepeatedString(Stream, FRepStr, 122);

  SaveRepeatedInt32(Stream, FRepI, 123);

  SaveRepeatedFloat(Stream, FRefF, 124);

  SaveRepeatedDouble(Stream, FRepD, 125);

  specialize SaveRepeatedMessage<TA>(Stream, FRepA, 126);

  SaveDouble(Stream, Dd, 200);

  SaveFloat(Stream, F, 201);

  SaveInt32(Stream, I32, 202);

  SaveInt64(Stream, I64, 203);

  SaveUint32(Stream, Ui32, 204);

  SaveUint64(Stream, Ui64, 205);

  SaveSint32(Stream, Si32, 206);

  SaveSint64(Stream, Si64, 207);

  SaveFixed32(Stream, F32, 208);

  SaveFixed64(Stream, F64, 209);

  SaveSfixed32(Stream, S32, 210);

  SaveSfixed64(Stream, S64, 211);

  SaveBool(Stream, B, 212);

  SaveString(Stream, AnS, 213);

  SaveRepeatedDouble(Stream, FRdd, 300);

  SaveRepeatedFloat(Stream, FRf, 301);

  SaveRepeatedInt32(Stream, FRi32, 302);

  SaveRepeatedInt64(Stream, FRi64, 303);

  SaveRepeatedUint32(Stream, FRui32, 304);

  SaveRepeatedUint64(Stream, FRui64, 305);

  SaveRepeatedSint32(Stream, FRsi32, 306);

  SaveRepeatedSint64(Stream, FRsi64, 307);

  SaveRepeatedFixed32(Stream, FRf32, 308);

  SaveRepeatedFixed64(Stream, FRf64, 309);

  SaveRepeatedSfixed32(Stream, FRs32, 310);

  SaveRepeatedSfixed64(Stream, FRs64, 311);

  SaveRepeatedBool(Stream, FRb, 312);

  SaveRepeatedString(Stream, FRs, 313);

  if  FAenums <> nil then
  begin
    for _Aenums in FAenums do
      SaveInt32(Stream, Int32(_Aenums), 314);
  end;


  if IdSubMessageMap <> nil then
    IdSubMessageMap.SaveToStream(Stream);

  if AnIntIntMap <> nil then
    AnIntIntMap.SaveToStream(Stream);

  if BIntIntMap <> nil then
    BIntIntMap.SaveToStream(Stream);

  SaveInt32(Stream, Ord(Aenum), 430);

end;


function TMyMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1100:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableA) then
        Exit(False);
    end;
    1101:
    begin
      if WireType <> 2 then
        Exit(False);
      if not MutableIdAMap.LoadFromStream(Stream) then
        Exit(False);
    end;

    12: MutableMyOneOf.AnEnum := TAnEnum(LoadInt32(Stream));

    13: MutableMyOneOf.DeptestAnEnum := TestDep1Unit.TAnEnum(LoadInt32(Stream));
    1:
      MutableMyOneOf.IsCountry := LoadString(Stream);

    2:
      MutableMyOneOf.IsState := LoadString(Stream);

    3:
      MutableMyOneOf.IsCounty := LoadString(Stream);

    4:
      MutableMyOneOf.IsCity := LoadString(Stream);

    5:
      MutableMyOneOf.IsStreet := LoadString(Stream);

    6:
      MutableMyOneOf.IsStr := LoadString(Stream);

    8:
      MutableMyOneOf.IsI := LoadInt32(Stream);

    9:
      MutableMyOneOf.IsF := LoadFloat(Stream);

    10:
      MutableMyOneOf.IsD := LoadDouble(Stream);

    11:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableMyOneOf.IsA) then
        Exit(False);
    end;
    14:
      MutableMyOneOf.IsB := LoadBool(Stream);

    122: 
      if not LoadRepeatedString(Stream, MutableRepStr) then
        Exit(False);

    123: 
      if not LoadRepeatedInt32(Stream, MutableRepI) then
        Exit(False);

    124: 
      if not LoadRepeatedFloat(Stream, MutableRefF) then
        Exit(False);

    125: 
      if not LoadRepeatedDouble(Stream, MutableRepD) then
        Exit(False);

    126: 
      if not (specialize LoadRepeatedMessage<TA>(Stream, MutableRepA)) then
        Exit(False);

    200:
      Dd := LoadDouble(Stream);

    201:
      F := LoadFloat(Stream);

    202:
      I32 := LoadInt32(Stream);

    203:
      I64 := LoadInt64(Stream);

    204:
      Ui32 := LoadUint32(Stream);

    205:
      Ui64 := LoadUint64(Stream);

    206:
      Si32 := LoadSint32(Stream);

    207:
      Si64 := LoadSint64(Stream);

    208:
      F32 := LoadFixed32(Stream);

    209:
      F64 := LoadFixed64(Stream);

    210:
      S32 := LoadSfixed32(Stream);

    211:
      S64 := LoadSfixed64(Stream);

    212:
      B := LoadBool(Stream);

    213:
      AnS := LoadString(Stream);

    300: 
      if not LoadRepeatedDouble(Stream, MutableRdd) then
        Exit(False);

    301: 
      if not LoadRepeatedFloat(Stream, MutableRf) then
        Exit(False);

    302: 
      if not LoadRepeatedInt32(Stream, MutableRi32) then
        Exit(False);

    303: 
      if not LoadRepeatedInt64(Stream, MutableRi64) then
        Exit(False);

    304: 
      if not LoadRepeatedUint32(Stream, MutableRui32) then
        Exit(False);

    305: 
      if not LoadRepeatedUint64(Stream, MutableRui64) then
        Exit(False);

    306: 
      if not LoadRepeatedSint32(Stream, MutableRsi32) then
        Exit(False);

    307: 
      if not LoadRepeatedSint64(Stream, MutableRsi64) then
        Exit(False);

    308: 
      if not LoadRepeatedFixed32(Stream, MutableRf32) then
        Exit(False);

    309: 
      if not LoadRepeatedFixed64(Stream, MutableRf64) then
        Exit(False);

    310: 
      if not LoadRepeatedSfixed32(Stream, MutableRs32) then
        Exit(False);

    311: 
      if not LoadRepeatedSfixed64(Stream, MutableRs64) then
        Exit(False);

    312: 
      if not LoadRepeatedBool(Stream, MutableRb) then
        Exit(False);

    313: 
      if not LoadRepeatedString(Stream, MutableRs) then
        Exit(False);

    314: 
        if not LoadRepeatedInt32(Stream, MutableAenums) then          Exit(False);

    420:
    begin
      if WireType <> 2 then
        Exit(False);
      if not MutableIdSubMessageMap.LoadFromStream(Stream) then
        Exit(False);
    end;
    421:
    begin
      if WireType <> 2 then
        Exit(False);
      if not MutableAnIntIntMap.LoadFromStream(Stream) then
        Exit(False);
    end;
    429:
    begin
      if WireType <> 2 then
        Exit(False);
      if not MutableBIntIntMap.LoadFromStream(Stream) then
        Exit(False);
    end;

    430: Aenum := TAnEnum(LoadInt32(Stream));

    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;




end.
