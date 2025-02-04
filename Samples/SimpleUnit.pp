unit SimpleUnit;
{$Mode objfpc}

interface

uses
 classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TA = class;

  // message A
  { TA }
  TA = class(TBaseMessage)
  // Forward Declarations.
  public type
    TB = class;

  public type
    // message B
    { TB }
    TB = class(TBaseMessage)
    // Forward Declarations.

    private
      FC: Int32;

    public
      function GetC: Int32;

    public
      // int32 c = 1;
      property C: Int32 read FC write FC;

    protected 
      procedure SaveToStream(Stream: TProtoStreamWriter); override;
      function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear; override;

    public // functions
      function DeepCopy: TB;

    end;

  public type
    TBs = specialize TObjectList<TB>;

  private
    FBs: TBs;

  public
    function GetBs: TBs;
    function GetOrCreateBs: TBs;

  public
    // repeated B bs = 1;
    property Bs: TBs read FBs write FBs;
    property ConstBs: TBs read GetBs;
    property MutableBs: TBs read GetOrCreateBs write FBs;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TA;

  end;



implementation

function TA.TB.GetC: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FC; 

end;


constructor TA.TB.Create;
begin
  inherited Create;


end;


destructor TA.TB.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TA.TB.Clear;
begin

  inherited;
end;

procedure TA.TB.SaveToStream(Stream: TProtoStreamWriter);
begin
    SaveInt32(Stream, C, 1);

end;


function TA.TB.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
      1:
        C := LoadInt32(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TA.TB.DeepCopy: TA.TB;
begin
  if Self = nil then
    Exit(nil);

  Result := TA.TB.Create;

  Result.C := Self.C;

end;


function TA.GetBs: TBs;
begin
  if Self = nil then
    Exit(nil);

  Result := FBs; 

end;

function TA.GetOrCreateBs: TBs;
begin
  if Self = nil then
    Exit(nil);

  if Self.FBs = nil then
    FBs := TBs.Create;

  Result := FBs; 

end;


constructor TA.Create;
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
  FreeAndNil(FBs);

  inherited;
end;

procedure TA.SaveToStream(Stream: TProtoStreamWriter);
begin
  specialize SaveRepeatedMessage<TB>(Stream, FBs, 1);

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
      if not (specialize LoadRepeatedMessage<TB>(Stream, MutableBs)) then
        Exit(False);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TA.DeepCopy: TA;
begin
  if Self = nil then
    Exit(nil);

  Result := TA.Create;

  Result.FBs := Self.Bs.DeepCopy;


end;



end.
