unit ProtoHelperListsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  { TObjectList }

  generic TObjectList<TMyObject> = class(specialize TFPGList<TMyObject>)
  private
    function GetCount: Integer;
  public
    property Count: Integer read GetCount write SetCount;

    destructor Destroy; override;

    function ToString: AnsiString; override;
    function LoadFromStream(Stream: TStream): Boolean;
    procedure SaveToStream(Stream: TStream);
  end;

  { TSimpleTypeList }

  generic TSimpleTypeList<TSimpleObject> = class(specialize TFPGList<TSimpleObject>)
  private
    function GetCount: Integer;
  protected
    FormatString: AnsiString;

  public
    property Count: Integer read GetCount write SetCount;

    constructor Create(FmtString: AnsiString);
    destructor Destroy; override;

    function ToString: AnsiString; override;
    function LoadFromStream(Stream: TStream): Boolean;
    procedure SaveToStream(Stream: TStream);
  end;

  { TBooleanList }

  TBooleanList = class(specialize TSimpleTypeList<Boolean>)
  public
    function ToString: AnsiString; override;
  end;

implementation

{ TBooleanList }

function TBooleanList.ToString: AnsiString;
var
  Data: Boolean;

begin
  Result := '[';
  for data in Self do
  begin
    if Length(Result) <> 1 then
      Result += ', ';
    Result += BoolToStr(data)
  end;
  Result += ']';

end;

{ TSimpleTypeList }

function TSimpleTypeList.GetCount: Integer;
begin
  if Self = nil then
    Exit(0);
  Result := FCount;
end;

constructor TSimpleTypeList.Create(FmtString: AnsiString);
begin
  inherited Create;

  FormatString := FmtString;
end;

destructor TSimpleTypeList.Destroy;
begin
  inherited Destroy;
end;

function TSimpleTypeList.ToString: AnsiString;
var
  Data: TSimpleObject;

begin
  Result := '[';
  for data in Self do
  begin
    if Length(Result) <> 1 then
      Result += ', ';
    Result += Format(FormatString, [data]);
  end;
  Result += ']';

end;

function TSimpleTypeList.LoadFromStream(Stream: TStream): Boolean;
begin

end;

procedure TSimpleTypeList.SaveToStream(Stream: TStream);
begin

end;

{ TObjectList }

function TObjectList.GetCount: Integer;
begin
  if Self = nil then
    Exit(0);
  Result := FCount;
end;

destructor TObjectList.Destroy;
var
  Obj: TObject;

begin
  for Obj in Self do
    Obj.Free;

  inherited Destroy;

end;

function TObjectList.ToString: AnsiString;
var
  Obj: TObject;

begin
  Result := '';
  for Obj in Self do
    Result += Obj.ToString;
end;

function TObjectList.LoadFromStream(Stream: TStream): Boolean;
begin
  Halt(1);
end;

procedure TObjectList.SaveToStream(Stream: TStream);
begin
  Halt(2);
end;

end.

