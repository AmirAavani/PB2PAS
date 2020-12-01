unit ObjectListUnit;

{$mode objfpc}{$H+}

interface

uses
  fgl, Classes, SysUtils;

type
  { TObjectList }

  generic TObjectList<TObject> = class(specialize TFPGList<TObject>)
  private
    function GetByName(aName: AnsiString): TObject;
  public
    property ByName[aName: AnsiString]: TObject read GetByName;
    destructor Destroy; override;

    function ToXML: AnsiString;
  end;



implementation

{ TObjectList }

function TObjectList.GetByName(aName: AnsiString): TObject;
var
  anObj: TObject;

begin
  for anObj in Self do
    if anObj.Name = aName then
      Exit(anObj);

  Exit(nil);
end;

destructor TObjectList.Destroy;
var
  Obj: TObject;

begin
  for Obj in Self do
    Obj.Free;

  inherited Destroy;

end;

function TObjectList.ToXML: AnsiString;
var
  Obj: TObject;
  NodeName: AnsiString;

begin
  if Self.Count = 0 then
    Exit;

  // ClassName is in form of TObjectList<PBDefinitionUnit.ClassName>
  NodeName := Copy(ClassName, Pos('.', ClassName) + 1, Length(ClassName) - Pos('.', ClassName) - 1) + 's';

  Result := Format('<%s>'#10, [NodeName]);

  for Obj in Self do
    Result += Obj.ToXML;

  Result += Format('</%s>'#10, [NodeName]);
end;

end.

