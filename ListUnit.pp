unit ListUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  { TObjectList }

  generic TObjectList<T> = class(specialize TList<T>)
  private
    function GetByName(const Name: AnsiString): T;
  public
    property ByName[const Name: AnsiString]: T read GetByName;
    function ToXML: AnsiString;

    destructor Destroy; override;

  end;

implementation

function TObjectList.GetByName(const Name: AnsiString): T;
var
  m: T;

begin
  Result := nil;

  for m in Self do
    if m.Name = Name then
      Exit(m);

end;

function TObjectList.ToXML: AnsiString;
var
  m: T;
  Strings: TStringList;

begin
  Strings := TStringList.Create;

  Strings.Add('<List>');
  for m in Self do
    Strings.Add(m.ToXML);
  Strings.Add('</List>');

  Result := Strings.Text;
  Strings.Free;

end;

destructor TObjectList.Destroy;
var
  m: T;

begin
  for m in Self do
    m.Free;

  inherited Destroy;
end;

end.

