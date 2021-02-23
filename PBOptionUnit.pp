unit PBOptionUnit;

{$mode objfpc}{$H+}

interface

uses
  NamedObjectListUnit, Classes, SysUtils;

type
  TOptionName = AnsiString;
  TConstValue = AnsiString;

  { TOption }

  TOption = class(TObject)
  private
    FOptionName: TOptionName;
    FConstValue: TConstValue;

  public
    constructor Create(Name: TOptionName; Value: TConstValue);

    property Name: AnsiString read FOptionName;
    property OptionName: TOptionName read FOptionName;
    property ConstValue: TConstValue read FConstValue;

    function ToXML: AnsiString;
  end;

  TOptions = specialize TNamedObjectList<TOption>;


implementation
uses
  UtilsUnit;

{ TOption }

constructor TOption.Create(Name: TOptionName; Value: TConstValue);
begin
  inherited Create;

  FOptionName := Name;
  FConstValue := Value;
end;

function TOption.ToXML: AnsiString;
begin
  Result := Format('<Option Name = "%s" Value = "%s"/>'#10, [OptionName, MaybeRemoveQuotations(ConstValue)]);

end;

end.

