unit PBTypeUnit;

{$mode objfpc}{$H+}

interface

uses
  PBOptionUnit, ObjectListUnit, Classes, SysUtils;

type

  { TBaseType }

  TBaseType = class(TObject)
  private
    FProtoType: AnsiString;

  protected
    function GetFPCType: AnsiString; virtual; abstract;

  public
    property ProtoType: AnsiString read FProtoType;
    property FPCType: AnsiString read GetFPCType;

    constructor Create(PType: AnsiString);

  end;

    { TEnumField }

    TEnumField = class(TObject)
    private
      FName: AnsiString;
      FOptions: TOptions;
      FValue: Integer;
      FEnumName: AnsiString;
      function GetFPCValue: AnsiString;

    public
      property EnumName: AnsiString read FEnumName;
      property Name: AnsiString read FName;
      property Value: Integer read FValue;
      property Options: specialize TObjectList<TOption> read FOptions;
      property FPCValue: AnsiString read GetFPCValue;

      constructor Create(_EnumName, _Name: AnsiString; _Options: TOptions; _Value: Integer);
      destructor Destroy; override;
      function ToXML: AnsiString;

    end;

    TEnumFields = specialize TObjectList<TEnumField>;

    { TEnum }

    TEnum = class(TEnumFields)
    private
      FName: AnsiString;
      FOptions: TOptions;
    public
      property Name: AnsiString read FName;
      property Options: TOptions read FOptions;

      constructor Create(_Name: AnsiString; _Options: TOptions;
          _EnumFields: TEnumFields);
      destructor Destroy; override;

      function ToXML: AnsiString;
    end;

implementation

uses
  UtilsUnit;

{ TEnum }

constructor TEnum.Create(_Name: AnsiString; _Options: TOptions;
  _EnumFields: TEnumFields);
var
  EF: TEnumField;

begin
  inherited Create;

  FName := _Name;
  FOptions := _Options;

  for EF in _EnumFields do
    Self.Add(EF);

end;

destructor TEnum.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TEnum.ToXML: AnsiString;
begin
  Result := Format('<TEnum Name= "%s">%s',
    [FName, FOptions.ToXML]);
  Result += inherited ToXML;
  Result += Format('</TEnum>', []);
end;

{ TEnumField }

function TEnumField.GetFPCValue: AnsiString;
begin
  Result := Format('%s_%s', [Canonicalize(EnumName), Canonicalize(Self.Name)]);
end;

constructor TEnumField.Create(_EnumName, _Name: AnsiString; _Options: TOptions;
  _Value: Integer);
begin
  inherited Create;

  FEnumName := _EnumName;
  FName := _Name;
  FOptions := _Options;
  FValue := _Value;

end;

destructor TEnumField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TEnumField.ToXML: AnsiString;
begin
  Result := Format('<EnumField Name= "%s" Value="%d">%s</EnumField>', [Name, Value, Options.ToXML]);

end;

{ TBaseType }

constructor TBaseType.Create(PType: AnsiString);
begin
  inherited Create;

  FProtoType := PType;

end;


end.

