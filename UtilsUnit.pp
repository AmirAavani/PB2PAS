unit UtilsUnit;

{$mode objfpc}{$H+}

interface

uses
  PBDefinitionUnit, Classes, SysUtils;

function Canonicalize(AName: AnsiString): AnsiString;
function FormatString(const FieldType: AnsiString): AnsiString;
function GetUnitName(const Filename: AnsiString): AnsiString;
function GetNonRepeatedType4FPC(TypeName: AnsiString): AnsiString;
function GetUnitName(MessageField: TMessageField; FieldProto: TProto; RelatedProtos: TProtos): AnsiString;
function IsOneOfType(MessageField: TMessageField; FieldProto: TProto; RelatedProtos: TProtos): Boolean;
function MaybeRemoveQuotations(aValue: AnsiString): AnsiString;

type
  ENotImplementedYet = class(Exception);

implementation

uses
  ALoggerUnit;

function Canonicalize(AName: AnsiString): AnsiString;
var
  c: Char;
  LastCharWasUnderline: Boolean;

begin
  LastCharWasUnderline:= True;
  Result := '';
  for c in AName do
    if c = '_' then
      LastCharWasUnderline:= True
    else
    begin
      if LastCharWasUnderline then
        Result += upcase(c)
      else
        Result += c;
      LastCharWasUnderline := False;
    end;
end;

function GetUnitName(const Filename: AnsiString): AnsiString;
var
  PureFilename: Ansistring;

begin
  PureFilename := Copy(ExtractFileName(Filename), 1,
        Length(ExtractFileName(Filename)) - Length(ExtractFileExt(Filename)));
  Result := Canonicalize(PureFilename) + 'Unit';

end;

function GetNonRepeatedType4FPC(TypeName: AnsiString): AnsiString;
begin
  case TypeName of
    'double': Result := 'Double';
    'float': Result := 'Single';
    'int16': Result := 'Int16';
    'int32': Result := 'Int32';
    'int64': Result := 'Int64';
    'sint16': Result := 'Int16';
    'sint32': Result := 'Int32';
    'sint64': Result := 'Int64';
    'fixed32': Result := 'UInt32';
    'fixed64': Result := 'UInt64';
    'sfixed32': Result := 'Int32';
    'sfixed64': Result := 'Int64';
    'uint16': Result := 'UInt16';
    'uint32': Result := 'UInt32';
    'uint64': Result := 'UInt64';
    'bool': Result := 'Boolean';
    'string': Result := 'AnsiString';
    'byte': Result := 'Byte';
    else
      Result := 'T' + Canonicalize(TypeName);
  end;
end;

function GetUnitName(MessageField: TMessageField; FieldProto: TProto;
  RelatedProtos: TProtos): AnsiString;
var
  PackageName: AnsiString;
  Proto: TProto;

begin
  PackageName := MessageField.FieldType.PackageName;

  if PackageName = '' then
    Exit('');

  for Proto in RelatedProtos do
    if Proto.PackageName = PackageName then
      Exit(Proto.OutputUnitName);

  Result := '';
end;

function IsOneOfType(MessageField: TMessageField; FieldProto: TProto;
  RelatedProtos: TProtos): Boolean;
begin
  if MessageField is TOneOf then
    Exit;
end;

function GetTypeSize(TypeName: AnsiString): Integer;
begin
  case TypeName of
    'Double': Result := SizeOf(Double);
    'Float': Result := SizeOf(Single);
    'Int16': Result := SizeOf(Int16);
    'Int32': Result := SizeOf(Int32);
    'Int64': Result := SizeOf(Int64);
    'UInt16': Result := SizeOf(UInt16);
    'UInt32': Result := SizeOf(UInt32);
    'UInt64': Result := SizeOf(UInt64);
    'Boolean': Result := SizeOf(Boolean);
    'Byte': Result := SizeOf(Byte)
  else
     raise Exception.Create(Format('GetTypeSize NIY for Typename %s', [TypeName]));
  end;
end;

function FormatString(const FieldType: AnsiString): AnsiString;
begin
  case FieldType of
  'double': Exit('e');
  'float': Exit('e');
  'int16': Exit('d');
  'sint16': Exit('d');
  'int32': Exit('d');
  'sint32': Exit('d');
  'fixed32': Exit('d');
  'int64': Exit('d');
  'sint64': Exit('d');
  'fixed64': Exit('d');
  'uint16': Exit('u');
  'uint32': Exit('u');
  'uint64': Exit('u');
  'bool': Exit('d');
  'string': Exit('s');
  'byte': Exit('d');
  'bytes': Exit('p');

  end;
end;

function MaybeRemoveQuotations(aValue: AnsiString): AnsiString;
begin
  if aValue = '' then
    Exit(aValue);
  if (aValue[1] = aValue[Length(aValue)]) and ((aValue[1] = '''') or (aValue[1] = '"')) then
    Result := Copy(aValue, 2, Length(aValue) - 2);
end;


end.

