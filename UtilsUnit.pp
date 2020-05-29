unit UtilsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function Canonicalize(AName: AnsiString): AnsiString;
function FormatString(const FieldType: AnsiString): AnsiString;
function GetUnitName(const Filename: AnsiString): AnsiString;
function GetTypeName(Prefix: AnsiString; TypeName: AnsiString): AnsiString;
function IsSimpleType(TypeName: AnsiString): Boolean;

type
  ENotImplementedYet = class(Exception);


implementation

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

function IsSimpleType(TypeName: AnsiString): Boolean;
begin
  Result := True;
  case TypeName of
    'double': Exit;
    'float': Exit;
    'int16': Exit;
    'sint16': Exit;
    'int32': Exit;
    'sint32': Exit;
    'fixed32': Exit;
    'int64': Exit;
    'sint64': Exit;
    'fixed64': Exit;
    'uint16': Exit;
    'uint32': Exit;
    'uint64': Exit;
    'bool': Exit;
    'string': Exit;
    'byte': Exit;
    'bytes': Exit;
    else
      Result := False;
  end;
end;

function GetTypeName(Prefix: AnsiString; TypeName: AnsiString): AnsiString;
begin
  case TypeName of
    'double': Result := 'Double';
    'float': Result := 'Single';
    'int16': Result := 'Int16';
    'sint16': Result := 'Int16';
    'int32': Result := 'Int32';
    'sint32': Result := 'Int32';
    'fixed32': Result := 'Int32';
    'int64': Result := 'Int64';
    'sint64': Result := 'Int64';
    'fixed64': Result := 'Int64';
    'uint16': Result := 'UInt16';
    'uint32': Result := 'UInt32';
    'uint64': Result := 'UInt64';
    'bool': Result := 'Boolean';
    'string': Result := 'AnsiString';
    'byte': Result := 'Byte';
    'bytes': Result := 'TBytes';
    else
    begin
      Result := 'T' + Canonicalize(TypeName);
      if Prefix <> '' then
        Result := Prefix + '.T' + Canonicalize(TypeName);
    end;
  end;

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


end.

