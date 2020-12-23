unit PBDefinitionUnit;

{$mode objfpc}{$H+}

interface

uses
  StreamUnit, PBOptionUnit, ObjectListUnit, Classes, SysUtils, fgl;

type
  TIntList = specialize TFPGList<Integer>;
  TIdentifier = AnsiString;
  TFullIdentifier = AnsiString;
  TMessage = class;
  TImports = class;
  TProto = class;
  TProtos = specialize TFPGList<TProto>;

  TOneOf = class;
  TType = AnsiString;

  TParent = record
    OneOf: TOneOf;
    Message: TMessage;
    Proto: TProto;
  end;

type

  { TPBBaseType }

  TPBBaseType = class(TObject)
  private
    function GetFPCTypeName: AnsiString;

  protected
    FParent: TParent;
    FName: AnsiString;
    FOptions: TOptions;
    FIsRepeated: Boolean;

  public
    property Parent: TParent read FParent;
    property Name: AnsiString read FName;
    property Options: TOptions read FOptions;
    property FPCTypeName: AnsiString read GetFPCTypeName;
    property IsRepeated: Boolean read FIsRepeated;

    constructor Create(_Name: AnsiString; _IsRepeated: Boolean;
        _Options: TOptions; _Parent: TParent);
    function ToXML: AnsiString; virtual;

  end;

  { TMessageField }

  TMessageField = class(TObject)
  private
    function GetCanonicalizeName: AnsiString;

  protected
    FParent: TParent;
    FFieldNumber: Integer;
    FIsRepeated: Boolean;
    FFieldType: TType;
    // FFieldPBType: TPBBaseType;
    FName: AnsiString;
    FOptions: TOptions;

    function GetFieldNumber: Integer; virtual;
    function GetFieldType: TType; virtual;
    function GetIsRepeated: Boolean; virtual;
    function GetCanonicalizeFullName: AnsiString; virtual;
    function GetName: AnsiString; virtual;
    function GetOptions: TOptions;  virtual;
    // Returns the translation of FieldType to FPC.
    function GetFPCTypeName: AnsiString;  virtual;
    function GetPackageName: AnsiString; virtual;
  public
    property Parent: TParent read FParent;
    property IsRepeated: Boolean read GetIsRepeated;
    property FieldType: TType read GetFieldType;
    property FPCTypeName: AnsiString read GetFPCTypeName;
    property PackageName: AnsiString read GetPackageName;
    property Name: AnsiString read GetName;
    property CanonicalizeName: AnsiString read GetCanonicalizeName;
    property CanonicalizeFullName: AnsiString read GetCanonicalizeFullName;
    property FieldNumber: Integer read GetFieldNumber;
    property Options: TOptions read GetOptions;

    constructor Create(_Name: AnsiString; _FieldType: TType; _IsRepeated: Boolean;
      _FieldNumber: Integer; _Options: TOptions; _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;

    function HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; virtual;
  end;

  TMessageFields = specialize TObjectList<TMessageField>;

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

  TEnum = class(TPBBaseType)
  protected
    FAllFields: TEnumFields;

  public
    property AllFields: TEnumFields read FAllFields;

    constructor Create(_Name: AnsiString; _Options: TOptions;
        _EnumFields: TEnumFields; _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString; override;
  end;

  TEnums = specialize TObjectList<TEnum>;

  { TOneOfField }

  TOneOfField = class(TMessageField)
  protected
    function GetCanonicalizeFullName: AnsiString; override;

  public
    constructor Create(_Name: AnsiString; _OneOfFieldType: TType;
         _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
         _Parent: TOneOf);
    function ToXML: AnsiString; override;

  end;

  TOneOfFields = specialize TObjectList<TOneOfField>;
  { TOneOf }

  TOneOf = class(TMessageField)
  private
    OneOfFields: TOneOfFields;
    function GetFieldNumbers: AnsiString;

  public
    property Fields: TOneOfFields read OneOfFields;
    property FieldNumbers: AnsiString read GetFieldNumbers;
    constructor Create(_Name: AnsiString; _Fields: TOneOfFields;
          ParentMessage: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString;override;
    function HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; override;
  end;

  TOneOfs = specialize TObjectList<TOneOf>;

  { TMap }

  TMap = class(TMessageField)
  private
    FKeyType: TType;
    FValueType: TType;
    FKeyPBType: TPBBaseType;
    FValuePBType: TPBBaseType;

  protected
    function GetFieldType: TType; override;
    // Returns the translation of FieldType to FPC.
    function GetFPCTypeName: AnsiString; override;

  public
    property KeyType: TType read FKeyType;
    property ValueType: TType read FValueType;
    property KeyPBType: TPBBaseType read FKeyPBType;
    property ValuePBType: TPBBaseType read FValuePBType;

    constructor Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType, _ValueType: TType;
           _Options: TOptions; _Parent: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString; override;
    function HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; override;
  end;


  TMaps = specialize TObjectList<TMap>;

  { TMessage }

  TMessage = class(TObject)
  private
    FFields: TMessageFields;
    FMessages: specialize TObjectList<TMessage>;
    FOptions: TOptions;
    FEnums: TEnums;
    FName: AnsiString;
    FParent: TParent;

    // MessageClassName: AnsiString;

    function GetFPCTypeName: AnsiString;
    procedure PrepareForCodeGeneration(AllClassesNames, AllEnumsNames: TStringList);
  public
    // property Parent:
    property Name: AnsiString read FName;
    property Fields: TMessageFields read FFields;
    property Messages: specialize TObjectList<TMessage> read FMessages;
    property Options: TOptions read FOptions;
    property Enums: TEnums read FEnums;
    property FPCTypeName: AnsiString read GetFPCTypeName;
    property Parent: TParent read FParent;

    constructor Create(_Name: AnsiString;
      _Fields: TMessageFields;
      _Messages: specialize TObjectList<TMessage>;
      _Options:  TOptions;
      _Enums:  TEnums;
      _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString;
  end;

  TMessages = specialize TObjectList<TMessage>;

  { TImports }

  TImports = class(TStringList)
    function ToXML: AnsiString;
  end;

  { TPackages }

  TPackages = class(TStringList)
    function ToXML: AnsiString;
  end;

  { TProto }

  TProto = class(TObject)
  private
    FOtherParams: TStringList;
    FImports: TImports;
    FOptions: TOptions;
    FMessages: TMessages;
    FEnums: TEnums;
    function GetInputProtoFilename: AnsiString;
    function GetOutputUnitName: AnsiString;
    function GetPackageName: AnsiString;
    function GetSyntax: AnsiString;

    function GetValueFromOtherParams(ParamName: AnsiString): AnsiString;
  public
    property Imports: TImports read FImports;
    property Options: TOptions read FOptions;
    property Messages: TMessages read FMessages;
    property Enums: TEnums read FEnums;
    property Syntax: AnsiString read GetSyntax;
    property InputProtoFilename: AnsiString read GetInputProtoFilename;
    property PackageName: AnsiString read GetPackageName;
    property OutputUnitName: AnsiString read GetOutputUnitName;

    constructor Create(_Imports: TImports; _Options: TOptions;
  _Messages: TMessages; _Enums: TEnums; OtherParams: TStringList);
    destructor Destroy; override;

    function ToXML: AnsiString;

  end;

  { TProto3 }

  TProto3 = class(TProto)
  public

  end;

  TStrLit = AnsiString;
  TConstant = AnsiString;
  TProtoMap = specialize TFPGMap<AnsiString, TProto>;

function CreateParent(anOneOf: TOneOf; aMessage: TMessage; aProto: TProto): TParent;


implementation
uses
  UtilsUnit, StringUnit, strutils;

function CreateParent(anOneOf: TOneOf; aMessage: TMessage; aProto: TProto): TParent;
begin
  Result.OneOf := anOneOf;
  Result.Message := aMessage;
  Result.Proto := aProto;

end;

{ TPBBaseType }

function TPBBaseType.GetFPCTypeName: AnsiString;
begin
  if IsRepeated then
    Result := 'T' + Canonicalize(Name)
  else
    Result := GetNonRepeatedType4FPC(Name);

end;

constructor TPBBaseType.Create(_Name: AnsiString; _IsRepeated: Boolean;
  _Options: TOptions; _Parent: TParent);
begin
  inherited Create;

  FParent := _Parent;
  FOptions := _Options;
  FName := _Name;
  FIsRepeated := _IsRepeated;

end;

function TPBBaseType.ToXML: AnsiString;
begin
  Result := Format('<%s Name= "%s">%s',
    [Self.ClassName, FName, FOptions.ToXML]);

end;

{ TEnum }

constructor TEnum.Create(_Name: AnsiString; _Options: TOptions;
  _EnumFields: TEnumFields; _Parent: TParent);
begin
  inherited Create(_Name, False, _Options, _Parent);

  FParent := _Parent;
  FAllFields := _EnumFields;

end;

destructor TEnum.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TEnum.ToXML: AnsiString;
begin
  Result := Format('<TEnum Name= "%s">%s',
    [Name, Options.ToXML]);
  Result += AllFields.ToXML;
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

{ TImports }

function TImports.ToXML: AnsiString;
var
  S: AnsiString;

begin
  Result := '<Imports>';

  for S in Self do
    Result += Format('<Import Name = "%s"/>', [S]);

  Result += '</Imports>'#10;

end;

{ TPackages }

function TPackages.ToXML: AnsiString;
var
  S: AnsiString;

begin
  Result := '<Packages>';

  for S in Self do
    Result += Format('<Package Name = "%s"/>', [S]);

  Result += '</Packages>'#10;

end;

{ TMap }

function TMap.GetFieldType: TType;
begin
  Result := Format('map<%s, %s>', [FKeyType, FValueType]);

end;

function TMap.GetFPCTypeName: AnsiString;
begin
  Result := Format('T%s2%sMap', [
    Canonicalize(KeyPBType.Name), Canonicalize(ValuePBType.Name)]);

end;

constructor TMap.Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType,
  _ValueType: TType; _Options: TOptions; _Parent: TMessage);
begin
   inherited Create(_Name, '', false, _FieldNumber, _Options,
     CreateParent(nil, _Parent, nil));

   FKeyType := _KeyType;
   FValueType := _ValueType;
   FKeyPBType := TPBBaseType.Create(FKeyType, False, nil, CreateParent(nil, _Parent, nil));
   FValuePBType := TPBBaseType.Create(FValueType, False, nil, CreateParent(nil, _Parent, nil));

end;

destructor TMap.Destroy;
begin
  inherited Destroy;

end;

function TMap.ToXML: AnsiString;
begin
  Result := Format('<TMap Name = "%s" FieldNumber = "%d">' + sLineBreak +
                     '<Key Type = "%s" PPType = "%s"/>' + sLineBreak +
                     '<Value Type = "%s" PPType = "%s"/>' + sLineBreak +
                     '%s' + sLineBreak +
                    '</TMap>',
   [GetName, GetFieldNumber, KeyType, KeyPBType.FPCTypeName,
     ValueType, ValuePBType.FPCTypeName, GetOptions.ToXML]);
end;

function TMap.HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean;
begin
  Result := False;
end;

{ TOneOf }

function TOneOf.GetFieldNumbers: AnsiString;
var
  Field: TOneOfField;

begin
  Result := '';

  for Field in Fields do
  begin
    if Result <> '' then
      Result += ', ';
    Result += IntToStr(Field.FieldNumber);
  end;

end;

constructor TOneOf.Create(_Name: AnsiString; _Fields: TOneOfFields;
  ParentMessage: TMessage);
begin
  inherited Create(_Name, _Name, False, -1, TOptions.Create,
    CreateParent(nil, ParentMessage, nil));

  OneOfFields := _Fields;

end;

destructor TOneOf.Destroy;
begin
  OneOfFields.Free;

  inherited Destroy;
end;

function TOneOf.ToXML: AnsiString;
var
  OOField: TOneOfField;

begin
  Result:= '';
  Result += Format('<OneOf Name = "%s">'#10'%s',
    [Name, Options.ToXML]);

  for OOField in Self.OneOfFields do
    Result += OOField.ToXML;
  Result += Format('</OneOf>'#10, []);

end;

function TOneOf.HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean;
begin
  Result := False;
end;

{ TOneOfField }

function TOneOfField.GetCanonicalizeFullName: AnsiString;
var
  FieldName: AnsiString;
  ParentName: AnsiString;

begin
  FieldName := Canonicalize(Name);
  ParentName := Canonicalize(Parent.OneOf.Name);

  Result := Format('%s.%s', [ParentName, FieldName]);
end;

constructor TOneOfField.Create(_Name: AnsiString; _OneOfFieldType: TType;
  _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
  _Parent: TOneOf);
begin
  inherited Create(_Name, _OneOfFieldType, _IsRepeated, _FieldNumber, _Options,
    CreateParent(_Parent, nil, nil));

end;

function TOneOfField.ToXML: AnsiString;
begin
  Result:= Format('<TOneOfField Name = "%s" Type ="%s" FieldNumber = "%d"> %s </TOneOfField>',
   [Name,
    FieldType,
    FieldNumber,
    Options.ToXML]);

end;

{ TMessage }

function CompareMessageFields(const F1, F2: TMessageField): Integer;
begin
  Result := F1.FieldNumber - F2.FieldNumber;
end;

procedure TMessage.PrepareForCodeGeneration(AllClassesNames,
  AllEnumsNames: TStringList);
var
  Msg: TMessage;

begin
  for Msg in Self.Messages do
    Msg.PrepareForCodeGeneration(AllClassesNames, AllEnumsNames);

  FFields.Sort(@CompareMessageFields);
end;

function TMessage.GetFPCTypeName: AnsiString;
begin
  Result := 'T' + Canonicalize(Name);

end;


constructor TMessage.Create(_Name: AnsiString; _Fields: TMessageFields;
  _Messages: specialize TObjectList<TMessage>; _Options: TOptions;
  _Enums: TEnums; _Parent: TParent);
begin
  inherited Create;

  FName := _Name;
  FFields := _Fields;
  FMessages := _Messages;
  FOptions := _Options;
  FEnums := _Enums;
  FParent := _Parent;
end;

destructor TMessage.Destroy;
begin
  FFields.Free;
  FMessages.Free;
  FOptions.Free;
  FEnums.Free;

  inherited Destroy;
end;

function TMessage.ToXML: AnsiString;
begin
  Result:= Format('<TMessage Name = "%s"> %s%s%s%s</TMessage>'#10,
    [Name,
     Options.ToXML,
     Fields.ToXML,
     Messages.ToXML,
     Enums.ToXML]);
end;

{ TMessageField }

function TMessageField.GetCanonicalizeName: AnsiString;
begin
  Result := Canonicalize(FName);

end;

function TMessageField.GetFieldNumber: Integer;
begin
  Result := FFieldNumber;
end;

function TMessageField.GetFieldType: TType;
var
  Parts: TStringList;

begin
  Result := FFieldType;
  if Pos('.', Result) = 0 then
    Exit;

  Parts := TStringList.Create;
  Parts.Delimiter:= '.';
  Parts.DelimitedText := Result;

  Result := Parts.Strings[Parts.Count - 1];
  Parts.Free;

end;

function TMessageField.GetIsRepeated: Boolean;
begin
  Result := FIsRepeated;

end;

function TMessageField.GetCanonicalizeFullName: AnsiString;
begin
  Result := Canonicalize(GetName);
end;

function TMessageField.GetName: AnsiString;
begin
  Result := FName;

end;

function TMessageField.GetOptions: TOptions;
begin
  Result := FOptions;

end;

function TMessageField.GetFPCTypeName: AnsiString;
var
  ThisFieldType: AnsiString;

begin
  ThisFieldType := FieldType;

  if IsRepeated then
    Result := 'T' + Canonicalize(FName)
  else
    Result := GetNonRepeatedType4FPC(ThisFieldType);

end;

function TMessageField.GetPackageName: AnsiString;
var
  Parts: TStringList;

begin
  if Pos('.', FFieldType) = 0 then
    Exit('');

  Parts := TStringList.Create;
  Parts.Delimiter:= '.';
  Parts.DelimitedText := FFieldType;
  Parts.Delete(Parts.Count - 1);

  Result := JoinStrings(Parts, '.');

  Parts.Free;
end;

constructor TMessageField.Create(_Name: AnsiString; _FieldType: TType;
  _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
  _Parent: TParent);
begin
  inherited Create;

  FName := _Name;
  FFieldType := _FieldType;
  FIsRepeated := _IsRepeated;
  FFieldNumber := _FieldNumber;
  FOptions := _Options;
  FParent := _Parent;

end;

destructor TMessageField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TMessageField.ToXML: AnsiString;
begin
  Result:= Format('<MessageField Name = "%s" IsRepeated = "%s" Type = "%s" FieldNumber = "%d"> %s</MessageField>'#10,
  [GetName,
    IfThen(IsRepeated, 'True', 'False'),
    GetFieldType,
    GetFieldNumber,
    GetOptions.ToXML
  ]);

end;

function TMessageField.HasSimpleType(Proto: TProto; RelatedProtos: TProtos
  ): Boolean;
var
  TmpParent: TParent;
  P: TProto;

begin
  case FieldType of
    'double' , 'float' , 'int32' , 'int64' , 'uint32' , 'uint64'
      , 'sint32' , 'sint64' , 'fixed32' , 'fixed64' , 'sfixed32' , 'sfixed64'
      , 'bool' , 'string', 'byte':
      Exit(True);
  end;

  if PackageName = '' then
  begin
    TmpParent := Parent;
    while (TmpParent.Message <> nil) or (TmpParent.Proto <> nil) do
    begin
      if (TmpParent.Message <> nil) and (TmpParent.Message.Enums <> nil) then
        if TmpParent.Message.Enums.ByName[FieldType] <> nil then
          Exit(True);
      if (TmpParent.Proto <> nil) and (TmpParent.Proto.Enums <> nil) then
        if TmpParent.Proto.Enums.ByName[FieldType] <> nil then
          Exit(True);

      if TmpParent.Message <> nil then
        TmpParent := TmpParent.Message.Parent
      else
        Break;

    end;

    if (Proto.Enums <> nil) and (Proto.Enums.ByName[FieldType] <> nil) then
      Exit(True);

    Exit(False);
  end;

  for p in RelatedProtos do
  begin
    if p.PackageName <> PackageName then
      Continue;
    if p.Enums <> nil then
      Exit(p.Enums.ByName[FieldType] <> nil);
  end;

  raise Exception.Create('Someting went wrong!');
end;

{ TProto }

function TProto.GetOutputUnitName: AnsiString;
begin
  Result := GetUnitName(InputProtoFilename);

end;

function TProto.GetInputProtoFilename: AnsiString;
begin
  Result := GetValueFromOtherParams('InputProtoFilename:');

end;

function TProto.GetPackageName: AnsiString;
begin
  Result := GetValueFromOtherParams('package:');

end;

function TProto.GetSyntax: AnsiString;
begin
  Result := GetValueFromOtherParams('syntax:');

end;

function TProto.GetValueFromOtherParams(ParamName: AnsiString): AnsiString;
var
  Str: AnsiString;

begin
  for Str in FOtherParams do
    if IsPrefix(ParamName, Str) then
      Exit(Copy(Str, Length(ParamName) + 1, Length(Str)));

  Result := '';
end;

constructor TProto.Create(_Imports: TImports; _Options: TOptions;
  _Messages: TMessages; _Enums: TEnums; OtherParams: TStringList);
begin
  inherited Create;

  FImports := _Imports;
  FOptions := _Options;
  FMessages := _Messages;
  FEnums := _Enums;
  FOtherParams := OtherParams;

end;

destructor TProto.Destroy;
begin
  Imports.Free;
  Options.Free;
  Messages.Free;
  Enums.Free;
  FOtherParams.Free;

  inherited Destroy;
end;

function TProto.ToXML: AnsiString;
begin
  Result := Format('<TProto FileName = "%s" Path= "%s" Package = "%s" >'#10'%s%s%s%s </TProto>'#10,
  [ExtractFileName(InputProtoFilename),
   ExtractFileDir(InputProtoFilename),
   PackageName,
   Options.ToXML,
   Imports.ToXML,
   Messages.ToXML,
   Enums.ToXML
  ]);

end;


end.

