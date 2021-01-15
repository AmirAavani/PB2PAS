unit PBDefinitionUnit;

{$mode objfpc}{$H+}

interface

uses
  PBOptionUnit, Classes, SysUtils, Generics.Collections,
  NamedObjectListUnit, GenericCollectionUnit;

type
  TIntList = specialize TList<Integer>;
  TIdentifier = AnsiString;
  TFullIdentifier = AnsiString;
  TMessage = class;
  TImports = class;
  TProto = class;
  TProtos = specialize TList<TProto>;

  TOneOf = class;
  // TType = AnsiString;

  TParent = record
    OneOf: TOneOf;
    Message: TMessage;
    Proto: TProto;
  end;

type

  { TPBBaseType }

  TPBBaseType = class(TObject)
  protected
    FParent: TParent;
    FName: AnsiString;
    FOptions: TOptions;
    FIsRepeated: Boolean;

  protected
    function GetNonRepeatedType4FPC: AnsiString; virtual;
    function GetFPCTypeName: AnsiString; virtual;
    function GetFullName: AnsiString; virtual;
    function GetIsSimple: Boolean; virtual;
    function GetName: AnsiString; virtual;
    function GetPackage: AnsiString; virtual;


  public
    property Parent: TParent read FParent;
    property Name: AnsiString read GetName;
    property FullName: AnsiString read GetFullName;
    property PackageName: AnsiString read GetPackage;
    property Options: TOptions read FOptions;
    property FPCTypeName: AnsiString read GetFPCTypeName;
    property NonRepeatedType4FPC: AnsiString read GetNonRepeatedType4FPC;
    property IsRepeated: Boolean read FIsRepeated;

    constructor Create(_Name: AnsiString; _IsRepeated: Boolean;
        _Options: TOptions; _Parent: TParent);
    function ToXML: AnsiString; virtual;
    function IsAnEnumType(Proto: TProto; RelatedProtos: TProtos): Boolean; virtual;
    function IsSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; virtual;

  end;

  { TMapPBType }

  TMapPBType = class(TPBBaseType)
  private
    FKeyPBType, FValuePBType: TPBBaseType;
  protected
    function GetFPCTypeName: AnsiString; override;
    function GetFullName: AnsiString; override;
    function GetIsSimple: Boolean; override;
    function GetPackage: AnsiString; override;

  public
    property KeyPBType: TPBBaseType read FKeyPBType;
    property ValuePBType: TPBBaseType read FValuePBType;

    constructor Create(_KeyType, _ValueType: TPBBaseType; _Parent: TParent);
    destructor Destroy; override;

    function IsAnEnumType(Proto: TProto; RelatedProtos: TProtos): Boolean; override;
    function IsSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; override;

  end;

  { TMessageField }

  TMessageField = class(TObject)
  private
    function GetCanonicalizeName: AnsiString;

  protected
    FParent: TParent;
    FFieldNumber: Integer;
    // FFieldType: TType;
    FFieldPBType: TPBBaseType;
    FName: AnsiString;
    FOptions: TOptions;

    function GetFieldNumber: Integer; virtual;
    function GetFieldType: TPBBaseType; virtual;
    function GetCanonicalizeFullName: AnsiString; virtual;
    function GetName: AnsiString; virtual;
    function GetOptions: TOptions;  virtual;
  public
    property Parent: TParent read FParent;
    property FieldType: TPBBaseType read GetFieldType;
    property Name: AnsiString read GetName;
    property CanonicalizeName: AnsiString read GetCanonicalizeName;
    property CanonicalizeFullName: AnsiString read GetCanonicalizeFullName;
    property FieldNumber: Integer read GetFieldNumber;
    property Options: TOptions read GetOptions;

    constructor Create(_Name: AnsiString; _FieldType: AnsiString; _IsRepeated: Boolean;
      _FieldNumber: Integer; _Options: TOptions; _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;

    function HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; virtual;
  end;

  TMessageFields = specialize TNamedObjectList<TMessageField>;

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
    property Options: TOptions read FOptions;
    property FPCValue: AnsiString read GetFPCValue;

    constructor Create(_EnumName, _Name: AnsiString; _Options: TOptions; _Value: Integer);
    destructor Destroy; override;
    function ToXML: AnsiString;

  end;

  TEnumFields = specialize TNamedObjectList<TEnumField>;

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

  TEnums = specialize TNamedObjectList<TEnum>;

  { TOneOfField }

  TOneOfField = class(TMessageField)
  protected
    function GetCanonicalizeFullName: AnsiString; override;

  public
    constructor Create(_Name: AnsiString; _OneOfFieldType: AnsiString;
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
    function GetMapFieldPBType: TMapPBType;

  public
    property MapFieldPBType: TMapPBType read GetMapFieldPBType;

    constructor Create(_Name: AnsiString; _FieldNumber: Integer;
          _KeyType, _ValueType: AnsiString;
          _Parent: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString; override;
    function HasSimpleType(Proto: TProto; RelatedProtos: TProtos): Boolean; override;
  end;


  TMaps = specialize TObjectList<TMap>;

  { TMessage }

  TMessage = class(TObject)
  private
    FFields: TMessageFields;
    FMessages: specialize TNamedObjectList<TMessage>;
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
    property Messages: specialize TNamedObjectList<TMessage> read FMessages;
    property Options: TOptions read FOptions;
    property Enums: TEnums read FEnums;
    property FPCTypeName: AnsiString read GetFPCTypeName;
    property Parent: TParent read FParent;

    constructor Create(_Name: AnsiString;
      _Fields: TMessageFields;
      _Messages: specialize TNamedObjectList<TMessage>;
      _Options:  TOptions;
      _Enums:  TEnums;
      _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString;
  end;

  TMessages = specialize TNamedObjectList<TMessage>;

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
  TProtoMap = specialize TMapSimpleKeyObjectValue<AnsiString, TProto>;

function CreateParent(anOneOf: TOneOf; aMessage: TMessage; aProto: TProto): TParent;


implementation
uses
  UtilsUnit, StringUnit, strutils, Generics.Defaults;

function CreateParent(anOneOf: TOneOf; aMessage: TMessage; aProto: TProto): TParent;
begin
  Result.OneOf := anOneOf;
  Result.Message := aMessage;
  Result.Proto := aProto;

end;

{ TMapPBType }

function TMapPBType.GetFPCTypeName: AnsiString;
begin
  Result := Format('T%sTo%sMap', [Canonicalize(KeyPBType.Name), Canonicalize(ValuePBType.Name)]);

end;

function TMapPBType.GetFullName: AnsiString;
begin
  Result := Format('T%sTo%sMap', [Canonicalize(KeyPBType.Name), Canonicalize(ValuePBType.Name)]);

end;

function TMapPBType.GetIsSimple: Boolean;
begin
  Result := False;

end;

function TMapPBType.GetPackage: AnsiString;
begin
  Result := '';

end;

constructor TMapPBType.Create(_KeyType, _ValueType: TPBBaseType; _Parent: TParent
  );
begin
  inherited Create('Map', False, nil, _Parent);

  FKeyPBType := _KeyType;
  FValuePBType := _ValueType;

end;

destructor TMapPBType.Destroy;
begin
  FKeyPBType.Free;
  FValuePBType.Free;

  inherited Destroy;
end;

function TMapPBType.IsAnEnumType(Proto: TProto; RelatedProtos: TProtos
  ): Boolean;
begin
  Result := False;
end;

function TMapPBType.IsSimpleType(Proto: TProto; RelatedProtos: TProtos
  ): Boolean;
begin
  Result := False;

end;

{ TPBBaseType }

function TPBBaseType.GetNonRepeatedType4FPC: AnsiString;
begin
  if not IsRepeated then
    Exit('');

  Result := UtilsUnit.GetNonRepeatedType4FPC(Name);

end;

function TPBBaseType.GetFPCTypeName: AnsiString;
begin
  if IsRepeated then
    Result := 'T' + Canonicalize(Name)
  else
    Result := UtilsUnit.GetNonRepeatedType4FPC(Name);

end;

function TPBBaseType.GetFullName: AnsiString;
begin
  Result := FName;

end;

function TPBBaseType.GetIsSimple: Boolean;
begin
  Result := False;
  case Name of
    'double' , 'float' , 'int32' , 'int64' , 'uint32' , 'uint64'
      , 'sint32' , 'sint64' , 'fixed32' , 'fixed64' , 'sfixed32' , 'sfixed64'
      , 'bool' , 'string', 'byte':
      Result := True;
  end;

end;

function TPBBaseType.GetName: AnsiString;
var
  StrList: TStringList;

begin
  StrList := Split(GetFullName, '.');

  Result := StrList[StrList.Count - 1];

  StrList.Free;

end;

function TPBBaseType.GetPackage: AnsiString;
var
  StrList: TStringList;

begin
  StrList := Split(GetFullName, '.');
  StrList.Delete(StrList.Count - 1);
  Result := '';

  if StrList.Count <> 0 then
    Result := JoinStrings(StrList, '.');

  StrList.Free;

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
  Result := Format('<%s FullName = "%s" Package = "%s" Name = "%s">%s',
    [Self.ClassName, FullName, PackageName, Name, FOptions.ToXML]);

end;

function TPBBaseType.IsSimpleType(Proto: TProto; RelatedProtos: TProtos
  ): Boolean;
var
  p: TProto;
  TmpParent: TParent;

begin
  case Name of
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
        if TmpParent.Message.Enums.ByName[Name] <> nil then
          Exit(True);
      if (TmpParent.Proto <> nil) and (TmpParent.Proto.Enums <> nil) then
        if TmpParent.Proto.Enums.ByName[Name] <> nil then
          Exit(True);

      if TmpParent.Message <> nil then
        TmpParent := TmpParent.Message.Parent
      else
        Break;

    end;

    if (Proto.Enums <> nil) and (Proto.Enums.ByName[Name] <> nil) then
      Exit(True);

    Exit(False);
  end;

  for p in RelatedProtos do
  begin
    if p.PackageName <> PackageName then
      Continue;
    if p.Enums <> nil then
      Exit(p.Enums.ByName[Name] <> nil);
  end;

  raise Exception.Create('Someting went wrong!');

end;

function TPBBaseType.IsAnEnumType(Proto: TProto; RelatedProtos: TProtos
  ): Boolean;
var
  ParentInfo: TParent;
  P: TProto;

begin
  case Self.Name of
    'double' , 'float' , 'int32' , 'int64' , 'uint32' , 'uint64'
      , 'sint32' , 'sint64' , 'fixed32' , 'fixed64' , 'sfixed32' , 'sfixed64'
      , 'bool' , 'string' , 'byte', 'bytes': Exit(False);
  end;

  if Self.PackageName = '' then
  begin
    ParentInfo := Self.Parent;
    while (Parent.Message <> nil) or (Parent.Proto <> nil) do
    begin
      if (ParentInfo.Message <> nil) and (ParentInfo.Message.Enums <> nil) then
        if ParentInfo.Message.Enums.ByName[Self.Name] <> nil then
          Exit(True);
      if (ParentInfo.Proto <> nil) and (ParentInfo.Proto.Enums <> nil) then
        if ParentInfo.Proto.Enums.ByName[Self.Name] <> nil then
          Exit(True);

      if ParentInfo.Message <> nil then
        ParentInfo := ParentInfo.Message.Parent
      else
        Break;

    end;

    if (Proto.Enums <> nil) and (Proto.Enums.ByName[Self.Name] <> nil) then
      Exit(True);

    Exit(False);
  end;

  for p in RelatedProtos do
  begin
    if p.PackageName <> Self.PackageName then
      Continue;
    if p.Enums <> nil then
      Exit(p.Enums.ByName[Self.Name] <> nil);
  end;

  Result := False;

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

function TMap.GetMapFieldPBType: TMapPBType;
begin
  Result := FFieldPBType as TMapPBType;

end;

constructor TMap.Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType,
  _ValueType: AnsiString; _Parent: TMessage);
begin
   inherited Create(_Name,
     '',
     False,
     _FieldNumber,
     nil,
     CreateParent(nil, _Parent, nil));

   FFieldPBType := TMapPBType.Create(
     TPBBaseType.Create(_KeyType, False, nil, CreateParent(nil, _Parent, nil)),
     TPBBaseType.Create(_ValueType, False, nil, CreateParent(nil, _Parent, nil)),
     CreateParent(nil, _Parent, nil));

end;

destructor TMap.Destroy;
begin
  FFieldPBType.Free;

  inherited Destroy;

end;

function TMap.ToXML: AnsiString;
begin
  Result := Format('<TMap Name = "%s" FieldNumber = "%d">' + sLineBreak +
                     '<FieldType>%s</FieldType>' + sLineBreak +
                    '</TMap>',
   [GetName, GetFieldNumber, FieldType.ToXML]);
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
  ParentName := Canonicalize(FieldType.Parent.OneOf.Name);

  Result := Format('%s.%s', [ParentName, FieldName]);
end;

constructor TOneOfField.Create(_Name: AnsiString; _OneOfFieldType: AnsiString;
  _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
  _Parent: TOneOf);
begin
  inherited Create(_Name, _OneOfFieldType, _IsRepeated, _FieldNumber, _Options,
    CreateParent(_Parent, nil, nil));

end;

function TOneOfField.ToXML: AnsiString;
begin
  Result := Format('<TOneOfField Name = "%s" Type ="%s" FPCType="%s" FieldNumber = "%d"> %s </TOneOfField>',
   [Name,
    FieldType.Name,
    FieldType.FPCTypeName,
    FieldNumber,
    Options.ToXML]);

end;

{ TMessage }

function CompareMessageFields(constref Item1, Item2: TMessageField): Integer;
begin
  Result := Item1.FieldNumber - Item2.FieldNumber;

end;

procedure TMessage.PrepareForCodeGeneration(AllClassesNames,
  AllEnumsNames: TStringList);
var
  Msg: TMessage;

begin
  for Msg in Self.Messages do
    Msg.PrepareForCodeGeneration(AllClassesNames, AllEnumsNames);

  FFields.Sort(specialize TComparer<TMessageField>.Construct(@CompareMessageFields));
end;

function TMessage.GetFPCTypeName: AnsiString;
begin
  Result := 'T' + Canonicalize(Name);

end;


constructor TMessage.Create(_Name: AnsiString; _Fields: TMessageFields;
  _Messages: specialize TNamedObjectList<TMessage>; _Options: TOptions;
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

function TMessageField.GetFieldType: TPBBaseType;
begin
  Result := FFieldPBType;

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

constructor TMessageField.Create(_Name: AnsiString; _FieldType: AnsiString;
  _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
  _Parent: TParent);
begin
  inherited Create;

  FName := _Name;
  FFieldPBType :=  TPBBaseType.Create(_FieldType, _IsRepeated, _Options, _Parent);
  FFieldNumber := _FieldNumber;
  FOptions := _Options;

end;

destructor TMessageField.Destroy;
begin
  FOptions.Free;
  FFieldPBType.Free;

  inherited Destroy;
end;

function TMessageField.ToXML: AnsiString;
begin
  Result:= Format('<MessageField Name = "%s" IsRepeated = "%s" Type = "%s" FPCType = "%s" FieldNumber = "%d"> %s</MessageField>'#10,
  [GetName,
    IfThen(FieldType. IsRepeated, 'True', 'False'),
    GetFieldType.Name,
    GetFieldType.FPCTypeName,
    GetFieldNumber,
    GetOptions.ToXML
  ]);

end;

function TMessageField.HasSimpleType(Proto: TProto; RelatedProtos: TProtos
  ): Boolean;
begin
  Exit(FieldType.IsSimpleType(Proto, RelatedProtos));

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

