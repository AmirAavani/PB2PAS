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
    function GetFullName: AnsiString; virtual;
    function GetName: AnsiString; virtual;
    function GetPackage: AnsiString; virtual;


  public
    property Parent: TParent read FParent;
    property Name: AnsiString read GetName;
    property FullName: AnsiString read GetFullName;
    property PackageName: AnsiString read GetPackage;
    property Options: TOptions read FOptions;
    property NonRepeatedType4FPC: AnsiString read GetNonRepeatedType4FPC;
    property IsRepeated: Boolean read FIsRepeated;

    constructor Create(_Name: AnsiString; _IsRepeated: Boolean;
        _Options: TOptions; _Parent: TParent);
    function ToXML: AnsiString; virtual;

  end;

  { TMapPBType }

  TMapPBType = class(TPBBaseType)
  private
    FKeyPBType, FValuePBType: TPBBaseType;
  protected
    function GetFullName: AnsiString; override;
    function GetName: AnsiString; override;
    function GetPackage: AnsiString; override;

  public
    property KeyPBType: TPBBaseType read FKeyPBType;
    property ValuePBType: TPBBaseType read FValuePBType;

    constructor Create(_KeyType, _ValueType: TPBBaseType; _Parent: TParent);
    destructor Destroy; override;

  end;

  TPBBaseTypes = specialize TObjectCollection<TPBBaseType>;

  { TPBTypeWithFields }

  TPBTypeWithFields = class(TPBBaseType)
  protected
    FFields: TPBBaseTypes;

  public
    property Fields: TPBBaseTypes read FFields;

    constructor Create(aName: AnsiString; _IsRepeated: Boolean; _Options: TOptions;  _Parent: TParent);
    destructor Destroy; override;
  end;

  { TOneOfPBType }

  TOneOfPBType = class(TPBTypeWithFields)
  public
    property Fields: TPBBaseTypes read FFields;

    constructor Create(aName: AnsiString; _Parent: TParent);

  end;

  { TMessagePBType }

  TMessagePBType = class(TPBTypeWithFields)
  public
    constructor Create(aName: AnsiString; _Parent: TParent);

  end;

  { TMessageField }

  TMessageField = class(TObject)
  protected
    FFieldNumber: Integer;
    FFieldPBType: TPBBaseType;
    FName: AnsiString;
    FOptions: TOptions;

    function GetFieldNumber: Integer; virtual;
    function GetFieldType: TPBBaseType; virtual;
    function GetCanonicalizeFullName: AnsiString; virtual;
    function GetName: AnsiString; virtual;
    function GetOptions: TOptions;  virtual;
    function GetCanonicalizeFullNameForWriting: AnsiString; virtual;
    function GetCanonicalizeFullNameForReading: AnsiString; virtual;
    function GetCanonicalizeName: AnsiString; virtual;

  public
    property FieldType: TPBBaseType read GetFieldType;
    property Name: AnsiString read GetName;
    property CanonicalizeName: AnsiString read GetCanonicalizeName;
    property CanonicalizeFullName: AnsiString read GetCanonicalizeFullName;
    property CanonicalizeFullNameForWriting: AnsiString read GetCanonicalizeFullNameForWriting;
    property CanonicalizeFullNameForReading: AnsiString read GetCanonicalizeFullNameForReading;
    property FieldNumber: Integer read GetFieldNumber;
    property Options: TOptions read GetOptions;

    constructor Create(_Name: AnsiString; _FieldType: AnsiString; _IsRepeated: Boolean;
      _FieldNumber: Integer; _Options: TOptions; _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;
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

  TService = class(TPBBaseType)
  public type
    TRPCMethod = class(TPBBaseType)
    end;
    TRPCMethods = specialize TNamedObjectList<TRPCMethod>;

  protected
  public

  end;

  TServices = specialize TNamedObjectList<TService>;

  { TOneOfField }

  TOneOfField = class(TMessageField)
  protected
    function GetCanonicalizeFullNameForReading: AnsiString; override;
    function GetCanonicalizeFullName: AnsiString; override;
    function GetCanonicalizeFullNameForWriting: AnsiString; override;
    function GetCanonicalizeName: AnsiString; override;

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
    function GetOneOfFieldPBType: TOneOfPBType;
    function GetFieldType: TPBBaseType; override;

  protected
    function GetCanonicalizeFullNameForWriting: AnsiString; override;

  public
    property OneOfFieldPBType: TOneOfPBType read GetOneOfFieldPBType;
    property Fields: TOneOfFields read OneOfFields;
    property FieldNumbers: AnsiString read GetFieldNumbers;

    constructor Create(_Name: AnsiString; _Fields: TOneOfFields;
          ParentMessage: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString;override;
  end;

  TOneOfs = specialize TObjectList<TOneOf>;

  { TMap }

  TMap = class(TMessageField)
  private
    function GetMapFieldPBType: TMapPBType;

  protected
    function GetCanonicalizeFullNameForWriting: AnsiString; override;
    function GetCanonicalizeFullNameForReading: AnsiString; override;

  public
    property MapFieldPBType: TMapPBType read GetMapFieldPBType;

    constructor Create(_Name: AnsiString; _FieldNumber: Integer;
          _KeyType, _ValueType: AnsiString;
          _Parent: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString; override;
  end;


  TMaps = specialize TObjectList<TMap>;

  { TMessage }

  TMessage = class(TObject)
  private
    MessagePBType: TMessagePBType;
    FFields: TMessageFields;
    FMessages: specialize TNamedObjectList<TMessage>;
    FOptions: TOptions;
    FEnums: TEnums;
    FName: AnsiString;
    FParent: TParent;

    procedure PrepareForCodeGeneration(AllClassesNames, AllEnumsNames: TStringList);
  public
    property MessageType: TMessagePBType read MessagePBType;
    property Name: AnsiString read FName;
    property Fields: TMessageFields read FFields;
    property Messages: specialize TNamedObjectList<TMessage> read FMessages;
    property Options: TOptions read FOptions;
    property Enums: TEnums read FEnums;
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

{ TMessagePBType }

constructor TMessagePBType.Create(aName: AnsiString; _Parent: TParent
  );
begin
  inherited Create(aName, False, nil, _Parent);

end;

{ TOneOfPBType }

constructor TOneOfPBType.Create(aName: AnsiString; _Parent: TParent);
begin
  inherited Create(aName, False, nil, _Parent);

end;

{ TPBTypeWithFields }

constructor TPBTypeWithFields.Create(aName: AnsiString;
  _IsRepeated: Boolean; _Options: TOptions; _Parent: TParent);
begin
  inherited Create(aName, _IsRepeated, _Options, _Parent);

  FFields := TPBBaseTypes.Create;
end;

destructor TPBTypeWithFields.Destroy;
begin
  FFields.Free;

  inherited Destroy;
end;

{ TMapPBType }

function TMapPBType.GetFullName: AnsiString;
begin
  Result := Format('map<%s, %s>', [KeyPBType.Name, ValuePBType.Name]);

end;

function TMapPBType.GetName: AnsiString;
begin
  Result:= GetFullName;

end;

function TMapPBType.GetPackage: AnsiString;
begin
  Result := '';

end;

constructor TMapPBType.Create(_KeyType, _ValueType: TPBBaseType; _Parent: TParent
  );
begin
  inherited Create(Format('map<%s,%s>', [_KeyType.Name, _ValueType.Name]), False, nil, _Parent);

  FKeyPBType := _KeyType;
  FValuePBType := _ValueType;

end;

destructor TMapPBType.Destroy;
begin
  FKeyPBType.Free;
  FValuePBType.Free;

  inherited Destroy;
end;

{ TPBBaseType }

function TPBBaseType.GetNonRepeatedType4FPC: AnsiString;
begin
  if not IsRepeated then
    Exit('');

  Result := UtilsUnit.GetNonRepeatedType4FPC(Name);

end;

function TPBBaseType.GetFullName: AnsiString;
begin
  Result := FName;

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

constructor TPBBaseType.Create(_Name: AnsiString;
  _IsRepeated: Boolean; _Options: TOptions; _Parent: TParent);
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
  FAllFields.Free;

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

function TMap.GetCanonicalizeFullNameForWriting: AnsiString;
begin
  Result:= Format('Mutable%s', [Self.CanonicalizeFullName]);

end;

function TMap.GetCanonicalizeFullNameForReading: AnsiString;
begin
  Result:= Format('Get%s', [Self.CanonicalizeFullName]);

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

   FFieldPBType.Free;
   FFieldPBType := TMapPBType.Create(
     TPBBaseType.Create(_KeyType, False, nil, CreateParent(nil, _Parent, nil)),
     TPBBaseType.Create(_ValueType, False, nil, CreateParent(nil, _Parent, nil)),
     CreateParent(nil, _Parent, nil));

end;

destructor TMap.Destroy;
begin
  inherited Destroy;

end;

function TMap.ToXML: AnsiString;
begin
  Result := Format('<TMap Name = "%s" FieldNumber = "%d">' + sLineBreak +
                     '<FieldType>%s</FieldType>' + sLineBreak +
                    '</TMap>',
   [GetName, GetFieldNumber, FieldType.ToXML]);
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

function TOneOf.GetOneOfFieldPBType: TOneOfPBType;
var
  aField: TOneOfField;

begin
  if (FFieldPBType as TOneOfPBType).FFields = nil then
  begin
    for aField in Self.Fields do
      (FFieldPBType as TOneOfPBType).Fields.Add(aField.FieldType);

  end;

  Result := (FFieldPBType as TOneOfPBType);

end;

function TOneOf.GetFieldType: TPBBaseType;
begin
  Result := Self.OneOfFieldPBType;

end;

function TOneOf.GetCanonicalizeFullNameForWriting: AnsiString;
begin
  Result:= Format('Mutable%s', [GetCanonicalizeFullName]);

end;


constructor TOneOf.Create(_Name: AnsiString; _Fields: TOneOfFields;
  ParentMessage: TMessage);
begin
  inherited Create(_Name, _Name, False, -1, TOptions.Create,
    CreateParent(nil, ParentMessage, nil));

  OneOfFields := _Fields;
  FFieldPBType.Free;
  FFieldPBType := TOneOfPBType.Create(_Name, CreateParent(nil, ParentMessage, nil));

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

{ TOneOfField }

function TOneOfField.GetCanonicalizeFullNameForReading: AnsiString;
begin
  Result := Format('%s.Get%s', [FieldType.Parent.OneOf.GetCanonicalizeFullNameForReading,
    Canonicalize(GetName)]);
end;

function TOneOfField.GetCanonicalizeFullName: AnsiString;
var
  FieldName: AnsiString;
  ParentName: AnsiString;

begin
  FieldName := Canonicalize(Name);
  ParentName := Canonicalize(FieldType.Parent.OneOf.Name);

  Result := Format('%s.%s', [ParentName, FieldName]);

end;

function TOneOfField.GetCanonicalizeFullNameForWriting: AnsiString;
begin
  Result := Format('%s.%s', [FieldType.Parent.OneOf.GetCanonicalizeFullNameForWriting,
    Canonicalize(GetName)]);

end;

function TOneOfField.GetCanonicalizeName: AnsiString;
begin
  Result := Format('%s.%s', [FieldType.Parent.OneOf.GetCanonicalizeFullName,
    Canonicalize(GetName)]);

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
  Result := Format('<TOneOfField Name = "%s" Type ="%s" FieldNumber = "%d"> %s </TOneOfField>',
   [Name,
    FieldType.Name,
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

constructor TMessage.Create(_Name: AnsiString; _Fields: TMessageFields;
  _Messages: specialize TNamedObjectList<TMessage>; _Options: TOptions;
  _Enums: TEnums; _Parent: TParent);
var
  Field: TMessageField;

begin
  inherited Create;

  FName := _Name;
  FFields := _Fields;
  FMessages := _Messages;
  FOptions := _Options;
  FEnums := _Enums;
  FParent := _Parent;
  MessagePBType := TMessagePBType.Create(_Name, _Parent);

  for Field in FFields do
    MessagePBType.Fields.Add(Field.FieldType);

end;

destructor TMessage.Destroy;
begin
  FFields.Free;
  FMessages.Free;
  FOptions.Free;
  FEnums.Free;

  MessagePBType.Free;

  inherited Destroy;
end;

function TMessage.ToXML: AnsiString;
begin
  if Self = nil then
    Exit('<NoMsg/>');

  Result:= Format('<TMessage Name = "%s"> %s%s%s%s</TMessage>'#10,
    [Name,
     Options.ToXML,
     Fields.ToXML,
     Messages.ToXML,
     Enums.ToXML]);
end;

{ TMessageField }

function TMessageField.GetCanonicalizeFullNameForWriting: AnsiString;
begin
  Result := GetCanonicalizeFullName;

end;

function TMessageField.GetCanonicalizeFullNameForReading: AnsiString;
begin
  Result := Self.GetCanonicalizeFullName;

end;

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
  Result:= Format('<MessageField Name = "%s" IsRepeated = "%s" Type = "%s" FieldNumber = "%d"> %s</MessageField>'#10,
  [GetName,
    IfThen(FieldType. IsRepeated, 'True', 'False'),
    GetFieldType.Name,
    GetFieldNumber,
    GetOptions.ToXML
  ]);

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

