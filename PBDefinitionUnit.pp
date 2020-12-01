unit PBDefinitionUnit;

{$mode objfpc}{$H+}

interface

uses
  StreamUnit, PBTypeUnit, PBOptionUnit, ObjectListUnit, Classes, SysUtils, fgl;

type
  TIntList = specialize TFPGList<Integer>;
  TIdentifier = AnsiString;
  TFullIdentifier = AnsiString;
  TMessage = class;
  TImports = class;
  TProto = class;
  TOneOf = class;

  TParent = record
    OneOf: TOneOf;
    Message: TMessage;
    Proto: TProto;
  end;

  function CreateParent(anOneOf: TOneOf; aMessage: TMessage; aProto: TProto): TParent;

type

  { TMessageField }

  TMessageField = class(TObject)
  private
    function GetCanonicalizeName: AnsiString;
  protected
    FParent: TParent;
    FFieldNumber: Integer;
    FIsRepeated: Boolean;
    FFieldType: TBaseType;
    FName: AnsiString;
    FOptions: TOptions;

    function GetFieldNumber: Integer; virtual;
    function GetFieldType: TBaseType; virtual;
    function GetIsRepeated: Boolean; virtual;
    function GetCanonicalizeFullName: AnsiString; virtual;
    function GetName: AnsiString; virtual;
    function GetOptions: TOptions;  virtual;
    // Returns the translation of FieldType to FPC.
    function GetFPCType: AnsiString;  virtual;
    function GetPackageName: AnsiString; virtual;
  public
    property Parent: TParent read FParent;
    property IsRepeated: Boolean read GetIsRepeated;
    property FieldType: TBaseType read GetFieldType;
    property FPCType: AnsiString read GetFPCType;
    property PackageName: AnsiString read GetPackageName;
    property Name: AnsiString read GetName;
    property CanonicalizeName: AnsiString read GetCanonicalizeName;
    property CanonicalizeFullName: AnsiString read GetCanonicalizeFullName;
    property FieldNumber: Integer read GetFieldNumber;
    property Options: TOptions read GetOptions;

    constructor Create(_Name: AnsiString; _FieldType: TBaseType; _IsRepeated: Boolean;
      _FieldNumber: Integer; _Options: TOptions; _Parent: TParent);
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;
  end;

  TMessageFields = specialize TObjectList<TMessageField>;


  TEnums = specialize TObjectList<TEnum>;

  { TOneOfField }

  TOneOfField = class(TMessageField)
  protected
    function GetCanonicalizeFullName: AnsiString; override;

  public
    constructor Create(_Name: AnsiString; _OneOfFieldType: TBaseType;
         _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
         ParentOneOf: TOneOf);
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
  end;

  TOneOfs = specialize TObjectList<TOneOf>;

  { TMap }

  TMap = class(TMessageField)
  private
    FKeyType: TBaseType;
    FValueType: TBaseType;

    function GetFieldType: TBaseType; override;
    // Returns the translation of FieldType to FPC.
    function GetFPCType: AnsiString; override;

  public
    property KeyType: TBaseType read FKeyType;
    property ValueType: TBaseType read FValueType;

    constructor Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType, _ValueType: TBaseType;
           _Options: TOptions; _Parent: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString; override;
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

    MessageClassName: AnsiString;

    function GetFPCType: AnsiString;
    procedure PrepareForCodeGeneration(AllClassesNames, AllEnumsNames: TStringList);
  public
    // property Parent:
    property Name: AnsiString read FName;
    property Fields: TMessageFields read FFields;
    property Messages: specialize TObjectList<TMessage> read FMessages;
    property Options: TOptions read FOptions;
    property Enums: TEnums read FEnums;
    property FPCType: AnsiString read GetFPCType;
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
    Filename: AnsiString;
    FPackageName: AnsiString;
    FSyntax: AnsiString;
    FImports: TImports;
    FOptions: TOptions;
    FMessages: TMessages;
    FEnums: TEnums;
    function GetOutputUnitName: AnsiString;

  public
    property Imports: TImports read FImports;
    property Options: TOptions read FOptions;
    property Messages: TMessages read FMessages;
    property Enums: TEnums read FEnums;
    property Syntax: AnsiString read FSyntax;
    property InputProtoFilename: AnsiString read Filename;
    property PackageName: AnsiString read FPackageName;
    property OutputUnitName: AnsiString read GetOutputUnitName;

    constructor Create(_Filename: AnsiString; _Syntax: AnsiString;
      _Imports: TImports; _PackageName: AnsiString; _Options: TOptions;
  _Messages: TMessages; _Enums: TEnums);
    destructor Destroy; override;

    function ToXML: AnsiString;

  end;

  { TProto3 }

  TProto3 = class(TProto)
  public

  end;

  TStrLit = AnsiString;
  TConstant = AnsiString;

implementation
uses
  UtilsUnit, StringUnit, strutils;

function CreateParent(anOneOf: TOneOf; aMessage: TMessage; aProto: TProto): TParent;
begin
  Result.OneOf := anOneOf;
  Result.Message := aMessage;
  Result.Proto := aProto
  ;
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

function TMap.GetFieldType: TBaseType;
begin
  Result := TBaseType.Create(Format('map<%s, %s>', [FKeyType, FValueType]));

end;

function TMap.GetFPCType: AnsiString;
begin
  Result := Format('T%s2%sMap', [KeyType, ValueType]);

end;

constructor TMap.Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType,
  _ValueType: TBaseType; _Options: TOptions; _Parent: TMessage);
begin
   inherited Create(_Name, nil, false, _FieldNumber, _Options,
     CreateParent(nil, _Parent, nil));

   FKeyType := _KeyType;
   FValueType := _ValueType;

end;

destructor TMap.Destroy;
begin
  inherited Destroy;
end;

function TMap.ToXML: AnsiString;
begin
  Result := Format('<TMap Name = "%s" KeyType = "%s" ValueType = "%s" FieldNumber = "%d">%s</TMap>',
   [GetName, FKeyType.ProtoType, FValueType.ProtoType, GetFieldNumber, GetOptions.ToXML]);
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
  inherited Create(_Name, TBaseType.Create(_Name), False, -1, TOptions.Create,
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

constructor TOneOfField.Create(_Name: AnsiString; _OneOfFieldType: TBaseType;
  _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions;
  ParentOneOf: TOneOf);
begin
  inherited Create(_Name, _OneOfFieldType, _IsRepeated, _FieldNumber, _Options,
    CreateParent(ParentOneOf, nil, nil));

end;

function TOneOfField.ToXML: AnsiString;
begin
  Result:= Format('<TOneOfField Name = "%s" Type ="%s" FieldNumber = "%d"> %s </TOneOfField>',
   [Name,
    FieldType.ProtoType,
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

function TMessage.GetFPCType: AnsiString;
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

function TMessageField.GetFieldType: TBaseType;
var
  Parts: TStringList;

begin
  Result := FFieldType;
  if Pos('.', Result.ProtoType) = 0 then
    Exit;

  Parts := TStringList.Create;
  Parts.Delimiter:= '.';
  Parts.DelimitedText := Result.ProtoType;

  Result := TBaseType.Create(Parts.Strings[Parts.Count - 1]);
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

function TMessageField.GetFPCType: AnsiString;
var
  ThisFieldType: AnsiString;

begin
  ThisFieldType := FieldType.ProtoType;

  if IsRepeated then
    Result := 'T' + Canonicalize(FName)
  else
    Result := GetNonRepeatedType4FPC(ThisFieldType);

end;

function TMessageField.GetPackageName: AnsiString;
var
  Parts: TStringList;

begin
  if Pos('.', FFieldType.ProtoType) = 0 then
    Exit('');

  Parts := TStringList.Create;
  Parts.Delimiter:= '.';
  Parts.DelimitedText := FFieldType.ProtoType;
  Parts.Delete(Parts.Count - 1);

  Result := JoinStrings(Parts, '.');

  Parts.Free;
end;

constructor TMessageField.Create(_Name: AnsiString; _FieldType: TBaseType;
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
    GetFieldType.ProtoType,
    GetFieldNumber,
    GetOptions.ToXML
  ]);

end;

{ TProto }

function TProto.GetOutputUnitName: AnsiString;
begin
  Result := GetUnitName(InputProtoFilename);

end;

constructor TProto.Create(_Filename: AnsiString; _Syntax: AnsiString;
  _Imports: TImports; _PackageName: AnsiString; _Options: TOptions;
  _Messages: TMessages; _Enums: TEnums);
begin
  inherited Create;

  Filename := _Filename;
  FSyntax := _Syntax;
  FImports := _Imports;
  FPackageName := _PackageName;
  FOptions := _Options;
  FMessages := _Messages;
  FEnums := _Enums;

end;

destructor TProto.Destroy;
begin
  Imports.Free;
  Options.Free;
  Messages.Free;
  Enums.Free;

  inherited Destroy;
end;

function TProto.ToXML: AnsiString;
begin
  Result := Format('<TProto FileName = "%s" Path= "%s" Package = "%s" >'#10'%s%s%s%s </TProto>'#10,
  [ExtractFileName(Filename),
   ExtractFileDir(Filename),
   PackageName,
   Options.ToXML,
   Imports.ToXML,
   Messages.ToXML,
   Enums.ToXML
  ]);

end;


end.

