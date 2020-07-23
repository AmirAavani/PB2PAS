unit PBDefinitionUnit;

{$mode objfpc}{$H+}

interface

uses
  StreamUnit, Classes, SysUtils, fgl;

type
  TIntList = specialize TFPGList<Integer>;
  TIdentifier = AnsiString;
  TFullIdentifier = AnsiString;
  TOptionName = AnsiString;
  TConstValue = AnsiString;
  TType = AnsiString;

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


  { TObjectList }

  generic TObjectList<TObject> = class(specialize TFPGList<TObject>)
  private
    function GetByName(aName: AnsiString): TObject;
  public
    property ByName[aName: AnsiString]: TObject read GetByName;
    destructor Destroy; override;

    function ToXML: AnsiString;
  end;

  TOptions = specialize TObjectList<TOption>;

  TMessage = class;
  TImports = class;
  TProto = class;

  TParent = record
    Message: TMessage;
    Proto: TProto;
  end;

  function CreateParent(aMessage: TMessage; aProto: TProto): TParent;

type

  { TMessageField }

  TMessageField = class(TObject)
  private
    FParent: TParent;
    FFieldNumber: Integer;
    FIsRepeated: Boolean;
    FFieldType: TType;
    FName: AnsiString;
    FOptions: TOptions;

    function GetFieldNumber: Integer; virtual;
    function GetFieldType: TType; virtual;
    function GetIsRepeated: Boolean; virtual;
    function GetName: AnsiString; virtual;
    function GetOptions: TOptions;  virtual;
    // Returns the translation of FieldType to FPC.
    function GetFPCType: AnsiString;  virtual;
    function GetPackageName: AnsiString; virtual;
  public
    property Parent: TParent read FParent;
    property IsRepeated: Boolean read GetIsRepeated;
    property FieldType: TType read GetFieldType;
    property FPCType: AnsiString read GetFPCType;
    property PackageName: AnsiString read GetPackageName;
    property Name: AnsiString read GetName;
    property FieldNumber: Integer read GetFieldNumber;
    property Options: TOptions read GetOptions;

    constructor Create(_Name: AnsiString; _FieldType: TType; _IsRepeated: Boolean;
      _FieldNumber: Integer; _Options: TOptions; ParentMessage: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString; virtual;
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


  TEnums = specialize TObjectList<TEnum>;

  { TOneOfField }

  TOneOfField = class(TObject)
  private
    FOneOfFieldType: AnsiString;
    FName: AnsiString;
    FOptions: specialize TObjectList<TOption>;
    FFieldNumber: Integer;
    function GetFPCType: AnsiString;
  public
    property OneOfFieldType: TType read FOneOfFieldType;
    property Name: AnsiString read FName;
    property FieldNumber: Integer read FFieldNumber;
    property FPCType: AnsiString read GetFPCType;

    constructor Create(_Name: AnsiString; _OneOfFieldType: TType;
          _FieldNumber: Integer; _Options: TOptions);
    destructor Destroy; override;
    function ToXML: AnsiString;

  end;

  TOneOfFields = specialize TObjectList<TOneOfField>;
  { TOneOf }

  TOneOf = class(TMessageField)
  private
    OneOfFields: TOneOfFields;

  public
    property Fields: TOneOfFields read OneOfFields;
    constructor Create(_Name: AnsiString; _Fields: TOneOfFields;
          ParentMessage: TMessage);
    destructor Destroy; override;

    function ToXML: AnsiString;override;
  end;

  TOneOfs = specialize TObjectList<TOneOf>;

  { TMap }

  TMap = class(TMessageField)
  private
    FKeyType: TType;
    FValueType: TType;

    function GetFieldType: TType; override;
    // Returns the translation of FieldType to FPC.
    function GetFPCType: AnsiString; override;

  public
    property KeyType: TType read FKeyType;
    property ValueType: TType read FValueType;

    constructor Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType, _ValueType: TType;
           _Options: TOptions; ParentMessage: TMessage);
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

function CreateParent(aMessage: TMessage; aProto: TProto): TParent;
begin
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

function TMap.GetFieldType: TType;
begin
  Result:= Format('map<%s, %s>', [FKeyType, FValueType]);

end;

function TMap.GetFPCType: AnsiString;
begin
  Result:= Format('T%s2%sMap', [KeyType, ValueType]);

end;

constructor TMap.Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType,
  _ValueType: TType; _Options: TOptions; ParentMessage: TMessage);
begin
   inherited Create(_Name, '', false, _FieldNumber, _Options, ParentMessage);

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
   [GetName, FKeyType, FValueType, GetFieldNumber, GetOptions.ToXML]);
end;

{ TOneOf }

constructor TOneOf.Create(_Name: AnsiString; _Fields: TOneOfFields;
  ParentMessage: TMessage);
begin
  inherited Create(_Name, _Name, False, -1, TOptions.Create, ParentMessage);

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

function TOneOfField.GetFPCType: AnsiString;
begin
  Result := GetNonRepeatedType4FPC(OneOfFieldType);
end;

constructor TOneOfField.Create(_Name: AnsiString; _OneOfFieldType: TType;
  _FieldNumber: Integer; _Options: TOptions);
begin
  inherited Create;

  FName := _Name;
  FOneOfFieldType := _OneOfFieldType;
  FFieldNumber := _FieldNumber;
  FOptions := _Options;
end;

destructor TOneOfField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TOneOfField.ToXML: AnsiString;
begin
  Result:= Format('<TOneOfField Name = "%s" Type ="%s" FieldNumber = "%d"> %s </TOneOfField>',
   [FName,
    FOneOfFieldType,
    FFieldNumber,
    FOptions.ToXML]);

end;

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

constructor TMessageField.Create(_Name: AnsiString;
  _FieldType: TType; _IsRepeated: Boolean; _FieldNumber: Integer;
  _Options: TOptions; ParentMessage: TMessage);
begin
  inherited Create;

  FName := _Name;
  FFieldType := _FieldType;
  FIsRepeated := _IsRepeated;
  FFieldNumber := _FieldNumber;
  FOptions := _Options;
  FParent.Message := ParentMessage;

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

{ TOption }

constructor TOption.Create(Name: TOptionName; Value: TConstValue);
begin
  inherited Create;

  FOptionName := Name;
  FConstValue := Value;
end;

function MaybeRemoveQuotations(aValue: AnsiString): AnsiString;
begin
  if aValue = '' then
    Exit(aValue);
  if (aValue[1] = aValue[Length(aValue)]) and ((aValue[1] = '''') or (aValue[1] = '"')) then
    Result := Copy(aValue, 2, Length(aValue) - 2);
end;

function TOption.ToXML: AnsiString;
begin
  Result := Format('<Option Name = "%s" Value = "%s"/>'#10, [OptionName, MaybeRemoveQuotations(ConstValue)]);

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

