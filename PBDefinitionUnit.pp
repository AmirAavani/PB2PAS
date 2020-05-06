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

    property OptionName: TOptionName read FOptionName;
    property ConstValue: TConstValue read FConstValue;

    function ToXML: AnsiString;
  end;


  { TObjectList }

  generic TObjectList<TObject> = class(specialize TFPGList<TObject>)
  public
    destructor Destroy; override;

    function ToXML: AnsiString;
  end;

  TOptions = specialize TObjectList<TOption>;


  { TMessageField }

  TMessageField = class(TObject)
  private
    FFieldNumber: Integer;
    FIsRepeated: Boolean;
    FFieldType: TType;
    FName: AnsiString;
    FOptions: TOptions;

    function ApplyPattern(MessageClassName: AnsiString; const Template: AnsiString): AnsiString;
    procedure GenerateDeclaration(MessageClassName: AnsiString; Output: TMyTextStream);
    procedure GenerateImplementation(MessageClassName: AnsiString; Output: TMyTextStream);

    function GetDefaultValue: AnsiString; virtual;
    procedure GenerateToString(Output: TMyTextStream);
    function GetFieldNumber: Integer; virtual;
    function GetFieldType: TType; virtual;
    function GetIsRepeated: Boolean; virtual;
    function GetName: AnsiString; virtual;
    function GetOptions: TOptions;  virtual;
    // Returns the translation of FieldType to FPC.
    function GetType: AnsiString;  virtual;

  public
    property IsRepeated: Boolean read GetIsRepeated;
    property FieldType: TType read GetFieldType;
    property Name: AnsiString read GetName;
    property FieldNumber: Integer read GetFieldNumber;
    property Options: TOptions read GetOptions;
    property DefaultValue: AnsiString read GetDefaultValue;

    constructor Create(_Name: AnsiString; _FieldType: TType; _IsRepeated: Boolean;
      _FieldNumber: Integer; _Options: TOptions);
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
  public
    property Name: AnsiString read FName;
    property Value: Integer read FValue;
    property Options: specialize TObjectList<TOption> read FOptions;

    constructor Create(_Name: AnsiString; _Options: TOptions; _Value: Integer);
    destructor Destroy; override;
    function ToXML: AnsiString;

  end;

  TEnumFields = specialize TObjectList<TEnumField>;

  { TEnum }

  TEnum = class(TEnumFields)
  private
    FName: AnsiString;
    FOptions: TOptions;
    procedure GenerateCode(Stream: TMyTextStream);
    procedure PrepareForCodeGeneration;
  public
    property Name: AnsiString read FName;

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
  public
    property OneOfFieldType: TType read FOneOfFieldType;
    property Name: AnsiString read FName;
    property FieldNumber: Integer read FFieldNumber;

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

    procedure GenerateDeclaration(Output: TMyTextStream);
  public

    constructor Create(_Name: AnsiString; _Fields: TOneOfFields);

    function ToXML: AnsiString;override;
  end;

  TOneOfs = specialize TObjectList<TOneOf>;

  { TMap }

  TMap = class(TMessageField)
  private
    FKeyType: TType;
    FValueType: TType;

    function GetDefaultValue: AnsiString; override;
    function GetFieldType: TType; override;
    // Returns the translation of FieldType to FPC.
    function GetType: AnsiString; override;

  public
    property KeyType: TType read FKeyType;
    property ValueType: TType read FValueType;

    constructor Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType, _ValueType: TType;
           _Options: TOptions);

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

    MessageClassName: AnsiString;

    procedure PrepareForCodeGeneration(AllClassesNames, AllEnumsNames: TStringList);
    procedure GenerateDeclaration(Output: TMyTextStream);
    procedure GenerateImplementation(Output: TMyTextStream);

    function HasRepeatedHasNonSimple: Boolean;
  public
    property Name: AnsiString read FName;
    property Fields: TMessageFields read FFields;
    property Messages: specialize TObjectList<TMessage> read FMessages;
    property Options: TOptions read FOptions;
    property Enums: TEnums read FEnums;

    constructor Create(_Name: AnsiString;
      _Fields: TMessageFields;
      _Messages: specialize TObjectList<TMessage>;
      _Options:  TOptions;
      _Enums:  TEnums);
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
    Syntax: AnsiString;
    Imports: TImports;
    Packages: TPackages;
    Options: TOptions;
    Messages: TMessages;
    Enums: TEnums;


    AllClassesNames: TStringList;
    AllEnumsNames: TStringList;

    procedure PrepareForCodeGeneration;
    procedure GenerateCode(
        OutputUnitName: AnsiString; OutputStream: TStream); virtual; abstract;

  public
    constructor Create(_Filename: AnsiString; _Syntax: AnsiString; _Imports: TImports; _Packages: TPackages;
        _Options: TOptions; _Messages: TMessages; _Enums: TEnums);
    destructor Destroy; override;

    function ToXML: AnsiString;

  end;

  { TProto3 }

  TProto3 = class(TProto)
    procedure GenerateCode(
        OutputUnitName: AnsiString; OutputStream: TStream); override;
  public

  end;

  TStrLit = AnsiString;
  TConstant = AnsiString;

implementation
uses
  UtilsUnit, strutils;

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

{ TProto3 }

procedure TProto3.GenerateCode(OutputUnitName: AnsiString; OutputStream: TStream);
var
  Output: TMyTextStream;
  Enum: TEnum;
  Message: TMessage;

begin
  {
  Output := TMyTextStream.Create(OutputStream, False);

  Output.WriteLine(Format('unit %s;', [OutputUnitName]));
  Output.WriteLine(Format('{$Mode objfpc}', []));
  Output.WriteLine(Format('interface', []));
  Output.WriteLine;
  Output.WriteLine(GenerateCodeForImports(Imports, Output));

  Output.WriteLine;
  if (Enums.Count <> 0) or (Messages.Count <> 0) then
    Output.WriteLine(Format('type', []));
  Output.WriteLine;

  for Enum in Enums do
    Enum.GenerateCode(Output);
  Output.WriteLine;
  Exit;

  for Message in Messages do
  begin
    Message.GenerateDeclaration(Output);
    Output.WriteLine;
  end;

  Output.WriteLine(sLineBreak + 'implementation' + sLineBreak);
  Output.WriteLine('uses strutils;');

  for Message in Messages do
  begin
    Output.WriteLine(sLineBreak + Format(' { T%s }', [Canonicalize(Message.Name)]) +
      sLineBreak);
    Message.GenerateImplementation(Output);
    Output.WriteLine;
  end;

  Output.WriteLine('end.');

  Output.Free;
  }
end;

{ TMap }

function TMap.GetDefaultValue: AnsiString;
begin
  Result:= 'nil';
end;

function TMap.GetFieldType: TType;
begin
  Result:= Format('map<%s, %s>', [FKeyType, FValueType]);

end;

function TMap.GetType: AnsiString;
begin
  WriteLn('raise ENotImplementedYet.Create(TMap.GetType)');
  Result:= inherited GetType;
end;

constructor TMap.Create(_Name: AnsiString; _FieldNumber: Integer; _KeyType,
  _ValueType: TType; _Options: TOptions);
begin
   inherited Create(_Name, '', false, _FieldNumber, _Options);

   FKeyType := _KeyType;
   FValueType := _ValueType;

end;

function TMap.ToXML: AnsiString;
begin
  Result := Format('<TMap Name = "%s" KeyType = "%s" ValueType = "%s" FieldNumber = "%d">%s</TMap>',
   [GetName, FKeyType, FValueType, GetFieldNumber, GetOptions.ToXML]);
end;

{ TOneOf }

procedure TOneOf.GenerateDeclaration(Output: TMyTextStream);
var
  i: Integer;

begin
  raise ENotImplementedYet.Create('OneOf');
  Output.WriteLine(Format('  T%s = Class(TObject)', [FName]));
  Output.WriteLine('  private');

  for i := 0 to OneOfFields.Count - 1 do
    Output.WriteLine(Format('    F%s: %s;', [OneOfFields[i].Name, OneOfFields[i].OneOfFieldType]));
  Output.WriteLine;
{  for i := 0 to Self.Count - 1 do
  begin
    EnumField := Self[i];
    Stream.WriteStr(Format('%s', [ifthen(i = 0, '(', ', ')]));
    Stream.WriteStr(Format('%s:%d', [EnumField.Name, EnumField.Value]));
  end;}

  Output.WriteLine('  public');

  for i := 0 to OneOfFields.Count - 1 do
    Output.WriteLine(Format('    property %s: %s read F%s write F%s;',
      [OneOfFields[i].Name, OneOfFields[i].OneOfFieldType, OneOfFields[i].Name, OneOfFields[i].Name]));
  Output.WriteLine;

  Output.WriteLine('  end;');
end;

constructor TOneOf.Create(_Name: AnsiString; _Fields: TOneOfFields);
begin
  inherited Create(_Name, '', false, -1, TOptions.Create);

  OneOfFields := _Fields;

end;

function TOneOf.ToXML: AnsiString;
var
  OOField: TOneOfField;

begin
  Result:= '';
  Result += Format('<OneOf Name = "%s">'#10'%s',
    [Name, Options.ToXML]);

//  for OOField in Self.OneOfFields do
//    Result += OOField.ToXML;
  Result += Format('</OneOf>'#10, []);

end;

{ TOneOfField }

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

constructor TEnumField.Create(_Name: AnsiString; _Options: TOptions;
  _Value: Integer);
begin
  inherited Create;

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

procedure TEnum.GenerateCode(Stream: TMyTextStream);
var
  EnumField: TEnumField;
  i: Integer;

begin
  Stream.WriteStr(Format('  T%s = ', [FName]));
  for i := 0 to Self.Count - 1 do
  begin
    EnumField := Self[i];
    Stream.WriteStr(Format('%s', [ifthen(i = 0, '(', ', ')]));
    Stream.WriteStr(Format('%s_%s = %d', [FName, EnumField.Name, EnumField.Value]));
  end;
  Stream.WriteLine(');');
end;

function CompareEnumFields(const F1, F2: TEnumField): Integer;
begin
  Result := F1.Value - F2.Value;
end;

procedure TEnum.PrepareForCodeGeneration;
begin
  Sort(@CompareEnumFields);
end;

constructor TEnum.Create(_Name: AnsiString; _Options: TOptions;
  _EnumFields: TEnumFields);
var
  EF: TEnumField;

begin
  inherited Create;

  FName := _Name;
  FOptions := TOptions.Create;

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
  Result := Format('<TEnum Name= "%s">', [FName]);
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

procedure TMessage.GenerateDeclaration(Output: TMyTextStream);
var
  i: Integer;
  Field: TMessageField;
  S: AnsiString;

begin
  MessageClassName := Format('T%s', [Canonicalize(Name)]);
  for i := 0 to Enums.Count - 1 do
  begin
    Enums[i].FName := Format('%s%s',
      [MessageClassName, Canonicalize(Enums[i].FName)]);
    Enums[i].GenerateCode(Output);
  end;

  if Options.Count <> 0 then
    raise Exception.Create('NIY: Options in Message');

  Output.WriteLine(Format('  T%s = Class(TBaseMessage)', [Canonicalize(FName)]));
  for Field in Fields do
    Field.GenerateDeclaration(MessageClassName, Output);

  Output.WriteLine('  protected ');
  Output.WriteLine('    procedure SaveToStream(Stream: TProtoStreamWriter); override;');
  Output.WriteLine('    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;');
  Output.WriteLine;
  Output.WriteLine('  public ');
  Output.WriteLine('    constructor Create;');
  if Fields.Count <> 0 then
  begin
    S := '    constructor Create(';
    for Field in Fields do
      S += Format('a%s: %s; ', [Canonicalize(Field.Name),
        Field.GetType]);
    S[Length(S) - 1] := ')';
    S[Length(S)] := ';';
    Output.WriteLine(S);

  end;

  Output.WriteLine('    destructor Destroy; override;');
  Output.WriteLine('    function ToString: AnsiString; override;');
  Output.WriteLine(' ');
  Output.WriteLine('  end;');

end;

procedure TMessage.GenerateImplementation(Output: TMyTextStream);

  procedure GenerateConstructors;
  var
    Field: TMessageField;
    CanName: AnsiString;
    CreateDeclaration: AnsiString;

  begin
    Output.WriteLine(Format('constructor %s.Create;', [MessageClassName]));
    Output.WriteLine('begin');
    Output.WriteLine(Format('  inherited Create;', []));
    Output.WriteLine;

    for Field in Fields do
    begin
      CanName := Canonicalize(Field.Name);
      if Field.DefaultValue <> '' then
        Output.WriteLine(Format('  F%s := %s;', [CanName, Field.DefaultValue]));
    end;

    Output.WriteLine('end;' + sLineBreak);

    if Fields.Count <> 0 then
    begin
      CreateDeclaration := 'Create(';
      for Field in Fields do
        CreateDeclaration += Format('a%s: %s; ', [Canonicalize(Field.Name),
          Field.GetType]);
      CreateDeclaration[Length(CreateDeclaration) - 1] := ')';
      CreateDeclaration[Length(CreateDeclaration)] := ';';
      Output.WriteLine(Format('constructor %s.%s',
        [MessageClassName, CreateDeclaration]));
      Output.WriteLine('begin');
      Output.WriteLine(Format('  inherited Create;', []));
      Output.WriteLine;
      for Field in Fields do
        Output.WriteLine(Format('  F%s := a%s; ', [Canonicalize(Field.Name),
            Canonicalize(Field.Name)]));

      Output.WriteLine(sLineBreak + 'end;');
    end;

  end;

  procedure GenerateDestructor;
  var
    Field: TMessageField;
    CanName: AnsiString;

  begin
    Output.WriteLine(Format('destructor %s.Destroy;', [MessageClassName]));
    Output.WriteLine('begin');
    for Field in FFields do
    begin
      CanName := Canonicalize(Field.Name);
      if not IsSimpleType(Field.FieldType) or Field.IsRepeated then
        Output.WriteLine(Format('  F%s.Free;', [CanName]));
    end;
    Output.WriteLine;
    Output.WriteLine('  inherited;');
    Output.WriteLine('end;');


  end;

  procedure GenerateToString;

  var
    Field: TMessageField;
    CanName: AnsiString;

  begin
    Output.WriteLine(Format('function %s.ToString: AnsiString;', [MessageClassName]));
    if HasRepeatedHasNonSimple then
    begin
      Output.WriteLine('var');
      Output.WriteLine('  BaseMessage: TBaseMessage;');
      Output.WriteLine;
    end;

    Output.WriteLine('begin');
    Output.WriteLine('  Result := '''';');
    Output.WriteLine;

    for Field in FFields do
      Field.GenerateToString(Output);
    Output.WriteLine;
    Output.WriteLine('end;');

  end;

  procedure GenerateSaveToStream;
  var
    Field: TMessageField;
    CanName: AnsiString;
    FieldType: AnsiString;

  begin
    Output.WriteLine(Format('procedure %s.SaveToStream(Stream: TProtoStreamWriter);', [MessageClassName]));
    if HasRepeatedHasNonSimple then
    begin
      Output.WriteLine('var');
      Output.WriteLine('  SizeNode: TLinkListNode;');
      Output.WriteLine('  BaseMessage: TBaseMessage;');
      Output.WriteLine;
    end;

    Output.WriteLine('begin');

    for Field in Fields do
    begin
      CanName := Canonicalize(Field.Name);
      FieldType :=  GetTypeName(Field.FieldType);

      if IsSimpleType(Field.FieldType) and not Field.IsRepeated then
      begin
        Output.WriteLine(Format('  Save%s(Stream, F%s, %d);',
          [FieldType, CanName, Field.FieldNumber]));
      end
      else if IsSimpleType(Field.FieldType) and Field.IsRepeated then
      begin
        CanName := Canonicalize(Field.Name);
        FieldType :=  GetTypeName(Field.FieldType);

        case GetTypeName(Field.FieldType) of
        'AnsiString', 'Single', 'Double', 'Int32', 'Int64', 'UInt32', 'UInt64', 'Boolean':
          Output.WriteLine(Format('  SaveRepeated%s(Stream, F%s, %d);',
            [FieldType, CanName, Field.FieldNumber]))
        else
           raise Exception.Create(Format('Type %s is not supported yet',
               [GetTypeName(Field.FieldType)]));
        end;
      end
      else if not IsSimpleType(Field.FieldType) and not Field.IsRepeated then
      begin
        Output.WriteLine(Format('  SaveMessage(Stream, F%s, %d);',
          [CanName, Field.FieldNumber]));
      end
      else if not IsSimpleType(Field.FieldType) and Field.IsRepeated then
      begin
        Output.WriteLine(Format('  if F%s <> nil then', [CanName]));
        Output.WriteLine(Format('    for BaseMessage in F%s do', [CanName]));
        Output.WriteLine(Format('      SaveMessage(Stream, BaseMessage, %d);',
                         [Field.FieldNumber]));
        Output.WriteLine;
      end;

      Output.WriteLine;
    end;

    Output.WriteLine('end;');
    Output.WriteLine;

  end;

  procedure GenerateLoadFromStream;
  var
    Field: TMessageField;
    CanName: AnsiString;
    FieldType: AnsiString;

  begin
    Output.WriteLine(Format('function %s.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;', [MessageClassName]));
    Output.WriteLine('var');
    Output.WriteLine('  StartPos, FieldNumber, WireType: Integer;'+ sLineBreak);
    if HasRepeatedHasNonSimple then
    begin
      Output.WriteLine('  BaseMessage: TBaseMessage;');
      Output.WriteLine;
    end;

    Output.WriteLine('begin');
    Output.WriteLine('  StartPos := Stream.Position;');
    Output.WriteLine('  while Stream.Position < StartPos + Len do');
    Output.WriteLine('  begin');
    Output.WriteLine('    Stream.ReadTag(FieldNumber, WireType);');

    Output.WriteLine;
    Output.WriteLine('    case FieldNumber of');

    for Field in Fields do
    begin
      CanName := Canonicalize(Field.Name);
      FieldType :=  GetTypeName(Field.FieldType);


      if IsSimpleType(Field.FieldType) and not Field.IsRepeated then
      begin
        Output.WriteLine(Format('    %d: F%s := Load%s(Stream);' + sLineBreak,
          [Field.FieldNumber, CanName, FieldType]));
      end
      else if IsSimpleType(Field.FieldType) and Field.IsRepeated then
      begin
        CanName := Canonicalize(Field.Name);
        FieldType :=  GetTypeName(Field.FieldType);

        case GetTypeName(Field.FieldType) of
        'AnsiString', 'Single', 'Double', 'Int32', 'Int64', 'UInt32', 'UInt64', 'Boolean':
        begin
          Output.WriteLine(Format('    %d:', [Field.FieldNumber]));
          Output.WriteLine       ('    begin' + sLineBreak);
          Output.WriteLine       ('      if WireType <> 2 then' + sLineBreak +
                                  '        Exit(False);');
          Output.WriteLine(Format('      LoadRepeated%s(Stream, GetOrCreateAll%s);',
               [FieldType, CanName]));
          Output.WriteLine       ('    end;' + sLineBreak);
        end
        else
           raise Exception.Create(Format('Type %s is not supported yet',
               [GetTypeName(Field.FieldType)]));
        end;
      end
      else if not IsSimpleType(Field.FieldType) and not Field.IsRepeated then
      begin
        Output.WriteLine(Format('    %d:', [Field.FieldNumber]));
        Output.WriteLine       ('    begin');
        Output.WriteLine       ('      if WireType <> 2 then' + sLineBreak +
                                '        Exit(False);');
        Output.WriteLine(Format('      F%s := %s.Create;', [CanName, FieldType]));
        Output.WriteLine(Format('      if not LoadMessage(Stream, F%s) then' + sLineBreak +
                                '        Exit(False);', [CanName]));
        Output.WriteLine       ('    end;' + sLineBreak);
      end
      else if not IsSimpleType(Field.FieldType) and Field.IsRepeated then
      begin
        Output.WriteLine(Format('    %d:', [Field.FieldNumber]));
        Output.WriteLine       ('    begin');
        Output.WriteLine       ('      if WireType <> 2 then' + sLineBreak +
                                '        Exit(False);');
        Output.WriteLine(Format('      GetOrCreateAll%s.Add(%s.Create);', [CanName, FieldType]));
        Output.WriteLine(Format('      if not LoadMessage(Stream, F%s.Last) then' + sLineBreak +
                                '        Exit(False);', [CanName]));
        Output.WriteLine       ('    end;' + sLineBreak);
        Output.WriteLine;
      end;

      Output.WriteLine;
    end;
    Output.WriteLine('    end;');
    Output.WriteLine('  end;' + sLineBreak);

    Output.WriteLine('  Result := StartPos + Len = Stream.Position;');
    Output.WriteLine('end;');

  end;


var
  Field: TMessageField;

begin
  for Field in Fields do
    Field.GenerateImplementation(MessageClassName, Output);
  GenerateConstructors;
  Output.WriteLine;
  GenerateDestructor;
  Output.WriteLine;
  GenerateToString;
  Output.WriteLine;
  GenerateSaveToStream;
  GenerateLoadFromStream;

  {
  Output.WriteLine;
  GenerateLoadFromStream;
  }
end;

function TMessage.HasRepeatedHasNonSimple: Boolean;
var
  Field: TMessageField;

begin
  Result := False;

  for Field in Fields do
    if Field.IsRepeated and not IsSimpleType(Field.FieldType) then
      Exit(True);

end;

constructor TMessage.Create(_Name: AnsiString;
      _Fields: TMessageFields;
      _Messages: specialize TObjectList<TMessage>;
      _Options:  TOptions;
      _Enums:  TEnums);
begin
  inherited Create;

  FName := _Name;
  FFields := _Fields;
  FMessages := _Messages;
  FOptions := _Options;
  FEnums := _Enums;
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

function TMessageField.ApplyPattern(MessageClassName: AnsiString;
  const Template: AnsiString): AnsiString;
begin
  Result := Template;
  Result := StringReplace(Result, '[[Field.Type]]', Self.FieldType, [rfReplaceAll]);
  Result := StringReplace(Result, '[[Field.Name]]', Self.Name, [rfReplaceAll]);
  Result := StringReplace(Result, '[[Field.Number]]', IntToStr(Self.FieldNumber), [rfReplaceAll]);
  Result := StringReplace(Result, '[[Field.DefaultValue]]', Self.DefaultValue, [rfReplaceAll]);
  Result := StringReplace(Result, '[[CanName]]', Canonicalize(Self.Name), [rfReplaceAll]);
  Result := StringReplace(Result, '[[FieldType]]', GetTypeName(Self.FieldType), [rfReplaceAll]);
  Result := StringReplace(Result, '[[FormatString]]', FormatString(FieldType), [rfReplaceAll]);
  Result := StringReplace(Result, '[[ClassName]]', MessageClassName, [rfReplaceAll]);

end;

{$i NonRepeatedSimpleFieldTemplate.inc}
{$i NonRepeatedNonSimpleFieldTemplate.inc}
{$i RepeatedNonSimpleFieldTemplate.inc}
{$i RepeatedSimpleFieldTemplate.inc}


procedure TMessageField.GenerateDeclaration(MessageClassName: AnsiString;
  Output: TMyTextStream);
begin
  if not IsRepeated and IsSimpleType(FieldType) then
    Output.WriteLine(ApplyPattern(MessageClassName, DeclareNonRepeatedSimpleFieldTemplate))
  else if not IsRepeated and not IsSimpleType(FieldType) then
    Output.WriteLine(ApplyPattern(MessageClassName, DeclareNonRepeatedNonSimpleFieldTemplate))
  else if IsRepeated and not IsSimpleType(FieldType) then
    Output.WriteLine(ApplyPattern(MessageClassName, DeclareRepeatedNonSimpleFieldTemplate))
  else
    Output.WriteLine(ApplyPattern(MessageClassName, DeclareRepeatedSimpleFieldTemplate));
end;

procedure TMessageField.GenerateImplementation(MessageClassName: AnsiString;
  Output: TMyTextStream);
begin
  if not IsRepeated then
  else if not IsSimpleType(FieldType) then
    Output.WriteLine(ApplyPattern(MessageClassName, ImplementRepeatedNonSimpleFieldTemplate))
  else if GetTypeName(FieldType) = 'Boolean' then
    Output.WriteLine(ApplyPattern(MessageClassName, ImplementRepeatedBooleanTemplate))
  else
    Output.WriteLine(ApplyPattern(MessageClassName, ImplementRepeatedSimpleFieldTemplate));
end;

function TMessageField.GetDefaultValue: AnsiString;
begin
  if IsRepeated or not IsSimpleType(FieldType) then
    Exit('');

  case GetTypeName(FieldType) of
    'Double',
    'Single': Exit('0.0');
    'Int16',
    'Int32',
    'Int64',
    'UInt16',
    'UInt32',
    'UInt64': Exit('0');
    'Boolean': Exit('False');
    'AnsiString': Exit('''''')
    else
       raise Exception.Create(Format('Type %s is not supported yet',
         [GetTypeName(FieldType)]));
  end;
end;

procedure TMessageField.GenerateToString(Output: TMyTextStream);
var
  CanName: AnsiString;

begin
  CanName := Canonicalize(Self.Name);

  if IsSimpleType(FieldType) and not IsRepeated then
  begin
    if GetTypeName(FieldType) = 'Boolean' then
    begin
      Output.WriteLine(Format(
                       '  if F%s then',
                       [CanName]));
      Output.WriteLine(Format(
                       '    Result += Format(''F%s: %%s'', [IfThen(F%s, ''True'', ''False'')]) + sLineBreak;',
                       [CanName, CanName]));
      Output.WriteLine;
    end
    else if DefaultValue <> '' then
      Output.WriteLine(
        ApplyPattern('', ImplementNonRepeatedSimpleFieldToStringTemplate))
    else
    begin
      Output.WriteLine(Format('  Result += Format(''%s: %%%s '', [F%s]) + sLineBreak;',
        [Self.Name, FormatString(FieldType), CanName]));
      Output.WriteLine;
    end;

  end
  else  if not IsSimpleType(FieldType) and not IsRepeated then
    Output.WriteLine(
      ApplyPattern('', ImplementNonRepeatedNonSimpleFieldToStringTemplate))
  else if IsSimpleType(FieldType) and IsRepeated then
    Output.WriteLine(
      ApplyPattern('', ImplementRepeatedSimpleFieldToStringTemplate))
  else if IsSimpleType(FieldType) and IsRepeated then
    Output.WriteLine(
      ApplyPattern('', ImplementRepeatedNonSimpleFieldToStringTemplate))
  else if not IsSimpleType(FieldType) and IsRepeated then
  begin
    Output.WriteLine(Format('    if F%s <> nil then', [CanName]));
    Output.WriteLine(       '    begin');
    Output.WriteLine(Format('      Result += ''%s  = '';', [Self.Name]));
    Output.WriteLine(Format('      for BaseMessage in F%s do',
                   [CanName]));
    Output.WriteLine(       '        Result += Format(''[%s]'', [BaseMessage.ToString]);');
    Output.WriteLine(       '      Result += sLineBreak;');
    Output.WriteLine(       '    end;');
  end
  else
  raise Exception.Create('Invalid Msg');
end;

function TMessageField.GetFieldNumber: Integer;
begin
  Result := FFieldNumber;
end;

function TMessageField.GetFieldType: TType;
begin
  Result := FFieldType;

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

function TMessageField.GetType: AnsiString;
begin
  if IsSimpleType(FieldType) and not IsRepeated then
    Result := GetTypeName(FieldType)
  else if not IsSimpleType(FieldType) and not IsRepeated then
      Result := Format('%s', [GetTypeName(FieldType)])
  else if IsSimpleType(FieldType) and IsRepeated then
    Result := Format('specialize TSimpleTypeList<%s>', [GetTypeName(FieldType)])
  else
    Result := Format('specialize TObjectList<%s>', [GetTypeName(FieldType)]);

end;

constructor TMessageField.Create(_Name: AnsiString; _FieldType: TType;
  _IsRepeated: Boolean; _FieldNumber: Integer; _Options: TOptions);
begin
  inherited Create;

  FName := _Name;
  FFieldType := _FieldType;
  FIsRepeated := _IsRepeated;
  FFieldNumber := _FieldNumber;
  FOptions := _Options;
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

constructor TProto.Create(_Filename, _Syntax: AnsiString; _Imports: TImports;
  _Packages: TPackages; _Options: TOptions; _Messages: TMessages; _Enums: TEnums
  );
begin
  inherited Create;

  Filename := _Filename;
  Syntax := _Syntax;
  Imports := _Imports;
  Packages := _Packages;
  Options := _Options;
  Messages := _Messages;
  Enums := _Enums;

end;

destructor TProto.Destroy;
begin
  Imports.Free;
  Packages.Free;
  Options.Free;
  Messages.Free;
  Enums.Free;

  inherited Destroy;
end;

procedure TProto.PrepareForCodeGeneration;
var
  Message: TMessage;
  Enum: TEnum;

begin
  AllClassesNames := TStringList.Create;
  AllEnumsNames := TStringList.Create;

  for Message in Messages do
    Message.PrepareForCodeGeneration(AllClassesNames, AllEnumsNames);

  for Enum in Enums do
    Enum.PrepareForCodeGeneration;
end;

function TProto.ToXML: AnsiString;
begin
  Result := Format('<TProto FileName = "%s" Path= "%s" >'#10'%s%s%s%s </TProto>'#10,
  [ExtractFileName(Filename),
   ExtractFileDir(Filename),
   Options.ToXML,
   Imports.ToXML,
   Packages.ToXML,
   Messages.ToXML
  ]);

end;


end.

