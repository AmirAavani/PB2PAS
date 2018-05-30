unit PBParserUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, StreamUnit;

type
  TTokenKind = (ttkStart, ttkDot, ttkOpenBrace, ttkCloseBrace, ttkOpenPar,
    ttkClosePar, ttkSemiColon, ttkEqualSign,
    ttkColon, ttkComma, ttkDoubleQuote, ttkSingleQuote,
    ttkMinus, ttkQuestionMark, ttkLessThan, ttkGreaterThan, ttkOpenBracket,
      ttkCloseBracket,
    ttkIdentifier, ttkComment, ttkNumber, ttkSpace, ttkSlash, ttkEndLine, ttkEOF);
  TCharKind = (tckStart, tckDot, tckOpenBrace, tckCloseBrace, tckOpenPar,
    tckClosePar, tckSemiColon, tckEqualSign,
      tckColon, tckComma, tckDoubleQuote, tckSingleQuote,
      tckMinus, tckQuestionMark, tckLessThan, tckGreaterThan, tckOpenBracket,
      tckCloseBracket,
      tckLetter, tckDigit,  tckUnderline, tckSpace, tckSlash,
      tckEndLine, tckEoF);
  { TToken }

  TToken = record
    Kind: TTokenKind;
    TokenString: AnsiString;
  end;


  TIntList = specialize TFPGList<Integer>;

  { TTokenizer }

  TTokenizer = class(TObject)
  private
    Current: Integer;
    FileSize: Integer;
    WholeFile: AnsiString;

  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function GetNextToken: TToken;

  end;

  { TOption }

  TOption = class(TObject)
  private
    FOptionName: AnsiString;
    FConstValue: AnsiString;

  public
    constructor Create(Tokenizer: TTokenizer);

    property OptionName: AnsiString read FOptionName;
    property ConstValue: AnsiString read FConstValue;
  end;


  { TSpecialList }

  generic TObjectList<TObject> = class(specialize TFPGList<TObject>)
  public
    destructor Destroy; override;

    function ToString: AnsiString; override;
  end;

  TType = AnsiString;

  { TEnumField }

  TEnumField = class(TObject)
  private
    FName: AnsiString;
    FOptions: specialize TObjectList<TOption>;
    FValue: Integer;
  public
    property Name: AnsiString read FName;
    property Value: Integer read FValue;
    property Options: specialize TObjectList<TOption> read FOptions;

    constructor Create(EnumName: AnsiString; Tokenizer: TTokenizer);
    destructor Destroy; override;
    function ToString: AnsiString; override;

  end;

  { TEnum }

  TEnum = class(specialize TObjectList<TEnumField>)
  private
    FName: AnsiString;

    procedure GenerateCode(Stream: TMyTextStream);
  public
    property Name: AnsiString read FName;

    constructor Create(Tokenizer: TTokenizer);
  end;


  { TOneOfField }

  TOneOfField = class(TObject)
  private
    FOneOfFieldType: AnsiString;
    FName: AnsiString;
    FOptions: specialize TObjectList<TOption>;
    FFeildNumber: Integer;
  public
    property OneOfFieldType: AnsiString read FOneOfFieldType;
    property Name: AnsiString read FName;
    property FieldNumber: Integer read FFeildNumber;
    property Options: specialize TObjectList<TOption> read FOptions;

    constructor Create(OneOfType: AnsiString; Tokenizer: TTokenizer);
    destructor Destroy; override;
    function ToString: AnsiString; override;

  end;

  { TOneOf }

  TOneOf = class(specialize TObjectList<TOneOfField>)
  private
    FName: AnsiString;
    FOptions: specialize TObjectList<TOption>;

    procedure GenerateDeclaration(Output: TMyTextStream);
  public
    property Name: AnsiString read FName;
    property Options: specialize TObjectList<TOption> read FOptions;

    constructor Create(Tokenizer: TTokenizer);
  end;

  { TMap }

  TMap = class(TObject)
  private
    FName: AnsiString;
    FKeyType: AnsiString;
    FValueType: AnsiString;
    FFieldNumber: Integer;

  public
    property Name: AnsiString read FName;
    property KeyType: AnsiString read FKeyType;
    property ValueType: AnsiString read FValueType;
    property FieldNumber: Integer read FFieldNumber;

    constructor Create(Tokenizer: TTokenizer);

  end;

  { TMessageField }

  TMessageField = class(TObject)
  private
    FFieldNumber: Integer;
    FIsRepeated: Boolean;
    FFieldType: TType;
    FName: AnsiString;
    FOptions: specialize TObjectList<TOption>;

    function ApplyPattern(MessageClassName: AnsiString; const Template: AnsiString): AnsiString;
    procedure GenerateDeclaration(MessageClassName: AnsiString; Output: TMyTextStream);
    procedure GenerateImplementation(MessageClassName: AnsiString; Output: TMyTextStream);
    function GetDefaultValue: AnsiString;
    procedure GenerateToString(Output: TMyTextStream);
    // Returns the translation of FieldType to FPC.
    function GetType: AnsiString;

  public
    property IsRepeated: Boolean read FIsRepeated;
    property FieldType: TType read FFieldType;
    property Name: AnsiString read FName;
    property FieldNumber: Integer read FFieldNumber;
    property Options: specialize TObjectList<TOption> read FOptions;
    property DefaultValue: AnsiString read GetDefaultValue;

    constructor Create(TokenString: AnsiString; Tokenizer: TTokenizer);
    destructor Destroy; override;

    function ToString: AnsiString; override;
  end;


  { TMessage }

  TMessage = class(TObject)
  private
    FFields: specialize TObjectList<TMessageField>;
    FMessages: specialize TObjectList<TMessage>;
    FOptions: specialize TObjectList<TOption>;
    FOneOfs: specialize TObjectList<TOneOf>;
    FMaps: specialize TObjectList<TMap>;
    FEnums: specialize TObjectList<TEnum>;
    FName: AnsiString;

    MessageClassName: AnsiString;

    procedure PrepareForCodeGeneration;
    procedure GenerateDeclaration(Output: TMyTextStream);
    procedure GenerateImplementation(Output: TMyTextStream);

    function HasRepeatedHasNonSimple: Boolean;
  public
    property Name: AnsiString read FName;
    property Fields: specialize TObjectList<TMessageField> read FFields;
    property Messages: specialize TObjectList<TMessage> read FMessages;
    property Options: specialize TObjectList<TOption> read FOptions;
    property Enums: specialize TObjectList<TEnum> read FEnums;
    property OneOfs: specialize TObjectList<TOneOf> read FOneOfs;

    constructor Create(Tokenizer: TTokenizer);
    destructor Destroy; override;

    function ToString: AnsiString; override;
  end;

  { TProto }

  TProto = class(TObject)
  private
    Imports: TStringList;
    Packages: TStringList;
    Options: specialize TObjectList<TOption>;
    Messages: specialize TObjectList<TMessage>;
    Enums: specialize TObjectList<TEnum>;

    procedure PrepareForCodeGeneration;
    class function Parse(InputStream: TStream): TProto;
    procedure GenerateCode(
        OutputUnitName: AnsiString; OutputStream: TStream); virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    class procedure GenerateCode(InputFilename: AnsiString);

    function ToString: AnsiString; override;

  end;

  { TProto3 }

  TProto3 = class(TProto)
    constructor Create(Tokenizer: TTokenizer);

    procedure GenerateCode(
        OutputUnitName: AnsiString; OutputStream: TStream); override;
  public

  end;

implementation
uses
  strutils;

type
  EExpectFailed = class(Exception)
  end;

function Expect(const Token: TToken; const T: AnsiString): Boolean;
begin
  Result := Token.TokenString = T;

  if not Result then
    raise EExpectFailed.Create(Format('Expected "%s" Visited "%s".',
      [Token.TokenString, T]));
end;

function Expect(const Token: TToken; Ts: array of TTokenKind): Boolean;
var
  Kind: TTokenKind;
begin
  Result := False;
  for Kind in Ts do
    Result := Result or (Token.Kind = Kind);

  if not Result then
    raise EExpectFailed.Create(Format('Expected "%d" Visited "%d".',
              [Ord(Token.Kind), Ord(Ts[0])]));
end;

function ParseType(const TokenString: AnsiString; Tokenizer: TTokenizer): AnsiString;
var
  Token: TToken;
  Pos: Integer;

begin
  Result := TokenString;
  Pos := Tokenizer.Current;
  Token := Tokenizer.GetNextToken;

  while Token.Kind = ttkDot do
  begin
    Result += Token.TokenString;

    Token := Tokenizer.GetNextToken;
    Expect(Token, [ttkIdentifier]);
    Result += Token.TokenString;

    Pos := Tokenizer.Current;
    Token := Tokenizer.GetNextToken;
  end;

  Tokenizer.Current := Pos;
end;

function ParseImport(Tokenizer: TTokenizer): AnsiString;
var
  CurrentToken: TToken;
  // import = "import" [ "weak" | "public" ] strLit ";"
begin
  Expect(Tokenizer.GetNextToken, [ttkDoubleQuote, ttkSingleQuote]);

  Result := '';
  CurrentToken := Tokenizer.GetNextToken;

  while not (CurrentToken.Kind in [ttkDoubleQuote, ttkSingleQuote]) do
  begin
    Result += CurrentToken.TokenString;
    CurrentToken := Tokenizer.GetNextToken;
  end;
  Expect(CurrentToken, [ttkDoubleQuote, ttkSingleQuote]);
  Expect(Tokenizer.GetNextToken, [ttkSemiColon]);

end;

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
  c: Char;
  LastCharWasUnderline: Boolean;

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

function GetTypeName(TypeName: AnsiString): AnsiString;
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
    else
      Result := 'T' + Canonicalize(TypeName);
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

function GenerateCodeForImports(Imports: TStringList; Output: TMyTextStream): AnsiString;
var
  i: Integer;

begin
  Result := 'uses ' + sLineBreak +
    '    classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit';
  for i := 0 to Imports.Count - 1 do
    Result += ', ' + GetUnitName(Imports[i]);

   Result += ';';
end;

function ParsePackage(Tokenizer: TTokenizer): AnsiString;
var
  Token: TToken;
  // package = "package" fullIdent ";"
begin
  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkIdentifier]);
  Result := Token.TokenString;

  Token := Tokenizer.GetNextToken;
  while Token.Kind in [ttkIdentifier, ttkDot] do
  begin
    Result += Token.TokenString;
    Token := Tokenizer.GetNextToken;
  end;
  Expect(Token, [ttkSemiColon]);

end;

function ParseSyntax(Tokenizer: TTokenizer): Integer;
// syntax = "syntax" "=" quote "proto3" quote ";"
begin
  Expect(Tokenizer.GetNextToken, [ttkEqualSign]);
  Expect(Tokenizer.GetNextToken, [ttkDoubleQuote]);
  Expect(Tokenizer.GetNextToken, 'proto3');
  Expect(Tokenizer.GetNextToken, [ttkDoubleQuote]);
  Expect(Tokenizer.GetNextToken, [ttkSemiColon]);
  Result := 3;

end;

{ TProto3 }

constructor TProto3.Create(Tokenizer: TTokenizer);
var
  Token: TToken;

begin
  inherited Create;

  Token := Tokenizer.GetNextToken;
  while Token.Kind <> ttkEOF do
  begin
    if Token.TokenString = 'import' then
      Self.Imports.Add(ParseImport(Tokenizer))
    else if Token.TokenString = 'package' then
      Self.Packages.Add(ParsePackage(Tokenizer))
    else if Token.TokenString = 'option' then
      Self.Options.Add(TOption.Create(Tokenizer))
    else if Token.TokenString = 'message' then
      Self.Messages.Add(TMessage.Create(Tokenizer))
    else if Token.TokenString = 'enum' then
      Self.Enums.Add(TEnum.Create(Tokenizer))
    else
      raise Exception.Create(Format('Invalid token: %s', [Token.TokenString]));
    Token := Tokenizer.GetNextToken;
  end;

end;

procedure TProto3.GenerateCode(OutputUnitName: AnsiString; OutputStream: TStream);
var
  Output: TMyTextStream;
  Enum: TEnum;
  Message: TMessage;

begin
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
end;

{ TMap }

constructor TMap.Create(Tokenizer: TTokenizer);
// mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
//keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
//          "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
var
  Token: TToken;

begin
  inherited Create;

  Expect(Tokenizer.GetNextToken, [ttkLessThan]);
  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkIdentifier]);
  FKeyType := Token.TokenString;

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkComma]);

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkIdentifier]);
  FValueType := ParseType(Token.TokenString, Tokenizer);

  Expect(Tokenizer.GetNextToken, [ttkGreaterThan]);

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkIdentifier]);
  FName := Token.TokenString;

  Expect(Tokenizer.GetNextToken, [ttkEqualSign]);

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkNumber]);
  FFieldNumber := StrToInt(Token.TokenString);

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkSemiColon]);
end;

{ TOneOf }

procedure TOneOf.GenerateDeclaration(Output: TMyTextStream);
var
  i: Integer;

begin
  raise Exception.Create('NIY: OneOf');
  Output.WriteLine(Format('  T%s = Class(TObject)', [FName]));
  Output.WriteLine('  private');

  for i := 0 to Self.Count - 1 do
    Output.WriteLine(Format('    F%s: %s;', [Self[i].Name, Self[i].OneOfFieldType]));
  Output.WriteLine;
{  for i := 0 to Self.Count - 1 do
  begin
    EnumField := Self[i];
    Stream.WriteStr(Format('%s', [ifthen(i = 0, '(', ', ')]));
    Stream.WriteStr(Format('%s:%d', [EnumField.Name, EnumField.Value]));
  end;}

  Output.WriteLine('  public');

  for i := 0 to Self.Count - 1 do
    Output.WriteLine(Format('    property %s: %s read F%s write F%s;',
      [Self[i].Name, Self[i].OneOfFieldType, Self[i].Name, Self[i].Name]));
  Output.WriteLine;

  Output.WriteLine('  end;');
end;

constructor TOneOf.Create(Tokenizer: TTokenizer);
// oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
var
  Token: TToken;
  OneOfType: AnsiString;

begin
  inherited Create;

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkIdentifier]);
  Fname := Token.TokenString;

  Expect(Tokenizer.GetNextToken, [ttkOpenBrace]);
  Token := Tokenizer.GetNextToken;

  while Token.Kind <> ttkCloseBrace do
  begin
    OneOfType := ParseType(Token.TokenString, Tokenizer);
    Self.Add(TOneOfField.Create(OneOfType, Tokenizer));

    Token := Tokenizer.GetNextToken;
  end;
end;

{ TOneOfField }

constructor TOneOfField.Create(OneOfType: AnsiString; Tokenizer: TTokenizer);
// Parses a limitted form
//  oneofField = type fieldName "=" fieldNumber ";"
// AND NOT
//  oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
var
  Token: TToken;

begin
  inherited Create;

  FOneOfFieldType := OneOfType;
  Token := Tokenizer.GetNextToken;

  Expect(Token, [ttkIdentifier]);
  FName:= Token.TokenString;
  Expect(Tokenizer.GetNextToken, [ttkEqualSign]);

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkNumber]);
  FFeildNumber := StrToInt(Token.TokenString);
  Expect(Tokenizer.GetNextToken, [ttkSemiColon]);

end;

destructor TOneOfField.Destroy;
begin
  inherited Destroy;
end;

function TOneOfField.ToString: AnsiString;
begin
  Result:=inherited ToString;
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

function TObjectList.ToString: AnsiString;
var
  Obj: TObject;

begin
  Result := '';
  for Obj in Self do
    Result += Obj.ToString;
end;

{ TEnumField }

constructor TEnumField.Create(EnumName: AnsiString; Tokenizer: TTokenizer);
var
  Token: TToken;
{
  This function just parses a simple form of enum:
  enumField = ident "=" int ";"

  And not the complete form:
  enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
  enumValueOption = optionName "=" constant
}
begin
  inherited Create;

  FName:= EnumName;
  Expect(Tokenizer.GetNextToken, [ttkEqualSign]);
  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkNumber]);
  FValue:= StrToInt(Token.TokenString);

  Expect(Tokenizer.GetNextToken, [ttkSemiColon]);
end;

destructor TEnumField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TEnumField.ToString: AnsiString;
begin
  Result:=Format('%s %s %d', [Name, Options.ToString, FValue]);

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

constructor TEnum.Create(Tokenizer: TTokenizer);
{
  enum = "enum" enumName enumBody
  enumBody = "{" { option | enumField | emptyStatement } "}"
}
var
  Token: TToken;

begin
  inherited Create;

  Token := Tokenizer.GetNextToken;
  Expect(Token, ttkIdentifier);
  FName := Token.TokenString;

  Expect(Tokenizer.GetNextToken, [ttkOpenBrace]);
  Token := Tokenizer.GetNextToken;
  while Token.Kind <> ttkCloseBrace do
  begin
    if Token.TokenString = 'option' then
      raise Exception.Create('NIY(Option in enum)!')
    else
    begin
      Self.Add(TEnumField.Create(Token.TokenString, Tokenizer));
    end;
    Token := Tokenizer.GetNextToken;
  end;
  Expect(Token, ttkCloseBrace);

end;

{ TMessage }

function CompareMessageFields(const F1, F2: TMessageField): Integer;
begin
  Result := F1.FieldNumber - F2.FieldNumber;
end;

procedure TMessage.PrepareForCodeGeneration;
begin
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

  if OneOfs.Count <> 0 then
    raise Exception.Create('NIY OneOfs');
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

constructor TMessage.Create(Tokenizer: TTokenizer);
var
  Token: TToken;

begin
  inherited Create;

  FFields := specialize TObjectList<TMessageField>.Create;
  FMessages := specialize TObjectList<TMessage>.Create;
  FOptions := specialize TObjectList<TOption>.Create;
  FOneOfs := specialize TObjectList<TOneOf>.Create;
  FMaps := specialize TObjectList<TMap>.Create;
  FEnums := specialize TObjectList<TEnum>.Create;

  Token := Tokenizer.GetNextToken;
  Expect(Token, ttkIdentifier);
  FName := Token.TokenString;
  Expect(Tokenizer.GetNextToken, ttkOpenBrace);
  Token := Tokenizer.GetNextToken;

  while Token.Kind <> ttkCloseBrace do
  begin
    if Token.TokenString = 'enum' then
      FEnums.Add(TEnum.Create(Tokenizer))
    else if Token.TokenString = 'message' then
      FMessages.Add(TMessage.Create(Tokenizer))
    else if Token.TokenString = 'option' then
      FOptions.Add(TOption.Create(Tokenizer))
    else if Token.TokenString = 'oneof' then
      FOneOfs.Add(TOneOf.Create(Tokenizer))
    else if Token.TokenString = 'map' then
      FMaps.Add(TMap.Create(Tokenizer))
    else if Token.TokenString = 'reserved' then
      raise Exception.Create('NIY reserved')
    else if Token.Kind = ttkSemiColon then
    else
      FFields.Add(TMessageField.Create(Token.TokenString, Tokenizer));
    Token := Tokenizer.GetNextToken;
  end;
  Expect(Token, ttkCloseBrace);

end;

destructor TMessage.Destroy;
begin
  FFields.Free;
  FMessages.Free;
  FOptions.Free;
  FOneOfs.Free;
  FMaps.Free;
  FEnums.Free;

  inherited Destroy;
end;

function TMessage.ToString: AnsiString;
begin
  Result:= Format('Name = %s Options: (%s) Fields: (%s) Messages: (%s)',
    [Name, Options.ToString, Fields.ToString, Messages.ToString]);
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

constructor TMessageField.Create(TokenString: AnsiString; Tokenizer: TTokenizer
  );
var
  Token: TToken;
  //  field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
begin
  inherited Create;

  FOptions := specialize TObjectList<TOption>.Create;
  FIsRepeated := TokenString = 'repeated';
  if TokenString = 'repeated' then
  begin
    Token := Tokenizer.GetNextToken;
    Expect(Token, ttkIdentifier);
    FFieldType := ParseType(Token.TokenString, Tokenizer);
  end
  else
    FFieldType := ParseType(TokenString, Tokenizer);

  Token := Tokenizer.GetNextToken;
  Expect(Token, ttkIdentifier);
  FName := Token.TokenString;
  Expect(Tokenizer.GetNextToken, ttkEqualSign);
  Token := Tokenizer.GetNextToken;
  Expect(Token, ttkNumber);
  FFieldNumber := StrToInt(Token.TokenString);
  Token := Tokenizer.GetNextToken;

  if Token.Kind <> ttkSemiColon then
  begin
    Expect(Token, ttkOpenBracket);
    Options.Add(TOption.Create(Tokenizer));
    Token := Tokenizer.GetNextToken;
  end;
  Expect(Token, ttkSemiColon);

end;

destructor TMessageField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TMessageField.ToString: AnsiString;
begin
  Result:= '';
  if IsRepeated then
    Result += 'repeated ';
  Result += Format('%s %s = %d (%s)',
    [FieldType, Name, FFieldNumber, Options.ToString]);

end;

{ TOption }

constructor TOption.Create(Tokenizer: TTokenizer);
var
  Token: TToken;
  // This function just support optionName = 'default';
  // option = "option" optionName  "=" constant ";"
  // optionName = ( ident | "(" fullIdent ")" ) { "." ident }
  // constant ::= ident | intLit | floatLit | strLit | boolLit
begin
  inherited Create;

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkIdentifier]);
  FOptionName:= Token.TokenString;
  Expect(Token, 'default');

  Token := Tokenizer.GetNextToken;
  while Token.Kind in [ttkDot, ttkIdentifier] do
  begin
    FOptionName += Token.TokenString;
    Token := Tokenizer.GetNextToken;
  end;
  Expect(Token, [ttkEqualSign]);

  Token := Tokenizer.GetNextToken;
  if Token.Kind = ttkNumber then
    FConstValue := Token.TokenString// ParseNumber
  else if Token.Kind = ttkSingleQuote then
  begin
    Token := Tokenizer.GetNextToken;
    while Token.Kind <> ttkSingleQuote do
    begin
      FConstValue += Token.TokenString;
      Token := Tokenizer.GetNextToken;
    end;
    Expect(Token, [ttkSingleQuote]);
  end;

  Token := Tokenizer.GetNextToken;
  Expect(Token, [ttkCloseBracket]);
end;

{ TTokenizer }

constructor TTokenizer.Create(Stream: TStream);
begin
  inherited Create;

  SetLength(WholeFile, Stream.Size);
  Stream.ReadBuffer(WholeFile[1], Stream.Size);
  FileSize := Stream.Size;
  Current := 1;

end;

destructor TTokenizer.Destroy;
begin

  inherited Destroy;
end;

type
  {TCharKind = (tckStart, tckDot, tckSpace, tckOpenBrace,
    tckCloseBrace, tckOpenPar, tckClosePar, tckSlash, tckSemiColon, tckEqualSign,
    tckColon, tckComma, tckDoubleQuote, tckSingleQuote,
    tckMinus, tckQuestionMark, tckLessThan, tckGreaterThan, tckOpenBracket,
    tckCloseBracket,
    tckLetter, tckDigit,  tckUnderline,
    tckEndLine, tckEoF);
 }
  TChar = record
    Kind: TCharKind;
    Ch: Char;
  end;

function TTokenizer.GetNextToken: Ttoken;

  function GetNextChar: TChar;
  begin
    if Length(WholeFile) < Current then
    begin
      Result.Kind := tckEoF;
      Exit;
    end;
    Result.Ch := WholeFile[Current];
    Inc(Current);

    case Result.Ch of
    '0'..'9': Result.Kind := tckDigit;
    'a'..'z': Result.Kind := tckLetter;
    'A'..'Z': Result.Kind := tckLetter;
    ' ': Result.Kind := tckSpace;
     #10, #13: Result.Kind := tckEndLine;
    '(': Result.Kind := tckOpenPar;
    ')': Result.Kind := tckClosePar;
    '{': Result.Kind := tckOpenBrace;
    '}': Result.Kind := tckCloseBrace;
    '=': Result.Kind := tckEqualSign;
    ':': Result.Kind := tckColon;
    ',': Result.Kind := tckComma;
    '"': Result.Kind := tckDoubleQuote;
    ';': Result.Kind := tckSemiColon;
    '.': Result.Kind := tckDot;
    '_': Result.Kind := tckUnderline;
    '-': Result.Kind := tckMinus;
    '/': Result.Kind := tckSlash;
    '?': Result.Kind := tckQuestionMark;
    Chr(39): Result.Kind := tckSingleQuote;
    '<': Result.Kind := tckLessThan;
    '>': Result.Kind := tckGreaterThan;
    '[': Result.Kind := tckOpenBracket;
    ']': Result.Kind := tckCloseBracket;
    else
      WriteLn(Result.ch + ' ' + IntToStr(Ord(Result.Ch)));
      raise Exception.Create(Result.ch + ' ' + IntToStr(Ord(Result.Ch)));
    end;

  end;

var
  State: Integer;
  Ch: Char;
  CurrentChar: TChar;

begin
  Result.TokenString := '';
  Result.Kind:= ttkStart;
  CurrentChar := GetNextChar;

  case CurrentChar.Kind of
    tckLetter:
    begin
      while CurrentChar.Kind in [tckLetter, tckDigit, tckUnderline] do
      begin
        Result.TokenString += CurrentChar.Ch;
        CurrentChar := GetNextChar;
      end;
      Dec(Current);
      Result.Kind:= ttkIdentifier;
    end;
    tckDigit:
    begin
      while CurrentChar.Kind = tckDigit do
      begin
        Result.TokenString += CurrentChar.Ch;
        CurrentChar := GetNextChar;
      end;
      Dec(Current);
      Result.Kind:= ttkNumber;
    end;
    tckDot, tckOpenBrace,
      tckCloseBrace, tckOpenPar, tckClosePar, tckSemiColon, tckEqualSign,
      tckColon, tckComma, tckDoubleQuote, tckSingleQuote,
      tckMinus, tckQuestionMark, tckLessThan, tckGreaterThan, tckOpenBracket,
      tckCloseBracket:
    begin
      Result.TokenString += CurrentChar.Ch;
      Result.Kind := TTokenKind(Ord(CurrentChar.Kind));
    end;
    tckSpace:
    begin
      while CurrentChar.Kind in [tckSpace] do
      begin
        Result.TokenString += CurrentChar.Ch;
        CurrentChar := GetNextChar;
      end;
      Dec(Current);
      Result.Kind:= ttkSpace;
      Result := Self.GetNextToken;
    end;
    tckEndLine:
    begin
      Result.TokenString += CurrentChar.Ch;
      Result.Kind := ttkEndLine;
      CurrentChar := GetNextChar;
      while CurrentChar.Kind = tckEndLine do
      begin
        Result.TokenString += CurrentChar.Ch;
        CurrentChar := GetNextChar;
      end;
      Dec(Current);
      if CurrentChar.Kind = tckEoF then
      begin
        Result.Kind:= ttkEOF;
        Exit;
      end;
      Result := Self.GetNextToken;
    end;
    tckEoF:
      Result.Kind := ttkEOF;
    tckSlash:
    begin
      Result.TokenString += CurrentChar.Ch;
      Result.Kind := ttkSlash;
      if GetNextChar.Kind <> tckSlash then
      begin
        Dec(Current);
        Exit;
      end;
      Result.TokenString += CurrentChar.Ch;
      Result.Kind := ttkComment;
      CurrentChar := GetNextChar;
      while not (CurrentChar.Kind in [tckEndLine, tckEoF]) do
      begin
        Result.TokenString += CurrentChar.Ch;
        CurrentChar := GetNextChar;
      end;
      Result := Self.GetNextToken;
    end
    else
    begin
      WriteLn(CurrentChar.ch + ' Ch:' + IntToStr(Ord(CurrentChar.Ch)) +
        ' Kind:' + IntToStr(Ord(CurrentChar.Kind)) +
        ' Result:' + Result.TokenString);
      raise Exception.Create(CurrentChar.ch + ' ' + IntToStr(Ord(CurrentChar.Ch)) +
        ' Result:' + Result.TokenString);
    end;
  end;
end;

{ TProto }

constructor TProto.Create;
begin
  inherited;

  Imports := TStringList.Create;
  Packages := TStringList.Create;
  Options := specialize TObjectList<TOption>.Create;
  Messages := specialize TObjectList<TMessage>.Create;
  Enums := specialize TObjectList<TEnum>.Create;

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

class procedure TProto.GenerateCode(InputFilename: AnsiString);
var
  Proto: TProto;
  InputStream: TFileStream;
  OutputStream: TFileStream;
  FilePath: AnsiString;
  OutputUnitName: AnsiString;

begin
  InputStream := TFileStream.Create(InputFilename, fmOpenRead);
  Proto := TProto.Parse(InputStream);
  InputStream.Free;

  OutputUnitName := GetUnitName(InputFilename);
  OutputStream := TFileStream.Create(ExtractFileDir(InputFilename) + '/' +
    OutputUnitName + '.pp', fmCreate);

  Proto.PrepareForCodeGeneration;
  Proto.GenerateCode(OutputUnitName, OutputStream);

  OutputStream.Free;
  Proto.Free;

end;

procedure TProto.PrepareForCodeGeneration;
var
  Message: TMessage;

begin
  for Message in Messages do
    Message.PrepareForCodeGeneration;

end;

class function TProto.Parse(InputStream: TStream): TProto;
var
  Tokenizer: TTokenizer;
  Token: TToken;

begin
  Tokenizer := TTokenizer.Create(InputStream);

  Token := Tokenizer.GetNextToken;
  Expect(Token, 'syntax');
  if ParseSyntax(Tokenizer) = 3 then
    Result := TProto3.Create(Tokenizer)
  else
    raise Exception.Create('This library just works for Protobuf Version 3');

  Tokenizer.Free;
end;

function TProto.ToString: AnsiString;
var
  Message: TMessage;
  StrList: TStringList;

begin
  StrList := TStringList.Create;
  StrList.Add('Imports:' + Imports.Text);
  StrList.Add('Packages:' + Packages.Text);
  StrList.Add('Options:' + Options.ToString);

  for Message in Messages do
    StrList.Add(Message.ToString);

  Result := StrList.Text;
  StrList.Free;

end;

end.

