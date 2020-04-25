unit PBParserUnit;
{$modeswitch advancedrecords}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, StreamUnit, gvector;

procedure GenerateCode(InputFile: AnsiString);

implementation

uses
  strutils;

type
  TTokenKind = (ttkStart, ttkDot, ttkOpenBrace, ttkCloseBrace, ttkOpenPar,
    ttkClosePar, ttkSemiColon, ttkEqualSign, ttkStar,
    ttkColon, ttkComma, ttkDoubleQuote, ttkSingleQuote,
    ttkMinus, ttkPlus, ttkQuestionMark, ttkLessThan, ttkGreaterThan, ttkOpenBracket,
      ttkCloseBracket,
    ttkIdentifier, ttkComment, ttkNumber, ttkSpace, ttkSlash, ttkEndLine, ttkEOF);
  TCharKind = (tckStart, tckDot, tckOpenBrace, tckCloseBrace, tckOpenPar,
    tckClosePar, tckSemiColon, tckEqualSign, tckStar,
      tckColon, tckComma, tckDoubleQuote, tckSingleQuote,
      tckMinus, tckPlus, tckQuestionMark, tckLessThan, tckGreaterThan, tckOpenBracket,
      tckCloseBracket,
      tckLetter, tckDigit,  tckUnderline, tckSpace, tckSlash, tckBackSlash, tckNumberSign,
      tckExclamationMark,
      tckEndLine, tckEoF);
  { TToken }
  TToken = record
    Kind: TTokenKind;
    TokenString: AnsiString;
         // class operator +(a,b : TToken) : TToken;
  end;

type
  ENotImplementedYet = class(Exception);

  { EInvalidCharacter }

  EInvalidCharacter = class(Exception)
  public
    constructor Create(Ch: Char; Code: Integer);

  end;

  { EInvalidToken }

  EInvalidToken = class(Exception)
  public
    constructor Create(TokenStr: AnsiString; TokenKind: TTokenKind);

  end;

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

    function ToString(Indent: AnsiString): AnsiString;
  end;


  { TObjectList }

  generic TObjectList<TObject> = class(specialize TFPGList<TObject>)
  public
    destructor Destroy; override;

    function ToString(Indent: AnsiString): AnsiString;
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
    property Options: specialize TObjectList<TOption> read GetOptions;
    property DefaultValue: AnsiString read GetDefaultValue;

    constructor Create;
    destructor Destroy; override;

    function ToString(Indent: AnsiString): AnsiString; virtual;
  end;


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

    constructor Create;
    destructor Destroy; override;
    function ToString(Indent: AnsiString): AnsiString;

  end;

  { TEnum }

  TEnum = class(specialize TObjectList<TEnumField>)
  private
    FName: AnsiString;
    FOptions: TOptions;
    procedure GenerateCode(Stream: TMyTextStream);

  public
    property Name: AnsiString read FName;

    constructor Create;
    destructor Destroy; override;

    function ToString(Indent: AnsiString): AnsiString;
  end;


  { TOneOfField }

  TOneOfField = class(TObject)
  private
    FOneOfFieldType: AnsiString;
    FName: AnsiString;
    FOptions: specialize TObjectList<TOption>;
    FFieldNumber: Integer;
  public
    property OneOfFieldType: AnsiString read FOneOfFieldType;
    property Name: AnsiString read FName;
    property FieldNumber: Integer read FFieldNumber;
    property Options: specialize TObjectList<TOption> read FOptions;

    constructor Create;//(OneOfType: AnsiString; Tokenizer: TTokenizer);
    destructor Destroy; override;
    function ToString(Indent: AnsiString): AnsiString;

  end;

  TOneOfFields = specialize TObjectList<TOneOfField>;
  { TOneOf }

  TOneOf = class(TMessageField)
  private
    OneOfFields: TOneOfFields;

    procedure GenerateDeclaration(Output: TMyTextStream);
  public

    constructor Create;
  end;

  { TMap }

  TMap = class(TMessageField)
  private
    FKeyType: AnsiString;
    FValueType: AnsiString;

    function GetDefaultValue: AnsiString; override;
    function GetFieldType: TType; override;
    // Returns the translation of FieldType to FPC.
    function GetType: AnsiString; override;

  public
    property KeyType: AnsiString read FKeyType;
    property ValueType: AnsiString read FValueType;

    constructor Create;
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

    constructor Create;
    destructor Destroy; override;

    function ToString(Indent: AnsiString): AnsiString;
  end;


  TProtoParser = class;
  { TProto }

  TProto = class(TObject)
  private
    Syntax: AnsiString;
    Imports: TStringList;
    Packages: TStringList;
    Options: specialize TObjectList<TOption>;
    Messages: specialize TObjectList<TMessage>;
    Enums: specialize TObjectList<TEnum>;

    procedure PrepareForCodeGeneration;
    class function GetParser(InputStream: TStream): TProtoParser;
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
    constructor Create;

    procedure GenerateCode(
        OutputUnitName: AnsiString; OutputStream: TStream); override;
  public

  end;

  TStrLit = AnsiString;
  TConstant = AnsiString;

  { TTokenizer }

  TTokenizer = class(TObject)
  private
    Current: Integer;
    Currents: TIntList;
    FileSize: Integer;
    WholeFile: AnsiString;

    function ExpectAll(const TokenStrs: array of AnsiString): Boolean;
    function Expect(const TokenStr: AnsiString): Boolean;
    function ExpectAll(Ts: array of TTokenKind): Boolean;
    function ExpectOne(Ts: array of TTokenKind): Boolean;
    function Expect(const TokenKind: TTokenKind): Boolean;

    function NextTokenIsIn(Ts: array of TTokenKind): Boolean;
    function NextTokenIsIn(Ts: array of AnsiString): Boolean;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function GetNextToken: TToken;
    procedure Rewind(Count: Integer = 1);

  end;

  { TTokenArray }

  TTokenArray = class(specialize TVector<TToken>)
  public
    procedure AddToken(aToken: TToken);
    destructor Destroy; override;

    function ToString: AnsiString;

  end;

  { TProtoParser }

  TProtoParser = class(TObject)
  protected
    FTokenizer: TTokenizer;

    function ParseStrLit: TStrLit;
    function ParseIdent: TIdentifier;
    function ParseFieldNumber: Integer;
    function ParseFullIdent: TFullIdentifier;
    function ParseType: TType;

    function CollectUntil(EndTokenKind: TTokenKind): TTokenArray;

    property Tokenizer: TTokenizer read FTokenizer;
  public
    constructor Create(_Tokenizer: TTokenizer);
    destructor Destroy; override;

    function ParseProto: TProto; virtual; abstract;

  end;

  { TProto3Parser }

  TProto3Parser = class(TProtoParser)
  private
    function ParseImport: AnsiString;
    function ParseSyntax: Boolean;
    function ParseMessage: TMessage;
    function ParseEnum: TEnum;
    function ParseEnumField: TEnumField;
    function ParsePackage: AnsiString;
    function ParseOption(EndingTokenTypes: array of TTokenKind): TOption;
    function MaybeParseOptions(OpenTokenType: TTokenKind;
      EndTokenTypes: array of TTokenKind): TOptions;
    function ParseOneOf: TOneOf;
    function ParseMap: TMap;
    function ParseMessageField: TMessageField;
    function ParseConstant: TConstant;

  public
    constructor Create(_Tokenizer: TTokenizer);
    destructor Destroy; override;

    function ParseProto: TProto; override;

  end;

  EExpectFailed = class(Exception)
  end;

{ TTokenArray }

procedure TTokenArray.AddToken(aToken: TToken);
begin
  Self.PushBack(aToken);
end;

destructor TTokenArray.Destroy;
begin
  inherited Destroy;
end;

function TTokenArray.ToString: AnsiString;
begin

end;

function TProtoParser.ParseType: TType;
 // type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
 //     | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
 //     | "bool" | "string" | "bytes" | messageType | enumType

var
  Token: TToken;

begin
  Token := Tokenizer.GetNextToken;
  Result := Token.TokenString;
  Token := Tokenizer.GetNextToken;

  while Token.Kind = ttkDot do
  begin
    Result += Token.TokenString;

    Token := Tokenizer.GetNextToken;
    if Token.Kind <> ttkIdentifier then
      raise EInvalidToken.Create(Token.TokenString, ttkIdentifier);
    Result += Token.TokenString;

    Token := Tokenizer.GetNextToken;
  end;

    Tokenizer.Rewind();
end;

function TProtoParser.CollectUntil(EndTokenKind: TTokenKind): TTokenArray;
var
  CurToken: TToken;

begin
  CurToken := Tokenizer.GetNextToken;

  Result := TTokenArray.Create;
  while CurToken.Kind <> EndTokenKind do
  begin
    Result.AddToken(CurToken);

    CurToken := Tokenizer.GetNextToken;
  end;
end;

{ EInvalidToken }

constructor EInvalidToken.Create(TokenStr: AnsiString; TokenKind: TTokenKind);
begin
  inherited Create(Format('Unexpected Token Visited [%s, %d]', [TokenStr, Ord(TokenKind)]))

end;

function TProto3Parser.ParseImport: AnsiString;
// import = "import" [ "weak" | "public" ] strLit ";"
var
  CurrentToken, Token: TToken;
  StartTag: TTokenKind;
  Tokens: TTokenArray;

begin
  if Tokenizer.NextTokenIsIn(['weak', 'public']) then
    Tokenizer.GetNextToken;
  CurrentToken := Tokenizer.GetNextToken;
  StartTag:= CurrentToken.Kind;
  if not (StartTag in [ttkDoubleQuote, ttkSingleQuote]) then
    raise EInvalidToken.Create(CurrentToken.TokenString, StartTag);
  Result := Tokenizer.GetNextToken.TokenString;
  Tokens := CollectUntil(StartTag);
  for Token in Tokens do
  begin
    if Token.Kind = StartTag then
      Break;
    Result += Token.TokenString;

  end;
  Tokens.Free;
  Tokenizer.ExpectAll([ttkSemiColon]);

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

procedure GenerateCode(InputFile: AnsiString);
begin
  TProto.GenerateCode(InputFile);
end;

{ TProtoParser }

function TProtoParser.ParseStrLit: TStrLit;
var
  Token: TToken;

begin
  Result := '';
  if Tokenizer.NextTokenIsIn([ttkSingleQuote]) then
  begin
    Tokenizer.GetNextToken;
    while Tokenizer.NextTokenIsIn([ttkSingleQuote]) do
      Result += Tokenizer.GetNextToken.TokenString;
    Tokenizer.Expect(ttkSingleQuote);
  end
  else if Tokenizer.NextTokenIsIn([ttkDoubleQuote]) then
  begin
    Tokenizer.GetNextToken;
    while Tokenizer.NextTokenIsIn([ttkDoubleQuote]) do
      Result += Tokenizer.GetNextToken.TokenString;
    Tokenizer.Expect(ttkDoubleQuote);
  end
  else
  begin
    Token := Tokenizer.GetNextToken;
    raise EInvalidToken.Create(Token.TokenString, token.Kind);
  end;

end;

function TProtoParser.ParseIdent: TIdentifier;
// ident = letter { letter | decimalDigit | "_" }
var
  Token: TToken;

begin
  Token := Tokenizer.GetNextToken;
  if (Token.TokenString <> '') and (Token.TokenString[1] in ['a'..'z', 'A'..'Z']) then
    Exit(Token.TokenString);
  raise EInvalidToken.Create(Token.TokenString, Token.Kind);
end;

function ParseDecimalLit(TokenStr: AnsiString): Integer;
// decimalLit = ( "1" … "9" ) { decimalDigit }
begin
  Result := StrToInt(TokenStr);
end;

function ParseOctalLit(TokenStr: AnsiString): Integer;
var
  c: Char;
begin
  Result := 0;
  for c in Copy(TokenStr, 2, Length(TokenStr)) do
  begin
    if not (c in ['0'..'7']) then
      raise EInvalidToken.Create(TokenStr, ttkNumber);
    Result *= 8;
    Result += Ord(c) - 48;
  end;

end;

function ParseHexLit(TokenStr: AnsiString): Integer;
var
  c: Char;

begin
  Result := 0;
  for c in Copy(TokenStr, 3, Length(TokenStr)) do
  begin
    Result *= 16;
    if c in ['0'..'9'] then
      Result += Ord(c) - 48
    else if UpCase(c) in ['A'..'F'] then
      Result += Ord(UpCase(c)) - 55
    else
      raise EInvalidToken.Create(TokenStr, ttkNumber);
  end;
end;

function TProtoParser.ParseFieldNumber: Integer;
// fieldNumber = intLit;
// intLit     = decimalLit | octalLit | hexLit
// octalLit   = "0" { octalDigit }
// hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
var
  Token: TToken;
  c: Char;

begin
  Token := Tokenizer.GetNextToken;

  if Token.TokenString[1] in ['1'..'9'] then
    Result := ParseDecimalLit(Token.TokenString)
  else if (2 <= Length(Token.TokenString)) and (Token.TokenString[1] = '0') and (UpCase(Token.TokenString[2]) <> Upcase('x')) then
    Result := ParseOctalLit(Token.TokenString)
  else if (2 <= Length(Token.TokenString)) and (Token.TokenString[1] = '0') and (UpCase(Token.TokenString[2]) = Upcase('x')) then
    Result := ParseHexLit(Token.TokenString)
  else
    raise EInvalidToken.Create(Token.TokenString, ttkNumber);

end;

function TProtoParser.ParseFullIdent: TFullIdentifier;
// fullIdent = ident { "." ident }

begin
  Result := ParseIdent;

  while Tokenizer.NextTokenIsIn([ttkDot]) do
  begin
    Tokenizer.GetNextToken;
    Result += '.';
    Result += ParseIdent;
  end;

end;

constructor TProtoParser.Create(_Tokenizer: TTokenizer);
begin
  inherited Create;

  FTokenizer := _Tokenizer;
end;

destructor TProtoParser.Destroy;
begin
  FTokenizer.Free;

  inherited Destroy;
end;

{ TProto3Parser }

function TProto3Parser.ParseSyntax: Boolean;
// syntax = "syntax" "=" quote "proto3" quote ";"
begin
  Tokenizer.ExpectAll(['syntax']);
  Tokenizer.ExpectAll([ttkEqualSign, ttkDoubleQuote]);
  Tokenizer.ExpectAll(['proto3']);
  Tokenizer.ExpectAll([ttkDoubleQuote, ttkSemiColon]);
  Result := True;

end;

function TProto3Parser.ParseMessage: TMessage;
var
  Token: TToken;

{
  message = "message" messageName messageBody
  messageName = ident
  messageBody = "{" { field | enum | message | option | oneof | mapField |
  reserved | emptyStatement } "}"
}
begin
  Result := TMessage.Create;
  Result.FName := ParseIdent;;
  Tokenizer.Expect(ttkOpenBrace);

  Token := Tokenizer.GetNextToken;

  while Token.Kind <> ttkCloseBrace do
  begin
    if Token.TokenString = 'enum' then
      Result.Enums.Add(ParseEnum)
    else if Token.TokenString = 'message' then
      Result.Messages.Add(Self.ParseMessage)
    else if Token.TokenString = 'option' then
      Result.Options.Add(ParseOption(ttkSemiColon))
    else if Token.TokenString = 'oneof' then
      Result.Fields.Add(ParseOneOf)
    else if Token.TokenString = 'map' then
      Result.Fields.Add(ParseMap)
    else if Token.TokenString = 'reserved' then
      raise Exception.Create('NIY reserved')
    else if Token.Kind = ttkSemiColon then
    else
    begin
      Tokenizer.Rewind;
      Result.Fields.Add(ParseMessageField);
    end;

    Token := Tokenizer.GetNextToken;
  end;

  // WriteLn(Result.ToString);
end;

function TProto3Parser.ParseEnum: TEnum;
var
  Token: TToken;
{
  enum = "enum" enumName enumBody
  enumBody = "{" { option | enumField | emptyStatement } "}"
  enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
  enumValueOption = optionName "=" constant

  enumName = Ident
}
begin
  Result := TEnum.Create;

  Token := Tokenizer.GetNextToken;
  Result.FName := Token.TokenString;

  Tokenizer.Expect(ttkOpenBrace);
  Token := Tokenizer.GetNextToken;
  while Token.Kind <> ttkCloseBrace do
  begin
    if Token.TokenString = 'option' then
      Result.FOptions.Add(ParseOption(ttkSemiColon))
    else if Token.Kind = ttkSemiColon then
    else
    begin
      Tokenizer.Rewind;
      Result.Add(ParseEnumField);
    end;

    Token := Tokenizer.GetNextToken;
  end;

end;

function TProto3Parser.ParseEnumField: TEnumField;
var
  Token: TToken;
(*
  enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
  enumValueOption = optionName "=" constant
*)
begin
  Result := TEnumField.Create;

  Token := Tokenizer.GetNextToken;
  Result.FName:= Token.TokenString;
  Tokenizer.Expect(ttkEqualSign);
  Token := Tokenizer.GetNextToken;
  Result.FValue:= StrToInt(Token.TokenString);

  Result.FOptions := MaybeParseOptions(ttkOpenBracket, ttkCloseBracket);

  Tokenizer.Expect(ttkSemiColon);

end;

function TProto3Parser.ParsePackage: AnsiString;
  // package = "package" fullIdent ";"

begin
  Result := ParseFullIdent;
  Tokenizer.Expect(ttkSemiColon);

end;

function IsTokenIn(AToken: TToken; TokenTypes: array of TTokenKind): Boolean;
var
  t: TTokenKind;

begin
  Result := False;

  for t in TokenTypes do
    if t = AToken.Kind then
      Exit(True);

end;


function TProto3Parser.ParseOption(EndingTokenTypes: array of TTokenKind): TOption;
  function ParseOptionName: TOptionName;
  var
    Token: TToken;

    // optionName = ( ident | "(" fullIdent ")" ) { "." ident }
  begin
    if Tokenizer.NextTokenIsIn([ttkOpenPar]) then
    begin
      Tokenizer.Expect(ttkOpenPar);
      Result := '(' + ParseFullIdent + ')';
      Tokenizer.Expect(ttkClosePar);

    end
    else
      Result := ParseIdent;

    while Tokenizer.NextTokenIsIn([ttkDot]) do
      Result += '.' + ParseIdent;
  end;

var
  OptionName: TOptionName;
  Value: TConstValue;
  Token: TToken;
//  option = "option" optionName  "=" constant ";"

begin
  OptionName := ParseOptionName;
  Tokenizer.Expect(ttkEqualSign);
  Value := '';
  Token := Tokenizer.GetNextToken;
  while not IsTokenIn(Token, EndingTokenTypes) do
  begin
    Value += Token.TokenString;
    Token := Tokenizer.GetNextToken;
  end;

  Result := TOption.Create(OptionName, Value);
  Tokenizer.ExpectOne(EndingTokenTypes);

end;
function TProto3Parser.MaybeParseOptions(OpenTokenType: TTokenKind;
      EndTokenTypes: array of TTokenKind): TOptions;
var
  Token: TToken;

begin
  Result := TOptions.Create;
  Token := Tokenizer.GetNextToken;

  if Token.Kind  <> OpenTokenType then
  begin
    Tokenizer.Rewind;
    Exit;
  end;

  while not IsTokenIn(Token, EndTokenTypes) do
  begin
    Result.Add(ParseOption(EndTokenTypes));
    Token := Tokenizer.GetNextToken;

  end;


end;

function TProto3Parser.ParseOneOf: TOneOf;
// oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
// oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
var
  Token: TToken;
  OneOfType: AnsiString;
  OneOfField: TOneOfField;

begin
  Result := TOneOf.Create;

  Token := Tokenizer.GetNextToken;
  Result.FName := Token.TokenString;

  Tokenizer.Expect(ttkOpenBrace);
  Token := Tokenizer.GetNextToken;

  while Token.Kind <> ttkCloseBrace do
  begin
    Tokenizer.Rewind;
    OneOfField := TOneOfField.Create;
    OneOfField.FOneOfFieldType := ParseType;
    OneOfField.FName := ParseIdent;
    Tokenizer.Expect(ttkEqualSign);
    OneOfField.FFieldNumber := ParseFieldNumber;
    Result.OneOfFields.Add(OneOfField);
    Tokenizer.Expect(ttkSemiColon);

    Token := Tokenizer.GetNextToken;
  end;

end;

function TProto3Parser.ParseMap: TMap;
// mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
// keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
//          "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
// mapName = ident

begin
  Result := TMap.Create;

  Tokenizer.Expect(ttkLessThan);
  Result.FKeyType :=  ParseType;
  Tokenizer.Expect(ttkComma);
  Result.FValueType := ParseType;
  Tokenizer.Expect(ttkGreaterThan);
  Result.FName := ParseIdent;
  Tokenizer.Expect(ttkEqualSign);
  Result.FFieldNumber := ParseFieldNumber;
  Tokenizer.Expect(ttkSemiColon);

end;

function TProto3Parser.ParseMessageField: TMessageField;
// field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
// fieldOptions = fieldOption { ","  fieldOption }
// fieldOption = optionName "=" constant
// optionName = ( ident | "(" fullIdent ")" ) { "." ident }

var
  CurToken: TToken;

begin
  Result := TMessageField.Create;
  if Tokenizer.NextTokenIsIn(['repeated']) then
  begin
    Tokenizer.GetNextToken;
    Result.FIsRepeated := True;
  end;

  Result.FFieldType := ParseType;
  Result.FName := ParseIdent;
  Tokenizer.Expect(ttkEqualSign);
  Result.FFieldNumber := ParseFieldNumber;

  Result.FOptions := MaybeParseOptions(ttkOpenBracket, ttkCloseBracket);

  if CurToken.Kind <> ttkSemiColon then
    raise EInvalidToken.Create(CurToken.TokenString, ttkSemiColon);

end;

function TProto3Parser.ParseConstant: TConstant;
// constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit
begin
  Result := '';
  raise ENotImplementedYet.Create('ParseConstant is not Implemented Yet');
end;


constructor TProto3Parser.Create(_Tokenizer: TTokenizer);
begin
  inherited Create(_Tokenizer);

end;

destructor TProto3Parser.Destroy;
begin
  inherited Destroy;
end;

function TProto3Parser.ParseProto: TProto;
var
  Token: TToken;
// syntax { import | package | option | topLevelDef | emptyStatement }
// topLevelDef = message | enum | service
begin
  // syntax is already processed.

  Result := TProto3.Create;
  Token := Tokenizer.GetNextToken;
  while Token.Kind <> ttkEOF do
  begin
    if Token.TokenString = 'import' then
      Result.Imports.Add(ParseImport)
    else if Token.TokenString = 'package' then
      Result.Packages.Add(ParsePackage)
    else if Token.TokenString = 'option' then
      Result.Options.Add(ParseOption(ttkSemiColon))
    else if Token.TokenString = 'message' then
      Result.Messages.Add(ParseMessage)
    else if Token.TokenString = 'enum' then
      Result.Enums.Add(ParseEnum)
    else if Token.TokenString = 'service' then
      raise ENotImplementedYet.Create('Service is not supported yet!')
    else
      raise Exception.Create(Format('Invalid token: %s', [Token.TokenString]));
    Token := Tokenizer.GetNextToken;
  end;

end;

{ EInvalidCharacter }

constructor EInvalidCharacter.Create(Ch: Char; Code: Integer);
begin
  inherited Create(Format('Invalid Character %s (%d)', [Ch, Code]));
end;

{ TProto3 }

constructor TProto3.Create;
begin
  inherited Create;

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

constructor TMap.Create;
begin
  inherited Create;

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

constructor TOneOf.Create;
begin
  inherited Create;

  OneOfFields := TOneOfFields.Create;

end;

{ TOneOfField }

constructor TOneOfField.Create;
begin
  inherited Create;

end;

destructor TOneOfField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TOneOfField.ToString(Indent: AnsiString): AnsiString;
begin
  Result:= Format('%s %s %s = %d (%s);', [Indent, FOneOfFieldType, FName,
    FFieldNumber, FOptions.ToString('')]);

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

function TObjectList.ToString(Indent: AnsiString): AnsiString;
var
  Obj: TObject;

begin
  Result := '';
  for Obj in Self do
    Result += Obj.ToString(Indent + '.') + #10;
end;

{ TEnumField }

constructor TEnumField.Create;
begin
  inherited Create;

end;

destructor TEnumField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TEnumField.ToString(Indent: AnsiString): AnsiString;
begin
  Result := Format('%s %s %s %d', [Indent, Name, Options.ToString(''), FValue]);

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

constructor TEnum.Create;
begin
  inherited Create;

  FOptions := TOptions.Create;

end;

destructor TEnum.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TEnum.ToString(Indent: AnsiString): AnsiString;
begin
  Result := Format('%s enum %s'#10, [Indent, FName]);
  Result += inherited ToString(Indent + '..');
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

constructor TMessage.Create;
begin
  inherited Create;

  FFields := specialize TObjectList<TMessageField>.Create;
  FMessages := specialize TObjectList<TMessage>.Create;
  FOptions := specialize TObjectList<TOption>.Create;
  FOneOfs := specialize TObjectList<TOneOf>.Create;
  FMaps := specialize TObjectList<TMap>.Create;
  FEnums := specialize TObjectList<TEnum>.Create;
  {

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
                                }
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

function TMessage.ToString(Indent: AnsiString): AnsiString;
begin
  Result:= Format('%s Name = %s'#10'%s Options: (%s) '#10+'%s Fields: '#10+ '(%s) '#10+
    '%s Messages: (%s)'+#10'%s Enums: (%s)',
    [Indent, Name, Indent, Options.ToString(''),
    Indent, Fields.ToString(Indent + '..'),
      Indent, Messages.ToString(Indent + '..'),
      Indent, Enums.ToString(Indent + '..')]);
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

function TMessageField.GetOptions: specialize TObjectList<TOption>;
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

constructor TMessageField.Create;
begin
  inherited Create;

  FIsRepeated := False;
end;

destructor TMessageField.Destroy;
begin
  FOptions.Free;

  inherited Destroy;
end;

function TMessageField.ToString(Indent: AnsiString): AnsiString;
begin
  Result:= Indent;
  if IsRepeated then
    Result += 'repeated ';
  Result += Format('%s %s = %d (%s)',
    [GetFieldType, GetName, GetFieldNumber, GetOptions.ToString('')]);

end;

{ TOption }

constructor TOption.Create(Name: TOptionName; Value: TConstValue);
begin
  inherited Create;

  FOptionName := Name;
  FConstValue := Value;
end;

function TOption.ToString(Indent: AnsiString): AnsiString;
begin
  Result := Format('%s %s -> %s', [Indent, FOptionName, FConstValue]);

end;

{ TTokenizer }

constructor TTokenizer.Create(Stream: TStream);
begin
  inherited Create;

  SetLength(WholeFile, Stream.Size);
  Stream.ReadBuffer(WholeFile[1], Stream.Size);
  FileSize := Stream.Size;
  Currents := TIntList.Create;
  Current := 1;
  Currents.Add(Current);

end;

destructor TTokenizer.Destroy;
begin
  Currents.Free;

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

function TTokenizer.GetNextToken: TToken;

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
    '*': Result.Kind := tckStar;
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
    '#': Result.Kind := tckNumberSign;
    '!': Result.Kind := tckExclamationMark;
    else
      WriteLn(Result.ch + ' ' + IntToStr(Ord(Result.Ch)));
      raise EInvalidCharacter.Create(Result.ch, Ord(Result.Ch));
    end;

  end;

  type
    TCharKindSet = set of TCharKind;

    function NextCharIs(ChSet: TCharKindSet): Boolean;
    var
      StartPos: Integer;
      Ch: TCharKind;
      NextChar: TChar;

    begin
      StartPos := Current;

      for Ch in ChSet do
      begin
        NextChar := GetNextChar;
        if (NextChar.Kind = tckEoF) or (NextChar.Kind <> Ch) then
        begin
          Current := StartPos;
          Exit(False);
        end;

      end;

      Current := StartPos;
      Result := True;
    end;

var
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
      Exit;
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
      Exit;
    end;
    tckEoF:
      Result.Kind := ttkEOF;
    tckSlash:
    begin
      Result.TokenString += CurrentChar.Ch;
      Result.Kind := ttkSlash;

      if NextCharIs([tckSlash]) then
      begin
        CurrentChar := GetNextChar;
        Result.TokenString += CurrentChar.Ch;
        Result.Kind := ttkComment;
        CurrentChar := GetNextChar;
        while not (CurrentChar.Kind in [tckEndLine, tckEoF]) do
        begin
          Result.TokenString += CurrentChar.Ch;
          CurrentChar := GetNextChar;
        end;
        Result := Self.GetNextToken;
        Exit;
      end
      else if NextCharIs([tckStar]) then
      begin
        CurrentChar := GetNextChar;
        Result.TokenString += CurrentChar.Ch;
        Result.Kind := ttkComment;

        while not NextCharIs([tckStar, tckSlash]) do
        begin
          Result.TokenString += CurrentChar.Ch;
          CurrentChar := GetNextChar;
        end;
        Result.TokenString += GetNextChar.Ch;
        Result.TokenString += GetNextChar.Ch;

        Result := Self.GetNextToken;
        Exit;
      end;

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

  Currents.Add(Current);
 end;

procedure TTokenizer.Rewind(Count: Integer);
begin
  Currents.Count := Currents.Count - Count;
  Current := Currents.Last;

end;

function TTokenizer.ExpectAll(const TokenStrs: array of AnsiString): Boolean;
var
  TokenStr: AnsiString;

begin
  for TokenStr in TokenStrs do
    Self.Expect(TokenStr);

end;

function TTokenizer.ExpectAll(Ts: array of TTokenKind): Boolean;
var
  TokenKind: TTokenKind;

begin
  for TokenKind in Ts do
    Self.Expect(TokenKind);
end;

function TTokenizer.Expect(const TokenStr: AnsiString): Boolean;
var
  Token: TToken;

begin
  Token := GetNextToken;
  Result := Token.TokenString = TokenStr;

  if not Result then
    raise EExpectFailed.Create(Format('Expected "%s" Visited "%s".',
      [Token.TokenString, TokenStr]));
end;

function TTokenizer.ExpectOne(Ts: array of TTokenKind): Boolean;
var
  Token: TToken;
  Kind: TTokenKind;

begin
  Token := GetNextToken;
  Result := False;
  for Kind in Ts do
    Result := Result or (Token.Kind = Kind);

  if not Result then
    raise EExpectFailed.Create(Format('Expected "%d" Visited "%d".',
              [Ord(Token.Kind), Ord(Ts[0])]));
end;

function TTokenizer.Expect(const TokenKind: TTokenKind): Boolean;
var
  Token: TToken;

begin
  Token := GetNextToken;
  Result := Token.Kind = TokenKind;

  if not Result then
    raise EExpectFailed.Create(Format('Expected "%d" Visited "%d".',
      [Token.Kind, TokenKind]));
end;

function TTokenizer.NextTokenIsIn(Ts: array of TTokenKind): Boolean;
var
  Next: TToken;
  Kind: TTokenKind;

begin
  Next := GetNextToken;
  Rewind;

  for Kind in Ts do
    if Next.Kind = Kind then
      Exit(True);

  Result := False;
end;

function TTokenizer.NextTokenIsIn(Ts: array of AnsiString): Boolean;
var
  Next: TToken;
  S: AnsiString;

begin
  Next := GetNextToken;

  for S in Ts do
    if Next.TokenString = S then
    begin
      Rewind(1);
      Exit(True);

    end;

  Rewind(1);
  Result := False;
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
  Parser: TProtoParser;
  InputStream: TFileStream;
  OutputStream: TFileStream;
  FilePath: AnsiString;
  OutputUnitName: AnsiString;

begin
  InputStream := TFileStream.Create(InputFilename, fmOpenRead);
  Parser := TProto.GetParser(InputStream);
  InputStream.Free;

  Proto := Parser.ParseProto;
  WriteLn(Proto.ToString);

  OutputUnitName := GetUnitName(InputFilename);
  OutputStream := TFileStream.Create(ConcatPaths([ExtractFileDir(InputFilename),
    OutputUnitName + '.pp']), fmCreate);

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

class function TProto.GetParser(InputStream: TStream): TProtoParser;
var
  Tokenizer: TTokenizer;
  Token: TToken;
  ProtoVersion: AnsiString;

begin
  Tokenizer := TTokenizer.Create(InputStream);

  Tokenizer.Expect('syntax');
  Tokenizer.ExpectAll([ttkEqualSign, ttkDoubleQuote]);
  ProtoVersion := Tokenizer.GetNextToken.TokenString;
  Tokenizer.ExpectAll([ttkDoubleQuote, ttkSemiColon]);

  if ProtoVersion = 'proto3' then
    Result := TProto3Parser.Create(Tokenizer)
  else
    raise Exception.Create('This library just works for Protobuf Version 3');
end;

function TProto.ToString: AnsiString;
var
  Message: TMessage;
  StrList: TStringList;

begin
  StrList := TStringList.Create;
  StrList.Add('Packages:' + Packages.Text);
  StrList.Add('Options:' + #10 + Options.ToString(''));
  StrList.Add('Imports:' + #10 + Imports.Text);

  for Message in Messages do
    StrList.Add(Message.ToString(''));

  Result := StrList.Text;
  StrList.Free;

end;

end.

