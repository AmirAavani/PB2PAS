unit PBParserUnit;
{$modeswitch advancedrecords}

{$mode objfpc}{$H+}

interface

uses
  PBDefinitionUnit, Classes, SysUtils, StreamUnit, gvector, ProtoHelperUnit;

type

  { TBaseProtoParser }

  TBaseProtoParser = class(TObject)
  private
    InputFilename: AnsiString;
  public
    class function GetParser(_InputFilename: AnsiString): TBaseProtoParser;
    class function Parse(_InputFilename: AnsiString): TProto;
    class function ParseAll(_InputFilename: AnsiString): TProtoMap;

    function ParseProto: TProto; virtual; abstract;
    destructor Destroy; override;
  end;

implementation

uses
  UtilsUnit, StringUnit, ALoggerUnit, PBOptionUnit, PathHelperUnit;

type
  TTokenKind = (ttkStart, ttkDot, ttkOpenBrace, ttkCloseBrace, ttkOpenPar,
    ttkClosePar, ttkSemiColon, ttkEqualSign, ttkStar,
    ttkColon, ttkComma, ttkDoubleQuote, ttkSingleQuote,
    ttkMinus, ttkPlus, ttkQuestionMark, ttkLessThan, ttkGreaterThan, ttkOpenBracket,
      ttkCloseBracket, ttkAtSgin,
    ttkIdentifier, ttkComment, ttkNumber, ttkSpace, ttkSlash,
    ttkEndLine, ttkEOF);
  TCharKind = (tckStart, tckDot, tckOpenBrace, tckCloseBrace, tckOpenPar,
    tckClosePar, tckSemiColon, tckEqualSign, tckStar,
      tckColon, tckComma, tckDoubleQuote, tckSingleQuote,
      tckMinus, tckPlus, tckQuestionMark, tckLessThan, tckGreaterThan, tckOpenBracket,
      tckCloseBracket, tckAtSgin,

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
  EParseIntFailed = class(Exception)

  end;

  { TTokenizer }

  TTokenizer = class(TObject)
  private
    Current: Integer;
    Currents: TIntList;
    FileSize: Integer;
    WholeFile: AnsiString;
    Stream: TStream;

    function ExpectAll(const TokenStrs: array of AnsiString): Boolean;
    function Expect(const TokenStr: AnsiString): Boolean;
    function ExpectAll(Ts: array of TTokenKind): Boolean;
    function ExpectOne(Ts: array of TTokenKind): Boolean;
    function Expect(const TokenKind: TTokenKind): Boolean;

    function NextTokenIsIn(Ts: array of TTokenKind): Boolean;
    function NextTokenIsIn(Ts: array of AnsiString): Boolean;
  public
    constructor Create(_Stream: TStream);
    destructor Destroy; override;

    function GetNextToken: TToken;
    procedure Rewind(Count: Integer = 1);

  end;

  { TTokenArray }

  TTokenArray = class(specialize TVector<TToken>)
  public
    procedure AddToken(aToken: TToken);
    destructor Destroy; override;

  end;

  { TProtoParser }

  TProtoParser = class(TBaseProtoParser)
  protected
    FTokenizer: TTokenizer;

    function ParseStrLit: TStrLit;
    function ParseIdent: TIdentifier;
    function ParseFieldNumber: Integer;
    function ParseFullIdent: TFullIdentifier;
    function ParseType: AnsiString;

    function CollectUntil(EndTokenKind: TTokenKind): TTokenArray;

    property Tokenizer: TTokenizer read FTokenizer;
  public
    constructor Create(_Tokenizer: TTokenizer);
    destructor Destroy; override;

  end;

  { TProto3Parser }

  TProto3Parser = class(TProtoParser)
  private
    function ParseImport: AnsiString;
    function ParseSyntax: Boolean;
    function ParseMessage(Parent: TParent): TMessage;
    function ParseEnum(Parent: TParent): TEnum;
    function ParseEnumField(EnumName: AnsiString): TEnumField;
    function ParsePackage: AnsiString;
    function ParseOption(EndTokenTypes: array of TTokenKind): TOption;
    function MaybeParseOptions: TOptions;
    function ParseOneOf(ParentMessage: TMessage): TOneOf;
    function ParseOneOfField(ParentOneOf: TOneOf): TOneOfField;
    function ParseMap(ParentMessage: TMessage): TMap;
    function ParseMessageField(ParentMessage: TMessage): TMessageField;
    function ParseConstant: TConstant;

  public
    constructor Create(_Tokenizer: TTokenizer);
    destructor Destroy; override;

    function ParseProto: TProto; override;

  end;

  EExpectFailed = class(Exception)
  end;

{ TTokenizer }

constructor TTokenizer.Create(_Stream: TStream);
begin
  inherited Create;

  Stream := _Stream;
  Stream.Position := 0;
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
  Stream.Free;

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
    '@': Result.Kind := tckAtSgin;
    '+': Result.Kind := tckPlus
    else
      FatalLn(Result.ch + ' ' + IntToStr(Ord(Result.Ch)));
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
    tckLetter, tckUnderline:
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
      tckCloseBracket, tckAtSgin:
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
      FatalLn(CurrentChar.ch + ' Ch:' + IntToStr(Ord(CurrentChar.Ch)) +
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
    if not Self.Expect(TokenStr) then
      Exit(False);

  Result := True;

end;

function TTokenizer.ExpectAll(Ts: array of TTokenKind): Boolean;
var
  TokenKind: TTokenKind;

begin
  for TokenKind in Ts do
    if not Self.Expect(TokenKind) then
      Exit(False);

  Result := True;

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

{ TTokenArray }

procedure TTokenArray.AddToken(aToken: TToken);
begin
  Self.PushBack(aToken);

end;

destructor TTokenArray.Destroy;
begin
  inherited Destroy;

end;

function TProtoParser.ParseType: AnsiString;
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

  if Result = 'bytes' then
  begin
    raise EParseIntFailed.Create('Bytes fields are not supported yet. Please use string instead.');
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
  if (Token.TokenString <> '') and (Token.TokenString[1] in ['a'..'z', 'A'..'Z', '_']) then
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

class function TBaseProtoParser.GetParser(_InputFilename: AnsiString): TBaseProtoParser;
var
  TmpTokenizer: TTokenizer;
  ProtoVersion: AnsiString;
  InputStream: TFileStream;

begin
  InputStream := TFileStream.Create(_InputFilename, fmOpenRead);
  TmpTokenizer := TTokenizer.Create(InputStream);

  TmpTokenizer.Expect('syntax');
  TmpTokenizer.ExpectAll([ttkEqualSign, ttkDoubleQuote]);
  ProtoVersion := TmpTokenizer.GetNextToken.TokenString;
  TmpTokenizer.ExpectAll([ttkDoubleQuote, ttkSemiColon]);

  if ProtoVersion = 'proto3' then
  begin
    Result := TProto3Parser.Create(TmpTokenizer);
    Result.InputFilename := _InputFilename;
  end
  else
    raise Exception.Create('This library just works for Protobuf Version 3');

end;

class function TBaseProtoParser.Parse(_InputFilename: AnsiString): TProto;
var
  Parser: TBaseProtoParser;

begin
  Parser := TBaseProtoParser.GetParser(_InputFilename);

  Result := Parser.ParseProto;

  Parser.Free;

end;

class function TBaseProtoParser.ParseAll(_InputFilename: AnsiString): TProtoMap;

  procedure RecParse(FilePath, ProtoFile: AnsiString; ProtoMap: TProtoMap);
  var
    Proto: TProto;
    Import: AnsiString;

  begin
    FMTDebugLn('Parsing %s', [JoinPath(FilePath, ProtoFile)]);

    Proto := TBaseProtoParser.Parse(JoinPath(FilePath, ProtoFile));
    ProtoMap.Add(JoinPath(FilePath, ProtoFile), Proto);

    for Import in Proto.Imports do
      if ProtoMap.Find(JoinPath(FilePath, Import)) = nil then
        RecParse(FilePath, Import, ProtoMap);
  end;

var
  Filename: AnsiString;

begin
  Result := TProtoMap.Create;
  Filename:= _InputFilename;
  if not IsPrefix('./', _InputFilename) and not IsPrefix('/', _InputFilename) then
    Filename := './' + _InputFilename;

  RecParse(ExtractFilePath(Filename), ExtractFileName(Filename), Result);

end;

destructor TBaseProtoParser.Destroy;
begin
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

function TProto3Parser.ParseMessage(Parent: TParent): TMessage;
var
  Token: TToken;
  Name: AnsiString;
  Enums: TEnums;
  Messages: TMessages;
  Fields: TMessageFields;
  Options: TOptions;

{
  message = "message" messageName messageBody
  messageName = ident
  messageBody = "{" { field | enum | message | option | oneof | mapField |
  reserved | emptyStatement } "}"
}
begin
  Name := ParseIdent;;
  Tokenizer.Expect(ttkOpenBrace);

  Token := Tokenizer.GetNextToken;

  Enums := TEnums.Create;
  Messages := TMessages.Create;
  Options := TOptions.Create;
  Fields := TMessageFields.Create;

  Result := TMessage.Create(Name, Fields, Messages, Options, Enums, Parent);

  while Token.Kind <> ttkCloseBrace do
  begin
    if Token.TokenString = 'enum' then
      Enums.Add(ParseEnum(CreateParent(nil, Result, nil)))
    else if Token.TokenString = 'message' then
      Messages.Add(Self.ParseMessage(CreateParent(nil, Result, Parent.Proto)))
    else if Token.TokenString = 'option' then
      Options.Add(ParseOption(ttkSemiColon))
    else if Token.TokenString = 'oneof' then
      Fields.Add(ParseOneOf(Result))
    else if Token.TokenString = 'map' then
      Fields.Add(ParseMap(Result))
    else if Token.TokenString = 'reserved' then
      raise Exception.Create('NIY reserved')
    else if Token.Kind = ttkSemiColon then
    else
    begin
      Tokenizer.Rewind;
      Fields.Add(ParseMessageField(Result));
    end;

    Token := Tokenizer.GetNextToken;
  end;

end;

function TProto3Parser.ParseEnum(Parent: TParent): TEnum;
var
  Token: TToken;
  FName: AnsiString;
  Options: TOptions;
  EnumFields: TEnumFields;

(*
  enum = "enum" enumName enumBody
  enumBody = "{" { option | enumField | emptyStatement } "}"
  enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
  enumValueOption = optionName "=" constant

  enumName = Ident
*)
begin
  Token := Tokenizer.GetNextToken;
  FName := Token.TokenString;
  Options := TOptions.Create;
  EnumFields := TEnumFields.Create;

  Tokenizer.Expect(ttkOpenBrace);
  Token := Tokenizer.GetNextToken;
  while Token.Kind <> ttkCloseBrace do
  begin
    if Token.TokenString = 'option' then
      Options.Add(ParseOption(ttkSemiColon))
    else if Token.Kind = ttkSemiColon then
    else
    begin
      Tokenizer.Rewind;
      EnumFields.Add(ParseEnumField(FName))

    end;

    Token := Tokenizer.GetNextToken;
  end;

  Result := TEnum.Create(FName, Options, EnumFields, Parent);

end;

function TProto3Parser.ParseEnumField(EnumName: AnsiString): TEnumField;
var
  Token: TToken;
  Name: AnsiString;
  Value: Integer;
  Options: TOptions;
(*
  enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
  enumValueOption = optionName "=" constant
*)
begin
  Token := Tokenizer.GetNextToken;
  Name := Token.TokenString;
  Tokenizer.Expect(ttkEqualSign);
  Token := Tokenizer.GetNextToken;
  Value:= StrToInt(Token.TokenString);

  Options := MaybeParseOptions;

  Tokenizer.Expect(ttkSemiColon);

  Result := TEnumField.Create(EnumName, Name, Options, Value);


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


function TProto3Parser.ParseOption(EndTokenTypes: array of TTokenKind): TOption;
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
  Value := ParseConstant;

  Tokenizer.ExpectOne(EndTokenTypes);
  Result := TOption.Create(OptionName, Value);
end;

function TProto3Parser.MaybeParseOptions: TOptions;
var
  Token: TToken;
  tt: TTokenKind;

begin
  Result := TOptions.Create;
  Token := Tokenizer.GetNextToken;
  if Token.Kind  <> ttkOpenBracket then
  begin
    Tokenizer.Rewind;
    Exit;
  end;

  Token := Tokenizer.GetNextToken;
  while not IsTokenIn(Token, [ttkCloseBracket]) do
  begin
    Tokenizer.Rewind();
    Result.Add(ParseOption([ttkComma, ttkCloseBracket]));

    Tokenizer.Rewind(1);
    Token := Tokenizer.GetNextToken;
    if Token.Kind <> ttkCloseBracket then
      Token := Tokenizer.GetNextToken;

  end;

end;

function TProto3Parser.ParseOneOf(ParentMessage: TMessage): TOneOf;
// oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
// oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
var
  Token: TToken;
  Name: AnsiString;
  OneOfFields: TOneOfFields;


begin
  Token := Tokenizer.GetNextToken;
  Name := Token.TokenString;

  Tokenizer.Expect(ttkOpenBrace);
  Token := Tokenizer.GetNextToken;

  OneOfFields := TOneOfFields.Create;
  Result := TOneOf.Create(Name, OneOfFields, ParentMessage);

  while Token.Kind <> ttkCloseBrace do
  begin
    Tokenizer.Rewind;
    OneOfFields.Add(ParseOneOfField(Result));
    Token := Tokenizer.GetNextToken;

  end;

end;

function TProto3Parser.ParseOneOfField(ParentOneOf: TOneOf): TOneOfField;
// oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
var
  OneOfFieldType: AnsiString;
  Name: AnsiString;
  FieldNumber: Integer;
  Options: TOptions;

begin
  OneOfFieldType := ParseType;
  Name := ParseIdent;
  Tokenizer.Expect(ttkEqualSign);
  FieldNumber := ParseFieldNumber;
  Options := MaybeParseOptions;
  Tokenizer.Expect(ttkSemiColon);

  Result := TOneOfField.Create(Name, OneOfFieldType, False, FieldNumber, options,
    ParentOneOf);

end;

function TProto3Parser.ParseMap(ParentMessage: TMessage): TMap;
var
  KeyType, ValueType: AnsiString;
  Name: AnsiString;
  FieldNumber: Integer;
// mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
// keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
//          "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
// mapName = ident

begin
  Tokenizer.Expect(ttkLessThan);
  KeyType :=  ParseType;
  Tokenizer.Expect(ttkComma);
  ValueType := ParseType;
  Tokenizer.Expect(ttkGreaterThan);
  Name := ParseIdent;
  Tokenizer.Expect(ttkEqualSign);
  FieldNumber := ParseFieldNumber;
  Tokenizer.Expect(ttkSemiColon);

  Result := TMap.Create(Name, FieldNumber, KeyType, ValueType, ParentMessage);

end;

function TProto3Parser.ParseMessageField(ParentMessage: TMessage
  ): TMessageField;
var
  Name: AnsiString;
  FieldType: AnsiString;
  IsRepeated: Boolean;
  FieldNumber: Integer;
  Options: TOptions;

// field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
// fieldOptions = fieldOption { ","  fieldOption }
// fieldOption = optionName "=" constant
// optionName = ( ident | "(" fullIdent ")" ) { "." ident }

begin
  IsRepeated := False;
  if Tokenizer.NextTokenIsIn(['repeated']) then
  begin
    Tokenizer.GetNextToken;
    IsRepeated := True;
  end;

  FieldType := ParseType;
  Name := ParseIdent;
  Tokenizer.Expect(ttkEqualSign);
  FieldNumber := ParseFieldNumber;

  Options := MaybeParseOptions;

  Tokenizer.Expect(ttkSemiColon);

  Result := TMessageField.Create(Name, FieldType, IsRepeated, FieldNumber,
    Options, CreateParent(nil, ParentMessage, nil));

end;

function TProto3Parser.ParseConstant: TConstant;
  function ParseIntLit: TConstant;
  // intLit     = decimalLit | octalLit | hexLit
  // decimalLit = ( "1" … "9" ) { decimalDigit }
  // octalLit   = "0" { octalDigit }
  // hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
  var
    Token: TToken;

  begin
    Token := Tokenizer.GetNextToken;

    if IsPrefix('+', Token.TokenString) then
    if Token.TokenString  = '0' then
      Exit(Token.TokenString);

    if Token.TokenString[1] in ['1'..'9'] then
      Exit(Token.TokenString);
    if (Token.TokenString[1] = '0') and (UpCase(Token.TokenString[2]) = 'X') then
      Exit(Token.TokenString)
    else
    Exit(Token.TokenString);

  end;

  // TODO(Amir): This is not implemented yet.
  function ParseFloatLit: TConstant;
  // floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
  // decimals  = decimalDigit { decimalDigit }
  // exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
  var
    Token: TToken;

  begin
    Token := Tokenizer.GetNextToken;
    raise ENotImplementedYet.Create('ParseFloatLit');
  end;


  // TODO(Amir): This needs some work yet.
  function ParseStringLit: TConstant;
  // strLit = ( "'" { charValue } "'" ) |  ( '"' { charValue } '"' )
  //charValue = hexEscape | octEscape | charEscape | /[^\0\n\\]/
  //hexEscape = '\' ( "x" | "X" ) hexDigit hexDigit
  //octEscape = '\' octalDigit octalDigit octalDigit
  //charEscape = '\' ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | '\' | "'" | '"' )
  // quote = "'" | '"
  var
    Token: TToken;
    Start: TToken;

  begin
    Start := Tokenizer.GetNextToken;
    if not (Start.Kind in [ttkSingleQuote, ttkDoubleQuote]) then
      raise EInvalidToken.Create(Start.TokenString, ttkDoubleQuote);

    Result := Start.TokenString;

    Token := Tokenizer.GetNextToken;

    while Token.Kind <> Start.Kind do
    begin
      Result += Token.TokenString;
      Token := Tokenizer.GetNextToken;

    end;
    Result += Token.TokenString;

  end;

  function ParseBoolLit: TConstant;
  // boolLit = "true" | "false"
  var
    Token: TToken;

  begin
    Token := Tokenizer.GetNextToken;

    if (Token.TokenString <> 'true') and (Token.TokenString <> 'false') then
      raise EInvalidToken.Create(Token.TokenString, Token.Kind);

    Result := Token.TokenString;
  end;

// constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit
var
  Token: TToken;

begin
  Result := '';

  Token := Tokenizer.GetNextToken;
  Tokenizer.Rewind;

  if (Token.TokenString = 'true') or (Token.TokenString = 'false') then
    Exit(ParseBoolLit);
  if Token.TokenString[1] in ['a'..'z', 'A'..'Z'] then
    Exit(ParseFullIdent);

  if IsPrefix('-', Token.TokenString) or IsPrefix('+', Token.TokenString) then
  begin
    try
      Result := Token.TokenString + ParseIntLit;
      Exit;
    except
      on e: EParseIntFailed do
      begin

        e.Free;
      end;
    end;

    Result := Token.TokenString + ParseFloatLit;
    Exit;
  end;

  if Token.TokenString[1] in ['0'..'9'] then
  begin
    try
      Result := ParseIntLit;
      Exit;
    except
      on e: EParseIntFailed do
      begin

        e.Free;
      end;
    end;

    Result := ParseFloatLit;
    Exit;
  end;

  if IsPrefix('''', Token.TokenString) or IsPrefix('"', Token.TokenString) then
    Result := ParseStringLit
  else
    Result := ParseBoolLit;
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
  Imports: TImports;
  OtherParams: TStringList;
  Options: TOptions;
  Messages: TMessages;
  Enums: TEnums;

// syntax { import | package | option | topLevelDef | emptyStatement }
// topLevelDef = message | enum | service
begin
  // syntax is already processed.
  OtherParams := TStringList.Create;

  Imports := TImports.Create;
  Options := TOptions.Create;
  Messages := TMessages.Create;
  Enums := TEnums.Create;

  Result := TProto3.Create(Imports, Options, Messages, Enums, OtherParams);

  OtherParams.Add('syntax:3');
  OtherParams.Add('InputProtoFilename:'+ InputFilename);

  Token := Tokenizer.GetNextToken;
  while Token.Kind <> ttkEOF do
  begin
    if Token.TokenString = 'import' then
      Imports.Add(ParseImport)
    else if Token.TokenString = 'package' then
      OtherParams.Add('package:' + ParsePackage)
    else if Token.TokenString = 'option' then
      Options.Add(ParseOption(ttkSemiColon))
    else if Token.TokenString = 'message' then
      Messages.Add(ParseMessage(CreateParent(nil, nil, Result)))
    else if Token.TokenString = 'enum' then
      Enums.Add(ParseEnum(CreateParent(nil, nil, Result)))
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

end.

