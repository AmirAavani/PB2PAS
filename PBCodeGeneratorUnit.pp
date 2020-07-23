unit PBCodeGeneratorUnit;

{$mode objfpc}{$H+}

interface

uses
  PBDefinitionUnit, UtilsUnit, Classes, SysUtils, fgl;

type
  { TPBBaseCodeGenerator }

  TPBBaseCodeGenerator = class(TObject)
  public
    class function GetCodeGenerator(Proto: TProto; RelatedProtos: TProtos): TPBBaseCodeGenerator;

    procedure GenerateCode; virtual; abstract;
    destructor Destroy; override;
  end;

implementation
uses
  StreamUnit, StringUnit, ALoggerUnit, strutils;

type

  { TUnitCode }

  // TODO(Amir): Maybe expand this class.
  // TOOD(Amir): Maybe use TemplateUnit.
  TUnitCode = class(TObject)
  private
    type

      { TInterface }

      TInterface = class(TObject)
        UsesList: TStringList;
        TypeList: TStringList;

        constructor Create;
        destructor Destroy; override;
        function ToString: AnsiString; override;
      end;

      { TImplementation }

      TImplementation = class(TObject)
        UsesList: TStringList;
        TypeList: TStringList;
        Methods: TStringList;

        constructor Create;
        destructor Destroy; override;
        function ToString: AnsiString; override;
      end;
  private
    Name: AnsiString;
    InterfaceCode: TInterface;
    ImplementationCode: TImplementation;

  public
    constructor Create(UName: AnsiString);
    destructor Destroy; override;

    function ToString: AnsiString; override;
  end;

  { TPBCodeGeneratorV1 }

  TPBCodeGeneratorV1 = class(TPBBaseCodeGenerator)
  private
    Proto: TProto;
    RelatedProtos: TProtos;
    OutputStream: TStream;

  protected
    procedure GenerateCodeForImports(const Imports: TImports; out UnitCode: TUnitCode; const Prefix, Indent: AnsiString);
    procedure GenerateCodeForEnum(const AnEnum: TEnum; out Unitcode: TUnitCode; const Indent: AnsiString);
    procedure GenerateCodeForMessage(const AMessage: TMessage; out Unitcode: TUnitCode; const Indent: AnsiString);
    procedure GenerateCodeForMessageField(const aField: TMessageField; MessageClassName: AnsiString; out UnitCode: TUnitCode; const Indent: AnsiString);
    procedure GenerateCodeForOneOf(const OneOf: TOneOf; out Unitcode: TUnitCode; const Indent: AnsiString);

  public
    procedure GenerateCode; override;

    constructor Create(_Proto: TProto; _RelatedProtos: TProtos);
    destructor Destroy; override;

  end;

procedure WriteStr(const aText: AnsiString; var Target: AnsiString);
begin
  Target += aText;
end;

procedure WriteLineStr(const aText: AnsiString; var Target: AnsiString);
begin
  Target += aText + #10;
end;

{ TUnitCode.TInterface }

constructor TUnitCode.TInterface.Create;
begin
  inherited;

  UsesList := TStringList.Create;
  TypeList := TStringList.Create;

end;

destructor TUnitCode.TInterface.Destroy;
begin
  UsesList.Free;
  TypeList.Free;

  inherited Destroy;
end;

function TUnitCode.TInterface.ToString: AnsiString;
begin
  Result :=  '';
  WriteLineStr('interface', Result);
  WriteLineStr('', Result);

  if Self.UsesList.Count <> 0 then
    WriteLineStr(Format('uses%s %s;', [sLineBreak, JoinStrings(Self.UsesList, ', ')]), Result);
  WriteLineStr('', Result);

  if Self.TypeList.Count <> 0 then
  begin
    WriteLineStr('type', Result);
    WriteLineStr(Format('%s', [JoinStrings(Self.TypeList, sLineBreak)]), Result);

  end;

end;

{ TUnitCode.TImplementation }

constructor TUnitCode.TImplementation.Create;
begin
  inherited;

  UsesList := TStringList.Create;
  TypeList := TStringList.Create;
  Methods := TStringList.Create;

end;

destructor TUnitCode.TImplementation.Destroy;
begin
  UsesList.Free;
  TypeList.Free;
  Methods.Free;

  inherited Destroy;
end;

function TUnitCode.TImplementation.ToString: AnsiString;
begin
  Result :=  'implementation'+ sLineBreak + sLineBreak;

  if Self.UsesList.Count <> 0 then
    WriteLineStr(Format('uses %s;', [JoinStrings(Self.UsesList, ',')]), Result);

  WriteLineStr(Format('%s', [JoinStrings(Self.Methods, sLineBreak)]), Result);
end;

{ TUnitCode }

constructor TUnitCode.Create(UName: AnsiString);
begin
  inherited Create;

  Name := UName;
  InterfaceCode := TInterface.Create;
  ImplementationCode := TImplementation.Create;
end;

destructor TUnitCode.Destroy;
begin
  InterfaceCode.Free;
  ImplementationCode.Free;

  inherited Destroy;
end;

function TUnitCode.ToString: AnsiString;
begin
  Result := '';
  WriteLineStr(Format('unit %s;', [Name]), Result);
  WriteLineStr(Format('{$Mode objfpc}', []), Result);
  WriteLineStr('', Result);
  WriteLineStr(InterfaceCode.ToString, Result);
  WriteLineStr('', Result);
  WriteLineStr(ImplementationCode.ToString, Result);
  WriteLineStr('', Result);
  WriteLineStr('end.', Result);

end;

{ TPBCodeGeneratorV1 }

procedure TPBCodeGeneratorV1.GenerateCodeForImports(const Imports: TImports;
  out UnitCode: TUnitCode; const Prefix, Indent: AnsiString);
const
  DefaultUsesList : array of AnsiString = (
    'classes',
    'fgl',
    'sysutils',
    'ProtoHelperUnit',
    'ProtoHelperListsUnit',
    'ProtoStreamUnit'
  );
var
  S: AnsiString;
  Import: AnsiString;

begin
  for s in DefaultUsesList do
    UnitCode.InterfaceCode.UsesList.Add(s);

  for Import in Imports do
    UnitCode.InterfaceCode.UsesList.Add(GetUnitName(Import));
end;

procedure TPBCodeGeneratorV1.GenerateCodeForEnum(const AnEnum: TEnum; out
  Unitcode: TUnitCode; const Indent: AnsiString);
var
  EnumField: TEnumField;
  i: Integer;
  Code: AnsiString;

begin
  Code := '';
  WriteLineStr(Format('%s// %s', [Indent, AnEnum.Name]), Code);
  WriteLineStr(Format('%sT%s = (', [Indent, Canonicalize(AnEnum.Name)]), Code);

  for i := 0 to AnEnum.Count - 1 do
  begin
    EnumField := AnEnum[i];
    WriteStr(Format('%s%s = %d', [Indent + '  ', EnumField.Name, EnumField.Value]),
      Code);
    if i <> AnEnum.Count - 1 then
      WriteLineStr(',', Code)
    else
      WriteLineStr('', Code);
  end;
  WriteLineStr(Format('%s);', [Indent]), Code);

  Unitcode.InterfaceCode.TypeList.Add(Code);
end;

function GetFieldType(aField: TMessageField; Proto: TProto; RelatedProtos: TProtos): AnsiString;
begin
  if aField.IsRepeated then
    Exit(aField.FPCType);
  Result :=  JoinStrings([GetUnitName(aField, Proto, RelatedProtos), aField.FPCType], '.', True);

end;

procedure TPBCodeGeneratorV1.GenerateCodeForMessage(const AMessage: TMessage;
  out Unitcode: TUnitCode; const Indent: AnsiString);

  procedure GenerateDeclarationForMessage(
    const aMessage: TMessage; out Unitcode: TUnitCode);
  var
    i: Integer;
    Field: TMessageField;
    Enum: TEnum;
    S: AnsiString;
    Option: TOption;
    MessageClassName: AnsiString;

  begin
    MessageClassName := GetMessageClassName(aMessage);
    Unitcode.InterfaceCode.TypeList.Add(Format('%s{ T%s }', [Indent, aMessage.Name]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sT%s = class(TBaseMessage)', [Indent, aMessage.Name]));
    for Option in aMessage.Options do
      Unitcode.InterfaceCode.TypeList.Add(Format('%s// %s = %s', [Indent + '  ', Option.OptionName, Option.ConstValue]));

    for Enum in aMessage.Enums do
    begin
      Unitcode.InterfaceCode.TypeList.Add('%stype', [Indent]);
      GenerateCodeForEnum(Enum, Unitcode, Indent + '  ');
    end;

    if aMessage.Enums.Count <> 0 then
      Unitcode.InterfaceCode.TypeList.Add('');

    for Field in aMessage.Fields do
      if Field.ClassType = TOneOf then
      begin
        Unitcode.InterfaceCode.TypeList.Add(Format('%spublic type', [Indent]));
        GenerateCodeForOneOf(Field as TOneOf, Unitcode, Indent + '  ');
        Unitcode.InterfaceCode.TypeList.Add('');

      end;

    for i := 0 to aMessage.Messages.Count - 1 do
    begin
      Unitcode.InterfaceCode.TypeList.Add('%spublic type', [Indent]);
      GenerateCodeForMessage(aMessage.Messages[i], Unitcode, Indent + '  ');

    end;

    Unitcode.InterfaceCode.TypeList.Add('%spublic', [Indent]);
    for Field in aMessage.Fields do
      GenerateCodeForMessageField(Field, MessageClassName, Unitcode, Indent + '  ');

    Unitcode.InterfaceCode.TypeList.Add(Format('%sprotected ', [Indent]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  procedure SaveToStream(Stream: TProtoStreamWriter); override;',
      [Indent]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;',
    [Indent]));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add('%spublic', [Indent]);
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  constructor Create;', [Indent]));
    if aMessage.Fields.Count <> 0 then
    begin
      S := Format('%sconstructor Create(', [Indent]);
      for Field in aMessage.Fields do
        S += Format('a%s: %s; ', [Canonicalize(Field.Name),
           GetFieldType(Field, Proto, RelatedProtos)]);

      S[Length(S) - 1] := ')';
      S[Length(S)] := ';';
      Unitcode.InterfaceCode.TypeList.Add(S);
    end;

    Unitcode.InterfaceCode.TypeList.Add(Format('%sdestructor Destroy; override;', [Indent + '  ']));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sfunction ToString: AnsiString; override;', [Indent + '  ']));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add(Format('%send;', [Indent]));
    Unitcode.InterfaceCode.TypeList.Add('');

  end;

  procedure GenerateImplementationForMessage(const aMessage: TMessage; out Unitcode: TUnitCode);

    procedure GenerateConstructors;
    var
      Field: TMessageField;
      CreateDeclaration: AnsiString;
      MessageClassName: AnsiString;

    begin
      MessageClassName := GetMessageClassName(aMessage);

      Unitcode.ImplementationCode.Methods.Add(Format('constructor %s.Create;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  inherited Create;');
      Unitcode.ImplementationCode.Methods.Add(sLineBreak);
      Unitcode.ImplementationCode.Methods.Add('end;' + sLineBreak);

      if aMessage.Fields.Count <> 0 then
      begin
        CreateDeclaration := 'Create(';
        for Field in aMessage.Fields do
          CreateDeclaration += Format('a%s: %s; ', [Canonicalize(Field.Name),
            GetFieldType(Field, Proto, RelatedProtos)]);
        CreateDeclaration[Length(CreateDeclaration) - 1] := ')';
        CreateDeclaration[Length(CreateDeclaration)] := ';';
        Unitcode.ImplementationCode.Methods.Add(Format('constructor %s.%s',
          [MessageClassName, CreateDeclaration]));
        Unitcode.ImplementationCode.Methods.Add('begin');
        Unitcode.ImplementationCode.Methods.Add(Format('  inherited Create;', []));
        Unitcode.ImplementationCode.Methods.Add('');
        for Field in aMessage.Fields do
          Unitcode.ImplementationCode.Methods.Add(Format('  F%s := a%s; ', [Canonicalize(Field.Name),
              Canonicalize(Field.Name)]));

        Unitcode.ImplementationCode.Methods.Add(sLineBreak + 'end;');
      end;

    end;

    procedure GenerateDestructor;
    var
      Field: TMessageField;
      CanName: AnsiString;
      MessageClassName: AnsiString;

    begin
      MessageClassName := GetMessageClassName(aMessage);
      Unitcode.ImplementationCode.Methods.Add(Format('destructor %s.Destroy;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);
        if not IsSimpleType(Field, Self.Proto, RelatedProtos) or Field.IsRepeated then
          Unitcode.ImplementationCode.Methods.Add(Format('  F%s.Free;', [CanName]));
      end;
      Unitcode.ImplementationCode.Methods.Add('');
      Unitcode.ImplementationCode.Methods.Add('  inherited;');
      Unitcode.ImplementationCode.Methods.Add('end;');

    end;

    function MessageHasRepeatedNonSimple: Boolean;
    var
      Field: TMessageField;

    begin
      Result := False;

      for Field in aMessage.Fields do
        if Field.IsRepeated and not IsSimpleType(Field, Self.Proto, RelatedProtos) then
          Exit(True);

    end;

    procedure GenerateToString;
    var
      Field: TMessageField;

    begin
      Unitcode.ImplementationCode.Methods.Add(Format('function %s.ToString: AnsiString;', [GetMessageClassName(aMessage)]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  Result := '''';');
      Unitcode.ImplementationCode.Methods.Add('');

      for Field in aMessage.Fields do
        if IsAnEnumType(Field, Proto, RelatedProtos) then
          Unitcode.ImplementationCode.Methods.Add(Format('  Result += IntToStr(Ord(F%s));' + sLineBreak,
                  [Canonicalize(Field.Name)]))
        else if Field.FPCType <> 'AnsiString' then
          Unitcode.ImplementationCode.Methods.Add(Format('  Result += F%s.ToString;' + sLineBreak,
              [Canonicalize(Field.Name)]))
        else
          Unitcode.ImplementationCode.Methods.Add(Format('  Result += F%s;' + sLineBreak,
              [Canonicalize(Field.Name)]));

      Unitcode.ImplementationCode.Methods.Add('');
      Unitcode.ImplementationCode.Methods.Add('end;');

    end;

    procedure GenerateSaveToStream;
    var
      Field: TMessageField;
      CanName: AnsiString;
      FieldType: AnsiString;
      VarIsWritten: Boolean;

    begin
      Unitcode.ImplementationCode.Methods.Add(
        Format('procedure %s.SaveToStream(Stream: TProtoStreamWriter);', [GetMessageClassName(aMessage)]));
      if MessageHasRepeatedNonSimple then
      begin
        Unitcode.ImplementationCode.Methods.Add('var');
        VarIsWritten := True;
        Unitcode.ImplementationCode.Methods.Add('  BaseMessage: TBaseMessage;');
      end;
      for Field in aMessage.Fields do
        if IsAnEnumType(Field, Self.Proto, RelatedProtos) and Field.IsRepeated then
        begin
          if not VarIsWritten then
          begin
            Unitcode.ImplementationCode.Methods.Add('var');
            VarIsWritten := True;
          end;
          Unitcode.ImplementationCode.Methods.Add(Format('  %s: T%s;',  [Canonicalize(Field.Name),
            JoinStrings([GetUnitName(Field, Proto, RelatedProtos), GetNonRepeatedType4FPC(Field.FieldType)], '.')]));
        end;
      if VarIsWritten then
        Unitcode.ImplementationCode.Methods.Add('');


      Unitcode.ImplementationCode.Methods.Add('begin');

      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);

        FieldType :=  JoinStrings([GetUnitName(Field, Proto, RelatedProtos), Field.FPCType], '.');
        if CanName = 'TerritoryType' then
          WriteLn(Format('CanName: %s FieldType: %s', [CanName, FieldType]));

        if not Field.IsRepeated then
        begin
          if IsAnEnumType(Field, Proto, RelatedProtos) then
            Unitcode.ImplementationCode.Methods.Add(Format('  SaveInt32(Stream, Ord(F%s), %d);',
            [CanName, Field.FieldNumber]))
          else if IsSimpleType(Field, Self.Proto, RelatedProtos) then
          begin
              Unitcode.ImplementationCode.Methods.Add(Format('  Save%s(Stream, F%s, %d);',
                [FieldType, CanName, Field.FieldNumber]));
          end
          else if IsOneOfType(Field, Self.Proto, RelatedProtos) then
          begin
            Unitcode.ImplementationCode.Methods.Add(Format('  SaveOneOf(Stream, F%s, %d);',
              [CanName, Field.FieldNumber]));
          end
          else
          begin
            Unitcode.ImplementationCode.Methods.Add(Format('  SaveMessage(Stream, F%s, %d);',
              [CanName, Field.FieldNumber]));
          end

        end
        else // Field.IsRepeatd
        begin
          if IsAnEnumType(Field, Proto, RelatedProtos) then
          begin
            Unitcode.ImplementationCode.Methods.Add(Format('  if F%s <> nil then', [CanName]));
            Unitcode.ImplementationCode.Methods.Add(Format('    for %s in F%s do', [CanName, CanName]));
            Unitcode.ImplementationCode.Methods.Add(Format('      SaveInt32(Stream, %s, %d);',
                             [CanName, Field.FieldNumber]));
            Unitcode.ImplementationCode.Methods.Add('');
          end
          else if IsSimpleType(Field, Self.Proto, RelatedProtos) then
          begin
            FieldType :=  GetNonRepeatedType4FPC(Field.FieldType);

            case FieldType of
            'AnsiString', 'Single', 'Double', 'Int32', 'Int64', 'UInt32', 'UInt64', 'Boolean', 'Byte':
              Unitcode.ImplementationCode.Methods.Add(Format('  SaveRepeated%s(Stream, F%s, %d);',
                [FieldType, CanName, Field.FieldNumber]))
            else
              raise Exception.Create(Format('Type %s is not supported yet',
                   [JoinStrings([GetUnitName(Field, Proto, RelatedProtos),
                     Field.FPCType], '.', True)]));
            end;
          end
          else if IsOneOfType(Field, Self.Proto, RelatedProtos) then
          begin

          end
          else
          begin
            Unitcode.ImplementationCode.Methods.Add(Format('  if F%s <> nil then', [CanName]));
            Unitcode.ImplementationCode.Methods.Add(Format('    for BaseMessage in F%s do', [CanName]));
            Unitcode.ImplementationCode.Methods.Add(Format('      SaveMessage(Stream, BaseMessage, %d);',
                             [Field.FieldNumber]));
            Unitcode.ImplementationCode.Methods.Add('');
          end;
        end;

        Unitcode.ImplementationCode.Methods.Add('');
      end;

      Unitcode.ImplementationCode.Methods.Add('end;');
      Unitcode.ImplementationCode.Methods.Add('');

    end;

    procedure GenerateLoadFromStream;
    var
      Field: TMessageField;
      CanName: AnsiString;
      FieldType: AnsiString;

    begin
      Unitcode.ImplementationCode.Methods.Add(Format('function %s.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;',
        [GetMessageClassName(aMessage)]));
      Unitcode.ImplementationCode.Methods.Add('var');
      Unitcode.ImplementationCode.Methods.Add('  StartPos, FieldNumber, WireType: Integer;'+ sLineBreak);

      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  StartPos := Stream.Position;');
      Unitcode.ImplementationCode.Methods.Add('  while Stream.Position < StartPos + Len do');
      Unitcode.ImplementationCode.Methods.Add('  begin');
      Unitcode.ImplementationCode.Methods.Add('    Stream.ReadTag(FieldNumber, WireType);');

      Unitcode.ImplementationCode.Methods.Add('');
      Unitcode.ImplementationCode.Methods.Add('    case FieldNumber of');

      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);
        FieldType := GetFieldType(Field, Proto, RelatedProtos);


        if IsSimpleType(Field, Self.Proto, RelatedProtos) and not Field.IsRepeated then
        begin
          if IsAnEnumType(Field, Self.Proto, RelatedProtos) then
            Unitcode.ImplementationCode.Methods.Add(Format('    %d: F%s := %s(LoadInt32(Stream));' + sLineBreak,
              [Field.FieldNumber, CanName, FieldType]))
          else
            Unitcode.ImplementationCode.Methods.Add(Format('    %d: F%s := Load%s(Stream);' + sLineBreak,
              [Field.FieldNumber, CanName, FieldType]));
        end
        else if IsSimpleType(Field, Self.Proto, RelatedProtos) and Field.IsRepeated then
        begin
          CanName := Canonicalize(Field.Name);
          FieldType :=  GetNonRepeatedType4FPC(Field.FieldType);

          case FieldType of
          'AnsiString', 'Single', 'Double', 'Int32', 'Int64', 'UInt32', 'UInt64', 'Boolean', 'Byte':
          begin
            Unitcode.ImplementationCode.Methods.Add(Format('    %d:', [Field.FieldNumber]));
            Unitcode.ImplementationCode.Methods.Add('    begin' + sLineBreak);
            Unitcode.ImplementationCode.Methods.Add('      if WireType <> 2 then' + sLineBreak +
                                    '        Exit(False);');
            Unitcode.ImplementationCode.Methods.Add(Format('      LoadRepeated%s(Stream, GetOrCreateAll%s);',
                 [FieldType, CanName]));
            Unitcode.ImplementationCode.Methods.Add('    end;' + sLineBreak);
          end
          else
             raise Exception.Create(Format('Type %s is not supported yet',
                 [GetFieldType(Field, Proto, RelatedProtos)]));
          end;
        end
        else if not IsSimpleType(Field, Self.Proto, RelatedProtos) and not Field.IsRepeated then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('    %d:', [Field.FieldNumber]));
          Unitcode.ImplementationCode.Methods.Add       ('    begin');
          Unitcode.ImplementationCode.Methods.Add       ('      if WireType <> 2 then' + sLineBreak +
                                                         '        Exit(False);');
          Unitcode.ImplementationCode.Methods.Add(Format('      F%s := %s.Create;', [CanName, FieldType]));
          Unitcode.ImplementationCode.Methods.Add(Format('      if not LoadMessage(Stream, F%s) then' + sLineBreak +
                                                         '        Exit(False);', [CanName]));
          Unitcode.ImplementationCode.Methods.Add(       '    end;' + sLineBreak);
        end
        else if not IsSimpleType(Field, Self.Proto, RelatedProtos) and Field.IsRepeated then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('    %d:', [Field.FieldNumber]));
          Unitcode.ImplementationCode.Methods.Add       ('    begin');
          Unitcode.ImplementationCode.Methods.Add       ('      if WireType <> 2 then' + sLineBreak +
                                                         '        Exit(False);');
          Unitcode.ImplementationCode.Methods.Add(Format('      GetOrCreateAll%s.Add(%s.Create);', [CanName, GetNonRepeatedType4FPC(Field.FieldType)]));
          Unitcode.ImplementationCode.Methods.Add(Format('      if not LoadMessage(Stream, F%s.Last) then' + sLineBreak +
                                                         '        Exit(False);', [CanName]));
          Unitcode.ImplementationCode.Methods.Add       ('    end;' + sLineBreak);
          Unitcode.ImplementationCode.Methods.Add('');
        end;

        Unitcode.ImplementationCode.Methods.Add('');
      end;
      Unitcode.ImplementationCode.Methods.Add('    end;');
      Unitcode.ImplementationCode.Methods.Add('  end;' + sLineBreak);

      Unitcode.ImplementationCode.Methods.Add('  Result := StartPos + Len = Stream.Position;');
      Unitcode.ImplementationCode.Methods.Add('end;');

    end;

    begin
      GenerateConstructors;
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateDestructor;
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateToString;
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateSaveToStream;
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateLoadFromStream;
  end;

begin
  if AMessage.Name = 'Geo' then
    DebugLn(AMessage.Name);
  GenerateDeclarationForMessage(AMessage, Unitcode);
  GenerateImplementationForMessage(AMessage, Unitcode);

end;

procedure TPBCodeGeneratorV1.GenerateCodeForMessageField(
  const aField: TMessageField; MessageClassName: AnsiString; out
  UnitCode: TUnitCode; const Indent: AnsiString);

  function ApplyPattern(MessageClassName: AnsiString;
    const Template: AnsiString): AnsiString;
  begin
    Result := Template;
    Result := StringReplace(Result, '[[Field.Type]]', aField.FieldType, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.FPCType]]', aField.FPCType, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.UnitNameAndFPCType]]',
      GetFieldType(aField, Proto, RelatedProtos), [rfReplaceAll]);
    if aField.PackageName <> '' then
    begin
      Result := StringReplace(Result, '[[Field.UnitNameWithDot]]',
        GetUnitName(aField, Proto, RelatedProtos) + '.', [rfReplaceAll]);
        Result := StringReplace(Result, '[[Field.PackageNameWithDot]]',
          aField.PackageName + '.', [rfReplaceAll])
    end
    else
    begin
      Result := StringReplace(Result, '[[Field.UnitNameWithDot]]', '', [rfReplaceAll]);
      Result := StringReplace(Result, '[[Field.PackageNameWithDot]]', '', [rfReplaceAll])
    end;

    Result := StringReplace(Result, '[[Field.Name]]', aField.Name, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.Number]]', IntToStr(aField.FieldNumber), [rfReplaceAll]);
    Result := StringReplace(Result, '[[CanName]]', Canonicalize(aField.Name), [rfReplaceAll]);
    Result := StringReplace(Result, '[[FormatString]]', FormatString(aField.FieldType), [rfReplaceAll]);
    Result := StringReplace(Result, '[[ClassName]]', MessageClassName, [rfReplaceAll]);
    if aField.IsRepeated then
    begin

      Result := StringReplace(Result, '[[Field.InnerFPCType]]', GetNonRepeatedType4FPC(aField.FieldType), [rfReplaceAll]);
    end;

  end;

  {$i NonRepeatedSimpleFieldTemplate.inc}
  {$i NonRepeatedNonSimpleFieldTemplate.inc}
  {$i RepeatedNonSimpleFieldTemplate.inc}
  {$i RepeatedSimpleFieldTemplate.inc}


  procedure GenerateDeclaration;
  begin
    if not aField.IsRepeated and IsSimpleType(aField, Self.Proto, RelatedProtos) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareNonRepeatedSimpleFieldTemplate))
    else if not aField.IsRepeated and not IsSimpleType(aField, Self.Proto, RelatedProtos) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareNonRepeatedNonSimpleFieldTemplate))
    else if aField.IsRepeated and not IsSimpleType(aField, Self.Proto, RelatedProtos) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareRepeatedNonSimpleFieldTemplate))
    else
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareRepeatedSimpleFieldTemplate));
  end;

  procedure GenerateImplementation;
  begin
    if not aField.IsRepeated then
    else if not IsSimpleType(aField, Self.Proto, RelatedProtos) then
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(MessageClassName, ImplementRepeatedNonSimpleFieldTemplate))
    else if aField.FPCType = 'Boolean' then
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(MessageClassName, ImplementRepeatedBooleanTemplate))
    else
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(MessageClassName, ImplementRepeatedSimpleFieldTemplate));

  end;

begin
  if aField.Name = 'territory_type' then
   WriteLn(aField.Name);
  GenerateDeclaration;
  GenerateImplementation;
end;

procedure TPBCodeGeneratorV1.GenerateCodeForOneOf(const OneOf: TOneOf; out
  Unitcode: TUnitCode; const Indent: AnsiString);

  procedure GenerateDeclaration;
  var
    Field: TOneOfField;

  begin
    Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = Class(TBaseOneOf)', [Indent,
      GetFieldType(OneOf, Proto, RelatedProtos)]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sprivate', [Indent]));

    for Field in OneOf.Fields do
      Unitcode.InterfaceCode.TypeList.Add(Format('%sF%s: %s;', [Indent + '  ', Canonicalize(Field.Name), Field.FPCType]));
    Unitcode.InterfaceCode.TypeList.Add('');
    for Field in OneOf.Fields do
    begin
      Unitcode.InterfaceCode.TypeList.Add(Format('%sfunction Get%s: %s;',
      [Indent + '  ', Canonicalize(Field.Name), Field.FPCType]));
      Unitcode.InterfaceCode.TypeList.Add(Format('%sprocedure Set%s(_%s: %s);',
      [Indent + '  ', Canonicalize(Field.Name), Canonicalize(Field.Name),
        Field.FPCType]));
    end;
    Unitcode.InterfaceCode.TypeList.Add('  public');

    for Field in OneOf.Fields do
      Unitcode.InterfaceCode.TypeList.Add(Format('%sproperty %s: %s read Get%s write Set%s;',
        [Indent + '  ', Canonicalize(Field.Name), Field.FPCType,
        Canonicalize(Field.Name), Canonicalize(Field.Name)]));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add('%sconstructor Create;', [Indent + '  ']);

    Unitcode.InterfaceCode.TypeList.Add(Format('%send;', [Indent]));
  end;

  procedure GenerateImplementation;
  var
    Field: TOneOfField;

  begin
    DebugLn('Not Implemented Yet!');
  end;


begin
  GenerateDeclaration;
  GenerateImplementation;
end;


procedure TPBCodeGeneratorV1.GenerateCode;
var
  Enum: TEnum;
  Message: TMessage;
  UnitCode: TUnitCode;
  Code: AnsiString;
//  Output

begin
  UnitCode := TUnitCode.Create(GetUnitName(Proto.InputProtoFilename));

  GenerateCodeForImports(Proto.Imports, UnitCode, '', '');

  for Enum in Proto.Enums do
    GenerateCodeForEnum(Enum, UnitCode, '  ');

  for Message in Proto.Messages do
    UnitCode.InterfaceCode.TypeList.Add('%s%s = class;', ['  ', Message.FPCType]);

  UnitCode.InterfaceCode.TypeList.Add('');

  for Message in Proto.Messages do
    GenerateCodeForMessage(Message, UnitCode, '  ');

  OutputStream := TFileStream.Create(ConcatPaths([ExtractFileDir(Proto.InputProtoFilename),
    GetUnitName(Proto.InputProtoFilename) + '.pp']), fmCreate);
  Code := UnitCode.ToString;
  OutputStream.Write(Code[1], Length(Code));

  OutputStream.Free;
  UnitCode.Free;

end;

constructor TPBCodeGeneratorV1.Create(_Proto: TProto; _RelatedProtos: TProtos);
begin
  inherited Create;

  Proto := _Proto;
  RelatedProtos := _RelatedProtos;

end;

destructor TPBCodeGeneratorV1.Destroy;
begin

  inherited Destroy;
end;

{ TPBBaseCodeGenerator }

class function TPBBaseCodeGenerator.GetCodeGenerator(Proto: TProto;
  RelatedProtos: TProtos): TPBBaseCodeGenerator;
begin
  Result := TPBCodeGeneratorV1.Create(Proto, RelatedProtos);

end;

destructor TPBBaseCodeGenerator.Destroy;
begin
  inherited;

end;


end.

