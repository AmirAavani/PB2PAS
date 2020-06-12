unit PBCodeGeneratorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PBDefinitionUnit;

type

  { TPBBaseCodeGenerator }

  TPBBaseCodeGenerator = class(TObject)
  public
    class function GetCodeGenerator(Proto: TProto): TPBBaseCodeGenerator;

    procedure GenerateCode; virtual; abstract;
    destructor Destroy; override;
  end;

implementation
uses
  UtilsUnit, StreamUnit, StringUnit, strutils;

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
    OutputStream: TStream;

  protected
    procedure GenerateCodeForImports(const Imports: TImports; out UnitCode: TUnitCode; const Prefix, Indent: AnsiString);
    procedure GenerateCodeForEnum(const AnEnum: TEnum; out Unitcode: TUnitCode; const Prefix, Indent: AnsiString);
    procedure GenerateCodeForMessage(const AMessage: TMessage; out Unitcode: TUnitCode; const Prefix, Indent: AnsiString);
    procedure GenerateCodeForMessageField(const aField: TMessageField; MessageClassName: AnsiString; out UnitCode: TUnitCode; const Prefix, Indent: AnsiString);
    procedure GenerateCodeForOneOf(const OneOf: TOneOf; out Unitcode: TUnitCode; const Prefix, Indent: AnsiString);

  public
    procedure GenerateCode; override;

    constructor Create(_Proto: TProto);
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
  Unitcode: TUnitCode; const Prefix, Indent: AnsiString);
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
    WriteStr(Format('%s%s_%s = %d', [Indent + '  ', Canonicalize(AnEnum.Name),
      Canonicalize(EnumField.Name), EnumField.Value]),
      Code);
    if i <> AnEnum.Count - 1 then
      WriteLineStr(',', Code)
    else
      WriteLineStr('', Code);
  end;
  WriteLineStr(Format('%s);', [Indent]), Code);

  Unitcode.InterfaceCode.TypeList.Add(Code);
end;

procedure TPBCodeGeneratorV1.GenerateCodeForMessage(const AMessage: TMessage;
  out Unitcode: TUnitCode; const Prefix, Indent: AnsiString);

  function GetMessageClassName: AnsiString;
  begin
    Result := Format('%sT%s', [Prefix, Canonicalize(aMessage.Name)]);
  end;

  procedure GenerateDeclarationForMessage(
    const aMessage: TMessage; out Unitcode: TUnitCode);
  var
    i: Integer;
    Field: TMessageField;
    Enum: TEnum;
    S: AnsiString;
    Option: TOption;
    MessageClassName: AnsiString;
    HasOneOf: Boolean;

  begin
    MessageClassName := Format('T%s', [Canonicalize(aMessage.Name)]);
    Unitcode.InterfaceCode.TypeList.Add(Format('%s{ %s }', [Indent, MessageClassName]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = class(TBaseMessage)', [Indent, MessageClassName]));
    for Option in aMessage.Options do
      Unitcode.InterfaceCode.TypeList.Add(Format('%s// %s = %s', [Indent + '  ', Option.OptionName, Option.ConstValue]));

    Unitcode.InterfaceCode.TypeList.Add('%spublic type', [Indent]);

    for Enum in aMessage.Enums do
      Unitcode.InterfaceCode.TypeList.Add('% ', [Indent]);

    if aMessage.Enums.Count <> 0 then
      Unitcode.InterfaceCode.TypeList.Add('');

    HasOneOf := False;
    for Field in aMessage.Fields do
      if Field.ClassType = TOneOf then
      begin
        GenerateCodeForOneOf(Field as TOneOf, Unitcode, Prefix, Indent);
        HasOneOf := True;
        Break;

      end;
    if HasOneOf then
      Unitcode.InterfaceCode.TypeList.Add('');

    for i := 0 to aMessage.Messages.Count - 1 do
    begin
      Unitcode.InterfaceCode.TypeList.Add('%spublic type', [Indent]);
      GenerateCodeForMessage(aMessage.Messages[i], Unitcode, Prefix, Indent + '  ');

    end;

    Unitcode.InterfaceCode.TypeList.Add('%spublic', [Indent]);
    for Field in aMessage.Fields do
      GenerateCodeForMessageField(Field, MessageClassName, Unitcode, Prefix, Indent + '  ');

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
          Field.FPCType]);
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
      CanName: AnsiString;
      CreateDeclaration: AnsiString;
      MessageClassName: AnsiString;

    begin
      MessageClassName := Format('%sT%s', [Prefix, Canonicalize(aMessage.Name)]);

      Unitcode.ImplementationCode.Methods.Add(Format('constructor %s.Create;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  inherited Create;');
      Unitcode.ImplementationCode.Methods.Add(sLineBreak);
      Unitcode.ImplementationCode.Methods.Add(sLineBreak);

      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);
        if Field.DefaultValue <> '' then
          Unitcode.ImplementationCode.Methods.Add(Format('  F%s := %s;', [CanName, Field.DefaultValue]));
      end;

      Unitcode.ImplementationCode.Methods.Add(sLineBreak + 'end;' + sLineBreak);

      if aMessage.Fields.Count <> 0 then
      begin
        CreateDeclaration := 'Create(';
        for Field in aMessage.Fields do
          CreateDeclaration += Format('a%s: %s; ', [Canonicalize(Field.Name),
            Field.FPCType]);
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
      MessageClassName := Format('%sT%s', [Prefix, Canonicalize(aMessage.Name)]);
      Unitcode.ImplementationCode.Methods.Add(Format('destructor %s.Destroy;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);
        if not IsSimpleType(Field.FieldType) or Field.IsRepeated then
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
        if Field.IsRepeated and not IsSimpleType(Field.FieldType) then
          Exit(True);

    end;

    procedure GenerateToString;
    var
      Field: TMessageField;

    begin
      Unitcode.ImplementationCode.Methods.Add(Format('function %s.ToString: AnsiString;', [GetMessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  Result := '''';');
      Unitcode.ImplementationCode.Methods.Add('');

      for Field in aMessage.Fields do
        if Field.FPCType <> 'AnsiString' then
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

    begin
      Unitcode.ImplementationCode.Methods.Add(
        Format('procedure %s.SaveToStream(Stream: TProtoStreamWriter);', [GetMessageClassName]));
      if MessageHasRepeatedNonSimple then
      begin
        Unitcode.ImplementationCode.Methods.Add('var');
        Unitcode.ImplementationCode.Methods.Add('  BaseMessage: TBaseMessage;');
        Unitcode.ImplementationCode.Methods.Add('');

      end;

      Unitcode.ImplementationCode.Methods.Add('begin');

      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);
        FieldType :=  Field.FPCType;

        if IsSimpleType(Field.FieldType) and not Field.IsRepeated then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('  Save%s(Stream, F%s, %d);',
            [FieldType, CanName, Field.FieldNumber]));
        end
        else if IsSimpleType(Field.FieldType) and Field.IsRepeated then
        begin
          FieldType :=  GetNonRepeatedType4FPC(Field.FieldType);

          case FieldType of
          'AnsiString', 'Single', 'Double', 'Int32', 'Int64', 'UInt32', 'UInt64', 'Boolean', 'Byte':
            Unitcode.ImplementationCode.Methods.Add(Format('  SaveRepeated%s(Stream, F%s, %d);',
              [FieldType, CanName, Field.FieldNumber]))
          else
             raise Exception.Create(Format('Type %s is not supported yet',
                 [Field.FPCType]));
          end;
        end
        else if not IsSimpleType(Field.FieldType) and not Field.IsRepeated then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('  SaveMessage(Stream, F%s, %d);',
            [CanName, Field.FieldNumber]));
        end
        else if not IsSimpleType(Field.FieldType) and Field.IsRepeated then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('  if F%s <> nil then', [CanName]));
          Unitcode.ImplementationCode.Methods.Add(Format('    for BaseMessage in F%s do', [CanName]));
          Unitcode.ImplementationCode.Methods.Add(Format('      SaveMessage(Stream, BaseMessage, %d);',
                           [Field.FieldNumber]));
          Unitcode.ImplementationCode.Methods.Add('');
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
      Unitcode.ImplementationCode.Methods.Add(Format('function %s.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;', [GetMessageClassName]));
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
        FieldType :=  Field.FPCType;


        if IsSimpleType(Field.FieldType) and not Field.IsRepeated then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('    %d: F%s := Load%s(Stream);' + sLineBreak,
            [Field.FieldNumber, CanName, FieldType]));
        end
        else if IsSimpleType(Field.FieldType) and Field.IsRepeated then
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
                 [Field.FPCType]));
          end;
        end
        else if not IsSimpleType(Field.FieldType) and not Field.IsRepeated then
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
        else if not IsSimpleType(Field.FieldType) and Field.IsRepeated then
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
  GenerateDeclarationForMessage(AMessage, Unitcode);
  GenerateImplementationForMessage(AMessage, Unitcode);

end;

procedure TPBCodeGeneratorV1.GenerateCodeForMessageField(
  const aField: TMessageField; MessageClassName: AnsiString; out
  UnitCode: TUnitCode; const Prefix, Indent: AnsiString);

  function ApplyPattern(MessageClassName: AnsiString;
    const Template: AnsiString): AnsiString;
  begin
    Result := Template;
    Result := StringReplace(Result, '[[Field.Type]]', aField.FieldType, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.FPCType]]', aField.FPCType, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.Name]]', aField.Name, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.Number]]', IntToStr(aField.FieldNumber), [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.DefaultValue]]', aField.DefaultValue, [rfReplaceAll]);
    Result := StringReplace(Result, '[[CanName]]', Canonicalize(aField.Name), [rfReplaceAll]);
    Result := StringReplace(Result, '[[FormatString]]', FormatString(aField.FieldType), [rfReplaceAll]);
    Result := StringReplace(Result, '[[ClassName]]', MessageClassName, [rfReplaceAll]);
    if aField.IsRepeated then
      Result := StringReplace(Result, '[[Field.InnerFPCType]]', GetNonRepeatedType4FPC(aField.FieldType), [rfReplaceAll]);

  end;

  {$i NonRepeatedSimpleFieldTemplate.inc}
  {$i NonRepeatedNonSimpleFieldTemplate.inc}
  {$i RepeatedNonSimpleFieldTemplate.inc}
  {$i RepeatedSimpleFieldTemplate.inc}


  procedure GenerateDeclaration;
  begin
    if not aField.IsRepeated and IsSimpleType(aField.FieldType) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareNonRepeatedSimpleFieldTemplate))
    else if not aField.IsRepeated and not IsSimpleType(aField.FieldType) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareNonRepeatedNonSimpleFieldTemplate))
    else if aField.IsRepeated and not IsSimpleType(aField.FieldType) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareRepeatedNonSimpleFieldTemplate))
    else
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(MessageClassName, DeclareRepeatedSimpleFieldTemplate));
  end;

  procedure GenerateImplementation;
  begin
    if not aField.IsRepeated then
    else if not IsSimpleType(aField.FieldType) then
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(MessageClassName, ImplementRepeatedNonSimpleFieldTemplate))
    else if aField.FPCType = 'Boolean' then
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(MessageClassName, ImplementRepeatedBooleanTemplate))
    else
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(MessageClassName, ImplementRepeatedSimpleFieldTemplate));

  end;


begin
  GenerateDeclaration;
  GenerateImplementation;
end;

procedure TPBCodeGeneratorV1.GenerateCodeForOneOf(const OneOf: TOneOf; out
  Unitcode: TUnitCode; const Prefix, Indent: AnsiString);

  procedure GenerateDeclaration;
  var
    Field: TOneOfField;

  begin
    Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = Class(TBaseOneOf)', [Indent, OneOf.FPCType]));
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
    Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = Class(TBaseOneOf)', [Indent, OneOf.FPCType]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sprivate', [Indent]));

    for Field in OneOf.Fields do
      Unitcode.InterfaceCode.TypeList.Add(Format('%sF%s: %s;', [Indent + '  ', Canonicalize(Field.Name), Field.FPCType]));
    Unitcode.InterfaceCode.TypeList.Add('');
    for Field in OneOf.Fields do
      Unitcode.InterfaceCode.TypeList.Add(Format('%sprocedure Set%s(_%s: %s);',
      [Indent + '  ', Canonicalize(Field.Name), Canonicalize(Field.Name),
        Field.FPCType]));
    Unitcode.InterfaceCode.TypeList.Add('  public');

    for Field in OneOf.Fields do
      Unitcode.InterfaceCode.TypeList.Add(Format('%sproperty %s: %s read F%s write Set%s;',
        [Indent + '  ', Canonicalize(Field.Name), Field.FPCType,
        Canonicalize(Field.Name), Canonicalize(Field.Name)]));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add('%sconstructor Create;', [Indent + '  ']);

    Unitcode.InterfaceCode.TypeList.Add(Format('%send;', [Indent]));
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
    GenerateCodeForEnum(Enum, UnitCode, '', '  ');

  for Message in Proto.Messages do
    UnitCode.InterfaceCode.TypeList.Add('%s%s = class;', ['  ', Message.FPCType]);

  UnitCode.InterfaceCode.TypeList.Add('');

  for Message in Proto.Messages do
    GenerateCodeForMessage(Message, UnitCode, '', '  ');
//  Halt(2);
  {
  for Message in Proto.Messages do
  begin
    Output.WriteLine(GenerateDeclarationForMessage(Message));
    Output.WriteLine;
  end;

  Output.WriteLine(sLineBreak + 'implementation' + sLineBreak);
  Output.WriteLine('uses strutils;');

  for Message in Proto.Messages do
  begin
    Output.WriteLine(sLineBreak + Format(' { T%s }', [Canonicalize(Message.Name)]) +
      sLineBreak);
    GenerateImplementationForMessage(Message);
    Output.WriteLine;
  end;

  Output.WriteLine('end.');

  Output.Free;
 }
   OutputStream := TFileStream.Create(ConcatPaths([ExtractFileDir(Proto.InputProtoFilename),
    GetUnitName(Proto.InputProtoFilename) + '.pp']), fmCreate);
  Code := UnitCode.ToString;
  OutputStream.Write(Code[1], Length(Code));

  OutputStream.Free;
  UnitCode.Free;

end;

constructor TPBCodeGeneratorV1.Create(_Proto: TProto);
begin
  inherited Create;

  Proto := _Proto;

end;

destructor TPBCodeGeneratorV1.Destroy;
begin

  inherited Destroy;
end;

{ TPBBaseCodeGenerator }

class function TPBBaseCodeGenerator.GetCodeGenerator(Proto: TProto
  ): TPBBaseCodeGenerator;
begin
  Result := TPBCodeGeneratorV1.Create(Proto);
end;

destructor TPBBaseCodeGenerator.Destroy;
begin
  inherited;

end;


end.

