unit PBCodeGeneratorUnit;

{$mode objfpc}{$H+}

interface

uses
  PBDefinitionUnit, UtilsUnit, Classes, SysUtils;

type
  { TPBBaseCodeGenerator }

  TPBBaseCodeGenerator = class(TObject)
  protected
    class function GetCodeGenerator(Proto: TProto; RelatedProtos: TProtos): TPBBaseCodeGenerator;
    procedure GenerateCode; virtual; abstract;

  public
    class procedure GenerateCode(ProtoMap: TProtoMap);

    destructor Destroy; override;
  end;

  EInvalidSyntax = class(Exception);

implementation
uses
  StringUnit, ALoggerUnit, PBOptionUnit, strutils, Generics.Defaults;

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
  private type

    { TContext }

    TContext = record
      Message: TMessage;
      Proto: TProto;

    end;

  private
    Proto: TProto;
    RelatedProtos: TProtos;
    OutputStream: TStream;

    function IsSimpleType(PBType: TPBBaseType): Boolean;
    function IsAnEnumType(PBType: TPBBaseType): Boolean;
  protected
    procedure GenerateCodeForImports(const Imports: TImports; UnitCode: TUnitCode; const Prefix, Indent: AnsiString);
    procedure GenerateCodeForEnum(const AnEnum: TEnum; Unitcode: TUnitCode;
      const Context: TContext; const Indent: AnsiString);
    procedure GenerateCodeForMessage(const AMessage: TMessage; Unitcode: TUnitCode;
      const Indent: AnsiString);
    procedure GenerateCodeForMessageField(const aField: TMessageField;
      UnitCode: TUnitCode; const Indent: AnsiString);
    procedure GenerateCodeForOneOf(const OneOf: TOneOf; Unitcode: TUnitCode;
      const Context: TContext; const Indent: AnsiString);
    procedure GenerateCodeForMap(const Map: TMap; Unitcode: TUnitCode;
      const Indent: AnsiString);

    function GetFPCType(PBType: TPBBaseType; Context: TContext): AnsiString;
    procedure GenerateCode; override;

  public
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

function CreateContext(Msg: TMessage): TPBCodeGeneratorV1.TContext;
begin
  Result.Message := Msg;

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

function TPBCodeGeneratorV1.IsSimpleType(PBType: TPBBaseType): Boolean;
begin
  case PBType.FullName of
    'double' , 'float' , 'int32' , 'int64' , 'uint32' , 'uint64'
      , 'sint32' , 'sint64' , 'fixed32' , 'fixed64' , 'sfixed32' , 'sfixed64'
      , 'bool' , 'string', 'byte':
      Exit(True);
  end;

  Result := IsAnEnumType(PBType);

end;

function TPBCodeGeneratorV1.IsAnEnumType(PBType: TPBBaseType): Boolean;
var
  ParentInfo: TParent;
  P: TProto;

begin
  case PBType.Name of
    'double' , 'float' , 'int32' , 'int64' , 'uint32' , 'uint64'
      , 'sint32' , 'sint64' , 'fixed32' , 'fixed64' , 'sfixed32' , 'sfixed64'
      , 'bool' , 'string' , 'byte', 'bytes': Exit(False);
  end;

  if PBType.PackageName = '' then
  begin
    ParentInfo := PBType.Parent;
    while (ParentInfo.Message <> nil) or (ParentInfo.Proto <> nil) or (ParentInfo.OneOf <> nil) do
    begin
      if (ParentInfo.Message <> nil) and (ParentInfo.Message.Enums <> nil) then
        if ParentInfo.Message.Enums.ByName[PBType.Name] <> nil then
          Exit(True);
      if (ParentInfo.Proto <> nil) and (ParentInfo.Proto.Enums <> nil) then
        if ParentInfo.Proto.Enums.ByName[PBType.Name] <> nil then
          Exit(True);

      if ParentInfo.Message <> nil then
        ParentInfo := ParentInfo.Message.Parent
      else if ParentInfo.OneOf <> nil then
        ParentInfo := ParentInfo.OneOf.OneOfFieldPBType.Parent
      else
        Break;

    end;

    if (Proto.Enums <> nil) and (Proto.Enums.ByName[PBType.Name] <> nil) then
      Exit(True);

    Exit(False);
  end;

  for p in RelatedProtos do
  begin
    if p.PackageName <> PBType.PackageName then
      Continue;
    if p.Enums <> nil then
      Exit(p.Enums.ByName[PBType.Name] <> nil);
  end;

  Result := False;

end;

procedure TPBCodeGeneratorV1.GenerateCodeForImports(const Imports: TImports;
  UnitCode: TUnitCode; const Prefix, Indent: AnsiString);
const
  DefaultUsesList : array of AnsiString = (
    'classes',
    'fgl',
    'sysutils',
    'ProtoHelperUnit',
    'ProtoHelperListsUnit',
    'ProtoStreamUnit',
    'GenericCollectionUnit'
  );
var
  S: AnsiString;
  Import: AnsiString;

begin
  for Import in Imports do
    UnitCode.InterfaceCode.UsesList.Add(GetUnitName(Import));

  for s in DefaultUsesList do
    UnitCode.InterfaceCode.UsesList.Add(s);

end;

function CompareEnumFields(constref a, b: TEnumField): Integer;
begin
  Result := a.Value - b.Value;

end;

procedure TPBCodeGeneratorV1.GenerateCodeForEnum(const AnEnum: TEnum;
  Unitcode: TUnitCode; const Context: TContext; const Indent: AnsiString);
var
  EnumField: TEnumField;
  i: Integer;
  Code: TStringList;

begin
  Unitcode.InterfaceCode.TypeList.Add(Format('%s// %s', [Indent, AnEnum.Name]));
  Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = (', [Indent, GetFPCType(AnEnum, Context)]));

  Code := TStringList.Create;
  AnEnum.AllFields.Sort(specialize TComparer<TEnumField>.Construct(@CompareEnumFields));
  for i := 0 to AnEnum.AllFields.Count - 1 do
  begin
    EnumField := AnEnum.AllFields[i];
    Code.Add(Format('%s%s_%s = %d', [Indent + '  ', UpperCase(AnEnum.Name),
      EnumField.Name, EnumField.Value]));
  end;
  Unitcode.InterfaceCode.TypeList.Add(JoinStrings(Code, ',' + sLineBreak));
  Unitcode.InterfaceCode.TypeList.Add(Format('%s);', [Indent]));

  Code.Free;
end;

procedure TPBCodeGeneratorV1.GenerateCodeForMessage(const AMessage: TMessage;
  Unitcode: TUnitCode; const Indent: AnsiString);

  procedure GenerateDeclarationForMessage(
    const aMessage: TMessage; Unitcode: TUnitCode);
  var
    i: Integer;
    Field: TMessageField;
    Enum: TEnum;
    Option: TOption;
    MessageClassName: AnsiString;
    AllMaps: TStringList;

  begin
    MessageClassName := GetFPCType(aMessage.MessageType,
      CreateContext(aMessage.Parent.Message));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s{ T%s }', [Indent, aMessage.Name]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = class(TBaseMessage)',
      [Indent, MessageClassName]));
    for Option in aMessage.Options do
      Unitcode.InterfaceCode.TypeList.Add(Format('%s// %s = %s', [
      Indent + '  ', Option.OptionName, Option.ConstValue]));

    for Enum in aMessage.Enums do
    begin
      Unitcode.InterfaceCode.TypeList.Add('%stype', [Indent]);
      GenerateCodeForEnum(Enum, Unitcode, CreateContext(aMessage), Indent + '  ');

    end;

    if aMessage.Enums.Count <> 0 then
      Unitcode.InterfaceCode.TypeList.Add('');

    // Forward Declarations:
    // TODO(Amir): This can be improved.
    Unitcode.InterfaceCode.TypeList.Add(Format('%s// Forward Declarations.', [Indent]));
    for Field in aMessage.Fields do
      if Field.ClassType = TOneOf then
      begin
        Unitcode.InterfaceCode.TypeList.Add(Format('%spublic type', [Indent]));
        UnitCode.InterfaceCode.TypeList.Add(
         Format('%s  %s = class;', [Indent, GetFpcType(Field.FieldType,
         CreateContext(aMessage))]));

      end;

    if aMessage.Messages.Count <> 0 then
      Unitcode.InterfaceCode.TypeList.Add('%spublic type', [Indent]);

    for i := 0 to aMessage.Messages.Count - 1 do
      Unitcode.InterfaceCode.TypeList.Add('%s  %s = class;', [Indent,
        GetFPCType(aMessage.Messages[i].MessageType,
        CreateContext(aMessage))]);
     UnitCode.InterfaceCode.TypeList.Add('');

    for Field in aMessage.Fields do
      if Field.ClassType = TOneOf then
      begin
        Unitcode.InterfaceCode.TypeList.Add(Format('%spublic type', [Indent]));
        GenerateCodeForOneOf(Field as TOneOf,
        Unitcode, CreateContext(aMessage),
        Indent + '  ');
        Unitcode.InterfaceCode.TypeList.Add('');

      end;

    for i := 0 to aMessage.Messages.Count - 1 do
    begin
      Unitcode.InterfaceCode.TypeList.Add('%spublic type', [Indent]);
      GenerateCodeForMessage(aMessage.Messages[i], Unitcode,
        Indent + '  ');

    end;


    AllMaps := TStringList.Create;
    for Field in aMessage.Fields do
      if Field.ClassType = TMap then
      begin
        if 0 <= AllMaps.IndexOf(GetFPCType(Field.FieldType,
          CreateContext(aMessage))) then
          Continue;
        AllMaps.Add(GetFPCType(Field.FieldType, CreateContext(aMessage)));

        Unitcode.InterfaceCode.TypeList.Add(Format('%spublic type', [Indent]));
        GenerateCodeForMap(
          Field as TMap, Unitcode,
          Indent + '  ');
        Unitcode.InterfaceCode.TypeList.Add('');

      end;

    for Field in aMessage.Fields do
      GenerateCodeForMessageField(Field,
          Unitcode,
          Indent + '  ');

    Unitcode.InterfaceCode.TypeList.Add(Format('%sprotected ', [Indent]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  procedure SaveToStream(Stream: TProtoStreamWriter); override;',
      [Indent]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;',
    [Indent]));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add('%spublic', [Indent]);
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  constructor Create;', [Indent]));

    Unitcode.InterfaceCode.TypeList.Add(Format('%sdestructor Destroy; override;', [Indent + '  ']));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sprocedure Clear; override;', [Indent + '  ']));
    // Unitcode.InterfaceCode.TypeList.Add(Format('%sfunction ToString: AnsiString; override;', [Indent + '  ']));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add(Format('%send;', [Indent]));
    Unitcode.InterfaceCode.TypeList.Add('');

  end;

  procedure GenerateImplementationForMessage(const aMessage: TMessage; Unitcode: TUnitCode);

    procedure GenerateConstructors;
    var
      MessageClassName: AnsiString;

    begin
      MessageClassName := GetFPCType(aMessage.MessageType, CreateContext(nil));

      Unitcode.ImplementationCode.Methods.Add(Format('constructor %s.Create;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  inherited Create;');
      Unitcode.ImplementationCode.Methods.Add(sLineBreak);
      Unitcode.ImplementationCode.Methods.Add('end;' + sLineBreak);

    end;

    procedure GenerateDestructor;
    var
      MessageClassName: AnsiString;

    begin
      MessageClassName := GetFPCType(aMessage.MessageType, CreateContext(nil));

      Unitcode.ImplementationCode.Methods.Add(Format('destructor %s.Destroy;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      Unitcode.ImplementationCode.Methods.Add('  Self.Clear;');
      Unitcode.ImplementationCode.Methods.Add('');
      Unitcode.ImplementationCode.Methods.Add('  inherited;');
      Unitcode.ImplementationCode.Methods.Add('end;');

    end;

    procedure GenerateClear;
    var
      Field: TMessageField;
      CanName: AnsiString;
      MessageClassName: AnsiString;

    begin
      MessageClassName := GetFPCType(aMessage.MessageType, CreateContext(nil));

      Unitcode.ImplementationCode.Methods.Add(Format('procedure %s.Clear;', [MessageClassName]));
      Unitcode.ImplementationCode.Methods.Add('begin');
      for Field in aMessage.Fields do
      begin
        CanName := Canonicalize(Field.Name);
        if not IsSimpleType(Field.FieldType) or
          Field.FieldType.IsRepeated then
          Unitcode.ImplementationCode.Methods.Add(Format('  F%s.Free;', [CanName]));
      end;
      Unitcode.ImplementationCode.Methods.Add('');
      Unitcode.ImplementationCode.Methods.Add('  inherited;');
      Unitcode.ImplementationCode.Methods.Add('end;');

    end;

    procedure GenerateSaveToStream(Indent: AnsiString);
      procedure GenerateForOneOf(OneOf: TOneOf; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForSimpleType(Field: TMessageField; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForEnum(Field: TMessageField; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForMessage(Field: TMessageField; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForMap(aMap: TMap; CanName, FieldType: AnsiString; Indent: AnsiString); forward;

      procedure GenerateForField(Field: TMessageField; Indent: AnsiString);
      var
        CanName: AnsiString;
        FieldType: AnsiString;

      begin
        CanName := Field._CanonicalizeFullName;
        FieldType :=  JoinStrings([GetUnitName(Field, Proto, RelatedProtos),
          GetFPCType(Field.FieldType, CreateContext(Field.FieldType.Parent.Message))], '.');

        if not Field.FieldType.IsRepeated then
        begin
          if IsAnEnumType(Field.FieldType) then
            GenerateForEnum(Field, CanName, FieldType, Indent)
          else if IsSimpleType(Field.FieldType) then
            GenerateForSimpleType(Field, CanName, FieldType, Indent)
          else if Field.ClassType = TOneOf then
            GenerateForOneOf(Field as TOneOf, CanName, FieldType, Indent)
          else if Field.ClassType = TMap then
            GenerateForMap(Field as TMap, CanName, FieldType, Indent)
          else
            GenerateForMessage(Field, CanName, FieldType, Indent);
          Exit;
        end;

        // Repeated Field
        if IsAnEnumType(Field.FieldType) then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('%sfor _%s in F%s do', [Indent, CanName, CanName]));
          Unitcode.ImplementationCode.Methods.Add(Format('%s  SaveInt32(Stream, F%s, %d);',
                           [Indent, CanName, Field.FieldNumber]));
          Unitcode.ImplementationCode.Methods.Add('');
          Exit;
        end;

        if IsSimpleType(Field.FieldType) then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format('  SaveRepeated%s(Stream, F%s, %d);',
              [Canonicalize(Field.FieldType.Name), CanName, Field.FieldNumber]));
          Exit;
        end;

        if IsOneOfType(Field, Self.Proto, RelatedProtos) then
        begin
          raise EInvalidSyntax.Create(Format('oneof cannot be repeated (%s)',
            [Field.Name]));
        end;

        Unitcode.ImplementationCode.Methods.Add(Format('  specialize SaveRepeatedMessage<%s>(Stream, F%s, %d);',
                         [GetNonRepeatedType4FPC(Field.FieldType.Name), CanName, Field.FieldNumber]));
      end;

      procedure GenerateForOneOf(OneOf: TOneOf; CanName, FieldType: AnsiString;
        Indent: AnsiString);
      var
        i: Integer;
        aField: TOneOfField;

      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%sif %s <> nil then',
          [Indent, OneOf.CanonicalizeName]));
        Unitcode.ImplementationCode.Methods.Add(Format('%sbegin', [Indent]));
        for i := 0 to OneOf.Fields.Count - 1 do
        begin
          aField := OneOf.Fields[i];
          Unitcode.ImplementationCode.Methods.Add(Format('%sif %s.GetPointerByIndex(%d) <> nil then',
            [Indent + '  ', OneOf.CanonicalizeName, i]));
            GenerateForField(aField, Indent + '    ')
        end;
        Unitcode.ImplementationCode.Methods.Add(Format('%send;', [Indent]));

      end;

      procedure GenerateForSimpleType(Field: TMessageField; CanName,
        FieldType: AnsiString; Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%sSave%s(Stream, %s, %d);',
          [Indent, Canonicalize(Field.FieldType.Name), CanName, Field.FieldNumber]));

      end;

      procedure GenerateForEnum(Field: TMessageField; CanName,
        FieldType: AnsiString; Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%sSaveInt32(Stream, Ord(%s), %d);',
          [Indent, CanName, Field.FieldNumber]))
      end;

      procedure GenerateForMessage(Field: TMessageField; CanName,
        FieldType: AnsiString; Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%sSaveMessage(Stream, %s, %d);',
          [Indent, CanName, Field.FieldNumber]));

      end;

      procedure GenerateForMap(aMap: TMap; CanName, FieldType: AnsiString;
        Indent: AnsiString);

      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%sif %s <> nil then' + sLineBreak +
                                                       '%s  %s.SaveToStream(Stream);', [Indent, Canname, Indent, CanName]));
      end;

    var
      Field: TMessageField;
      VarParts: TStringList;
      Str: AnsiString;

    begin
      Unitcode.ImplementationCode.Methods.Add(
        Format(
          'procedure %s.SaveToStream(Stream: TProtoStreamWriter);',
           [GetFPCType(aMessage.MessageType, CreateContext(nil))]));

      VarParts := TStringList.Create;
      for Field in aMessage.Fields do
      begin
        if IsAnEnumType(Field.FieldType) and Field.FieldType.IsRepeated then
        begin
          VarParts.Add(Format('  _%s: T%s;',  [Canonicalize(Field.Name),
            JoinStrings([GetUnitName(Field, Proto, RelatedProtos),
              GetNonRepeatedType4FPC(Field.FieldType.Name)], '.')]));
        end
      end;

      if VarParts.Count <> 0 then
      begin
        Unitcode.ImplementationCode.Methods.Add('var');
        for Str in VarParts do
          Unitcode.ImplementationCode.Methods.Add(Str);
        Unitcode.ImplementationCode.Methods.Add('');
        Unitcode.ImplementationCode.Methods.Add('');

      end;

      Unitcode.ImplementationCode.Methods.Add('begin');

      for Field in aMessage.Fields do
      begin
        GenerateForField(Field, Indent);

        Unitcode.ImplementationCode.Methods.Add('');
      end;

      Unitcode.ImplementationCode.Methods.Add('end;');
      Unitcode.ImplementationCode.Methods.Add('');

    end;

    procedure GenerateLoadFromStream;

      procedure GenerateForOneOf(OneOf: TOneOf; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForSimpleType(Field: TMessageField; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForEnum(Field: TMessageField; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForMap(aMap: TMap; CanName, FieldType: AnsiString; Indent: AnsiString); forward;
      procedure GenerateForMessage(Field: TMessageField; CanName, FieldType: AnsiString; Indent: AnsiString); forward;

      procedure GenerateForField(Field: TMessageField; Indent: AnsiString);
      var
        CanName: AnsiString;
        FieldType: AnsiString;

      begin
        CanName := Field.CanonicalizeFullNameForWriting;
        FieldType :=  JoinStrings([GetUnitName(Field, Proto, RelatedProtos),
          GetFPCType(Field.FieldType, CreateContext(Field.FieldType.Parent.Message))], '.');

        if not Field.FieldType.IsRepeated then
        begin
          if IsAnEnumType(Field.FieldType) then
            GenerateForEnum(Field, CanName, FieldType, Indent + '  ')
          else if IsSimpleType(Field.FieldType) then
            GenerateForSimpleType(Field, CanName, FieldType, Indent + '  ')
          else if Field.ClassType = TOneOf then
            GenerateForOneOf(Field as TOneOf, CanName, FieldType, Indent + '  ')
          else if Field.ClassType = TMap then
            GenerateForMap(Field as TMap, CanName, FieldType, Indent + '  ')
          else
            GenerateForMessage(Field, CanName, FieldType, Indent + '  ');
          Exit;
        end;

        Unitcode.ImplementationCode.Methods.Add(Format(
          '%s  %d: ',
          [Indent, Field.FieldNumber]));

        // Repeated Field
        if IsAnEnumType(Field.FieldType) then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format(
          '%s      if not LoadRepeatedInt32(Stream, Mutable%s) then' +
          '%s        Exit(False);' +
            sLineBreak,
            [
            Indent, CanName,
            Indent
            ]));
          Exit;
        end;

        if IsSimpleType(Field.FieldType) then
        begin
          Unitcode.ImplementationCode.Methods.Add(Format(
          '%s    if not LoadRepeated%s(Stream, Mutable%s) then' + sLineBreak +
          '%s      Exit(False);' +
          sLineBreak,
            [
            Indent, Canonicalize(Field.FieldType.Name), CanName,
            Indent
            ]));
          Exit;
        end;

        if IsOneOfType(Field, Self.Proto, RelatedProtos) then
        begin
          raise EInvalidSyntax.Create(Format('oneof cannot be repeated (%s)',
            [Field.Name]));
        end;

        Unitcode.ImplementationCode.Methods.Add(Format(
          '%s    if not (specialize LoadRepeatedMessage<%s>(Stream, F%s)) then' + sLineBreak +
          '%s      Exit(False);' +
          sLineBreak,
            [Indent, GetNonRepeatedType4FPC(Field.FieldType.Name), CanName,
             Indent
            ]));
      end;

      procedure GenerateForOneOf(OneOf: TOneOf; CanName, FieldType: AnsiString;
        Indent: AnsiString);
      var
        aField: TOneOfField;

      begin
        for aField in OneOf.Fields do
        begin
          GenerateForField(aField, Copy(Indent, 1, Length(Indent) - 2));

        end;

      end;

      procedure GenerateForSimpleType(Field: TMessageField; CanName,
        FieldType: AnsiString; Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%s%d:' + sLineBreak +
          '%s  %s := Load%s(Stream);' + sLineBreak,
          [Indent, Field.FieldNumber,
           Indent, CanName, Canonicalize(Field.FieldType.Name)]));

      end;

      procedure GenerateForEnum(Field: TMessageField; CanName,
        FieldType: AnsiString; Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%s%s%d: %s := %s(LoadInt32(Stream));',
          [sLineBreak, Indent, Field.FieldNumber, CanName, FieldType]))
      end;

      procedure GenerateForMap(aMap: TMap; CanName, FieldType: AnsiString;
        Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%s%d:', [Indent, aMap.FieldNumber]));
        Unitcode.ImplementationCode.Methods.Add(Format('%sbegin', [Indent]));

        Unitcode.ImplementationCode.Methods.Add(Format('%s  if WireType <> 2 then' + sLineBreak +
                                                       '%s    Exit(False);', [Indent, Indent]));
        Unitcode.ImplementationCode.Methods.Add(Format('%s  if not Mutable%s.LoadFromStream(Stream) then', [Indent, CanName, Indent]));
        Unitcode.ImplementationCode.Methods.Add(Format('%s    Exit(False);', [Indent]));

        Unitcode.ImplementationCode.Methods.Add(Format('%send;', [Indent]));

      end;

      procedure GenerateForMessage(Field: TMessageField; CanName,
        FieldType: AnsiString; Indent: AnsiString);
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('%s%d:', [Indent, Field.FieldNumber]));
        Unitcode.ImplementationCode.Methods.Add(Format('%sbegin', [Indent]));
        Unitcode.ImplementationCode.Methods.Add(Format('%s  if WireType <> 2 then' + sLineBreak +
                                                       '%s    Exit(False);', [Indent, Indent]));
        Unitcode.ImplementationCode.Methods.Add(Format('%s  if not LoadMessage(Stream, %s) then' + sLineBreak +
                                                       '%s    Exit(False);', [Indent, Field.CanonicalizeFullNameForWriting, Indent]));
        Unitcode.ImplementationCode.Methods.Add(Format('%send;', [Indent]));
      end;


    var
      Field: TMessageField;

    begin
      Unitcode.ImplementationCode.Methods.Add(Format('function %s.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;',
        [GetFPCType(aMessage.MessageType, CreateContext(nil))]));
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
        GenerateForField(Field, Indent);
      Unitcode.ImplementationCode.Methods.Add('');

      Unitcode.ImplementationCode.Methods.Add('    end;');
      Unitcode.ImplementationCode.Methods.Add('  end;' + sLineBreak);

      Unitcode.ImplementationCode.Methods.Add('  Result := StartPos + Len = Stream.Position;' + sLineBreak);
      Unitcode.ImplementationCode.Methods.Add('end;');

    end;

    begin
      GenerateConstructors;
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateDestructor;
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateClear;
      Unitcode.ImplementationCode.Methods.Add('');
      // GenerateToString;
      // Unitcode.ImplementationCode.Methods.Add('');
      GenerateSaveToStream(Indent);
      Unitcode.ImplementationCode.Methods.Add('');
      GenerateLoadFromStream;
  end;

begin
  GenerateDeclarationForMessage(AMessage, Unitcode);
  GenerateImplementationForMessage(AMessage, Unitcode);

end;

procedure TPBCodeGeneratorV1.GenerateCodeForMessageField(
  const aField: TMessageField; UnitCode: TUnitCode; const Indent: AnsiString);

  function ApplyPattern(const Template: AnsiString): AnsiString;
  begin
    Result := Template;
    Result := StringReplace(Result, '[[Indent]]', Indent, [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.Type]]', aField.FieldType.Name, [rfReplaceAll]);
    Result := StringReplace(
      Result,
      '[[Field.FullFPCType]]',
      GetFPCType(aField.FieldType, CreateContext(nil)),
      [rfReplaceAll]);
    Result := StringReplace(
      Result,
      '[[Field.ShortFPCType]]',
      GetFPCType(aField.FieldType, CreateContext(aField.FieldType.Parent.Message)),
      [rfReplaceAll]);
    if aField.FieldType.PackageName <> '' then
    begin
      Result := StringReplace(Result, '[[Field.UnitNameWithDot]]',
        GetUnitName(aField, Proto, RelatedProtos) + '.', [rfReplaceAll]);
        Result := StringReplace(Result, '[[Field.PackageNameWithDot]]',
          aField.FieldType.PackageName + '.', [rfReplaceAll])
    end
    else
    begin
      Result := StringReplace(Result, '[[Field.UnitNameWithDot]]', '', [rfReplaceAll]);
      Result := StringReplace(Result, '[[Field.PackageNameWithDot]]', '', [rfReplaceAll])
    end;

    Result := StringReplace(Result, '[[Field.Name]]', aField.Name,
      [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.Number]]',
      IntToStr(aField.FieldNumber), [rfReplaceAll]);
    Result := StringReplace(Result, '[[Field.CanName]]', Canonicalize(aField.Name),
      [rfReplaceAll]);
    Result := StringReplace(Result,
      '[[Field.ClassName]]',
      GetFPCType(aField.FieldType.Parent.Message.MessageType, CreateContext(nil)),
      [rfReplaceAll]);
    if aField.FieldType.IsRepeated then
      Result := StringReplace(Result, '[[Field.InnerFPCType]]',
        GetNonRepeatedType4FPC(aField.FieldType.Name), [rfReplaceAll]);

  end;

  {$i NonRepeatedSimpleFieldTemplate.inc}
  {$i NonRepeatedNonSimpleFieldTemplate.inc}
  {$i RepeatedNonSimpleFieldTemplate.inc}
  {$i RepeatedSimpleFieldTemplate.inc}


  procedure GenerateDeclaration;
  begin
    if not aField.FieldType.IsRepeated and IsSimpleType(aField.FieldType) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(DeclareNonRepeatedSimpleFieldTemplate))
    else if not aField.FieldType.IsRepeated and not IsSimpleType(aField.FieldType) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(DeclareNonRepeatedNonSimpleFieldTemplate))
    else if aField.FieldType.IsRepeated and not IsSimpleType(aField.FieldType) then
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(DeclareRepeatedNonSimpleFieldTemplate))
    else
      UnitCode.InterfaceCode.TypeList.Add(ApplyPattern(DeclareRepeatedSimpleFieldTemplate));
  end;

  procedure GenerateImplementation;
  begin
    if not aField.FieldType.IsRepeated and IsSimpleType(aField.FieldType) then
    else if not aField.FieldType.IsRepeated and not IsSimpleType(aField.FieldType) then
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(ImplementNonRepeatedNonSimpleFieldTemplate))
    else if aField.FieldType.IsRepeated and not IsSimpleType(aField.FieldType) then
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(ImplementRepeatedNonSimpleFieldTemplate))
    else
      UnitCode.ImplementationCode.Methods.Add(ApplyPattern(ImplementRepeatedSimpleFieldTemplate));

  end;

begin
  GenerateDeclaration;
  GenerateImplementation;
end;

procedure TPBCodeGeneratorV1.GenerateCodeForOneOf(const OneOf: TOneOf;
  Unitcode: TUnitCode; const Context: TContext; const Indent: AnsiString);

  procedure GenerateDeclaration;
  var
    Field: TOneOfField;

  begin
    Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = Class(TBaseOneOf)', [Indent,
      GetFPCType(OneOf.FieldType, Context)]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sprivate', [Indent]));

    for Field in OneOf.Fields do
    begin
      Unitcode.InterfaceCode.TypeList.Add(Format('%sfunction Get%s: %s;',
      [Indent + '  ', Canonicalize(Field.Name), GetFPCType(Field.FieldType, Context)]));
      Unitcode.InterfaceCode.TypeList.Add(Format('%sprocedure Set%s(_%s: %s);',
      [Indent + '  ', Canonicalize(Field.Name), Canonicalize(Field.Name),
        GetFPCType(Field.FieldType, Context)]));
    end;
    Unitcode.InterfaceCode.TypeList.Add('');

    Unitcode.InterfaceCode.TypeList.Add('%spublic', [Indent]);
    for Field in OneOf.Fields do
      Unitcode.InterfaceCode.TypeList.Add(Format('%sproperty %s: %s read Get%s write Set%s;',
        [Indent + '  ', Canonicalize(Field.Name), GetFPCType(Field.FieldType, Context),
        Canonicalize(Field.Name), Canonicalize(Field.Name)]));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  constructor Create;', [Indent]));

    Unitcode.InterfaceCode.TypeList.Add(Format('%sdestructor Destroy; override;', [Indent + '  ']));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sprocedure Clear; override;', [Indent + '  ']));
    // Unitcode.InterfaceCode.TypeList.Add(Format('%sfunction ToString: AnsiString; override;', [Indent + '  ']));
    Unitcode.InterfaceCode.TypeList.Add('');
    Unitcode.InterfaceCode.TypeList.Add(Format('%send;', [Indent]));

  end;

  procedure GenerateImplementation;
  var
    i: Integer;
    Field: TOneOfField;
    OneOfClassName: AnsiString;
    PtrType: AnsiString;

  begin
    OneOfClassName := GetFPCType(OneOf.OneOfFieldPBType, Context);

    for i := 0 to OneOf.Fields.Count - 1 do
    begin
      Field := OneOf.Fields[i];

      Unitcode.ImplementationCode.Methods.Add(Format('function %s.Get%s: %s;',
      [OneOfClassName, Canonicalize(Field.Name), GetFPCType(Field.FieldType, Context)]));
      Unitcode.ImplementationCode.Methods.Add(Format('begin', []));
      if IsAnEnumType(Field.FieldType) then
        PtrType := 'PUInt64'
      else if IsSimpleType(Field.FieldType) then
        PtrType := 'P' + GetFPCType(Field.FieldType, Context)
      else
        PtrType := GetFPCType(Field.FieldType, Context);

      if PtrType <> GetFPCType(Field.FieldType, Context) then
        Unitcode.ImplementationCode.Methods.Add(
            Format('  Result := %s(%s(Self.GetPointerByIndex(%d))^)',
            [GetFPCType(Field.FieldType, Context), PtrType, i]))
      else
        Unitcode.ImplementationCode.Methods.Add(
          Format('  Result := %s(Self.GetPointerByIndex(%d))',
            [GetFPCType(Field.FieldType, Context), i]));
      Unitcode.ImplementationCode.Methods.Add(Format('', []));
      Unitcode.ImplementationCode.Methods.Add(Format('end;', []));
      Unitcode.ImplementationCode.Methods.Add(Format('', []));

      Unitcode.ImplementationCode.Methods.Add(Format('procedure %s.Set%s(_%s: %s);',
      [OneOfClassName, Canonicalize(Field.Name), Canonicalize(Field.Name),
        GetFPCType(Field.FieldType, Context)]));
      if PtrType <> GetFPCType(Field.FieldType, Context) then
      begin
        Unitcode.ImplementationCode.Methods.Add(Format('var', []));
        Unitcode.ImplementationCode.Methods.Add(Format('  PData: %s;',
          [PtrType]));
        Unitcode.ImplementationCode.Methods.Add(Format('', []));

      end;

      Unitcode.ImplementationCode.Methods.Add(Format('begin', []));
      if IsSimpleType(Field.FieldType) then
      begin
        Unitcode.ImplementationCode.Methods.Add('  PData := new(%s);',
          [PtrType]);
        if IsAnEnumType(Field.FieldType) then
          Unitcode.ImplementationCode.Methods.Add('  PData^ := UInt64(_%s);',
            [Canonicalize(Field.Name)])
        else
          Unitcode.ImplementationCode.Methods.Add('  PData^ := _%s;',
          [Canonicalize(Field.Name)]);

        Unitcode.ImplementationCode.Methods.Add(
          Format('  Self.SetPointerByIndex(%d, PData);', [i]));

      end
      else
        Unitcode.ImplementationCode.Methods.Add(
          Format('  Self.SetPointerByIndex(%d, _%s);', [i, Canonicalize(Field.Name)]));
      Unitcode.ImplementationCode.Methods.Add(Format('', []));
      Unitcode.ImplementationCode.Methods.Add(Format('end;', []));
      Unitcode.ImplementationCode.Methods.Add(Format('', []));

    end;
    Unitcode.ImplementationCode.Methods.Add('');

    Unitcode.ImplementationCode.Methods.Add(Format('constructor %s.Create;', [OneOfClassName]));
    Unitcode.ImplementationCode.Methods.Add(Format('begin', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  inherited Create;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('', []));
    Unitcode.ImplementationCode.Methods.Add(Format('end;', []));
    Unitcode.ImplementationCode.Methods.Add('');

    Unitcode.ImplementationCode.Methods.Add(Format('destructor %s.Destroy;', [OneOfClassName]));
    Unitcode.ImplementationCode.Methods.Add(Format('begin', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  Clear;' + sLineBreak, []));
    Unitcode.ImplementationCode.Methods.Add(Format('', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  inherited Destroy;' + sLineBreak, []));
    Unitcode.ImplementationCode.Methods.Add(Format('end;', []));
    Unitcode.ImplementationCode.Methods.Add('');

    Unitcode.ImplementationCode.Methods.Add(Format('procedure %s.Clear;', [OneOfClassName]));
    Unitcode.ImplementationCode.Methods.Add(Format('begin', []));
    for i := 0 to OneOf.Fields.Count - 1 do
    begin
      Field := OneOf.Fields[i];
      if IsAnEnumType(Field.FieldType) then
        Unitcode.ImplementationCode.Methods.Add(
          Format('  MaybeDispose(PUInt64(GetPointerByIndex(%d)));',
            [i]))
      else if IsSimpleType(Field.FieldType) then
        Unitcode.ImplementationCode.Methods.Add(
          Format('  MaybeDispose(P%s(GetPointerByIndex(%d)));',
            [GetFPCType(Field.FieldType, Context), i]))
      else
        Unitcode.ImplementationCode.Methods.Add(Format('  Get%s.Free;',
        [Canonicalize(Field.Name)]));

    end;

    Unitcode.ImplementationCode.Methods.Add(Format('  inherited;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('', []));
    Unitcode.ImplementationCode.Methods.Add(Format('end;', []));
    Unitcode.ImplementationCode.Methods.Add('');

  end;

begin
  GenerateDeclaration;
  GenerateImplementation;

end;

procedure TPBCodeGeneratorV1.GenerateCodeForMap(const Map: TMap;
  Unitcode: TUnitCode; const Indent: AnsiString);

  procedure GenerateDeclaration;
  begin
    if IsSimpleType(Map.MapFieldPBType.ValuePBType) then
      Unitcode.InterfaceCode.TypeList.Add(Format(
        '%s%s = class(specialize TMap<%s, %s>)',
        [
          Indent,
          GetFPCType(Map.FieldType, CreateContext(Map.FieldType.Parent.Message)),
          GetFPCType(Map.MapFieldPBType.KeyPBType, CreateContext(Map.FieldType.Parent.Message)),
          GetFPCType(Map.MapFieldPBType.ValuePBType, CreateContext(Map.FieldType.Parent.Message))]))
    else
      Unitcode.InterfaceCode.TypeList.Add(Format('%s%s = class(specialize TMapSimpleKeyObjectValue<%s, %s>)',
        [Indent,
         GetFPCType(Map.FieldType, CreateContext(Map.FieldType.Parent.Message)),
         GetFPCType(Map.MapFieldPBType.KeyPBType, CreateContext(Map.FieldType.Parent.Message)),
         GetFPCType(Map.MapFieldPBType.ValuePBType, CreateContext(Map.FieldType.Parent.Message))]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%sprivate', [Indent]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  function LoadFromStream(Stream: TProtoStreamReader): Boolean;',
      [Indent]));
    Unitcode.InterfaceCode.TypeList.Add(Format('%s  procedure SaveToStream(Stream: TProtoStreamWriter);',
      [Indent]));
    Unitcode.InterfaceCode.TypeList.Add('');
      Unitcode.InterfaceCode.TypeList.Add(Format('%send;', [Indent]));

  end;

  procedure GenerateImplementation;
  var
    MapClassName: AnsiString;

  begin
    MapClassName := GetFPCType(Map.MapFieldPBType, CreateContext(nil));

    Unitcode.ImplementationCode.Methods.Add('');
    Unitcode.ImplementationCode.Methods.Add(Format('function %s.LoadFromStream(Stream: TProtoStreamReader): Boolean;', [MapClassName]));
    Unitcode.ImplementationCode.Methods.Add(Format('var', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  StartPos, Len, f, w, fs: Integer;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  Key: %s;', [
      GetFPCType(Map.MapFieldPBType.KeyPBType, CreateContext(Map.FieldType.Parent.Message))]));
    Unitcode.ImplementationCode.Methods.Add(Format('  Value: %s;', [
      GetFPCType(Map.MapFieldPBType.ValuePBType, CreateContext(Map.FieldType.Parent.Message))]));
    Unitcode.ImplementationCode.Methods.Add('');
    Unitcode.ImplementationCode.Methods.Add(Format('begin', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  Len := Stream.ReadVarUInt32;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  StartPos := Stream.Position;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('  fs := 0;', []));

    Unitcode.ImplementationCode.Methods.Add(sLineBreak + '  while Stream.Position < StartPos + Len do');
    Unitcode.ImplementationCode.Methods.Add('  begin');
    Unitcode.ImplementationCode.Methods.Add(Format('    Stream.ReadTag(f, w);' + sLineBreak, []));
    Unitcode.ImplementationCode.Methods.Add(Format('    if f = 1 then', []));
    Unitcode.ImplementationCode.Methods.Add(Format('    begin', []));
    Unitcode.ImplementationCode.Methods.Add(Format('      Key := Load%s(Stream)', [Map.MapFieldPBType.KeyPBType.Name]));
    Unitcode.ImplementationCode.Methods.Add(Format('    end', []));
    Unitcode.ImplementationCode.Methods.Add(Format('    else if f = 2 then', []));
    Unitcode.ImplementationCode.Methods.Add(Format('    begin', []));
    if IsAnEnumType(Map.MapFieldPBType.ValuePBType) then
      Unitcode.ImplementationCode.Methods.Add(Format('      Value := %s(LoadInt32(Stream))' + sLineBreak,
            [GetFPCType(Map.MapFieldPBType.ValuePBType, CreateContext(Map.FieldType.Parent.Message))]))
    else if IsSimpleType(Map.MapFieldPBType.ValuePBType) then
      Unitcode.ImplementationCode.Methods.Add(Format('      Value := Load%s(Stream)',
          [Map.MapFieldPBType.ValuePBType.Name]))
    else
    begin
      Unitcode.ImplementationCode.Methods.Add(Format(
                                              '      Value := %s.Create;' + sLineBreak, [
                                              GetFPCType(Map.MapFieldPBType.ValuePBType, CreateContext(Map.FieldType.Parent.Message))]));
      Unitcode.ImplementationCode.Methods.Add('      if not LoadMessage(Stream, Value) then');
      Unitcode.ImplementationCode.Methods.Add('      begin');
      Unitcode.ImplementationCode.Methods.Add('        Exit(False);');
      Unitcode.ImplementationCode.Methods.Add('      end' + sLineBreak);

    end;
    Unitcode.ImplementationCode.Methods.Add(Format('    end', []));
    Unitcode.ImplementationCode.Methods.Add(Format('    else', []));
    Unitcode.ImplementationCode.Methods.Add(Format('      Exit(False);' + sLineBreak, []));
    Unitcode.ImplementationCode.Methods.Add(Format('    fs := fs xor f;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('    if fs = 3 then', []));
    Unitcode.ImplementationCode.Methods.Add(Format('    begin', []));
    Unitcode.ImplementationCode.Methods.Add(Format('      Self.Add(Key, Value);', []));
    Unitcode.ImplementationCode.Methods.Add(Format('      fs := 0;' + sLineBreak, []));
    Unitcode.ImplementationCode.Methods.Add(Format('    end;' + sLineBreak, []));
    Unitcode.ImplementationCode.Methods.Add(Format('  end;' + sLineBreak, []));

    Unitcode.ImplementationCode.Methods.Add(Format('  Result := StartPos + Len = Stream.Position;', []));
    Unitcode.ImplementationCode.Methods.Add(Format('end;' + sLineBreak, []));

    Unitcode.ImplementationCode.Methods.Add(Format('procedure %s.SaveToStream(Stream: TProtoStreamWriter);', [MapClassName]));
    Unitcode.ImplementationCode.Methods.Add('var');
    Unitcode.ImplementationCode.Methods.Add(Format('  it: %s.TPairEnumerator;', [MapClassName]));
    Unitcode.ImplementationCode.Methods.Add('  SizeNode: TLinkListNode;' + sLineBreak);
    Unitcode.ImplementationCode.Methods.Add('begin');
    Unitcode.ImplementationCode.Methods.Add('  if (Self = nil) or (Self.Count = 0) then');
    Unitcode.ImplementationCode.Methods.Add('    Exit;' + sLineBreak);
    Unitcode.ImplementationCode.Methods.Add('  it := Self.GetEnumerator;');
    Unitcode.ImplementationCode.Methods.Add('  while it.MoveNext do');
    Unitcode.ImplementationCode.Methods.Add('  begin');
    Unitcode.ImplementationCode.Methods.Add(Format('    Stream.WriteTag(%d, WIRETYPE_LENGTH_DELIMITED);',
       [Map.FieldNumber]));
    Unitcode.ImplementationCode.Methods.Add(       '    SizeNode := Stream.AddIntervalNode;');
    Unitcode.ImplementationCode.Methods.Add(Format('    Save%s(Stream, it.Current.Key, 1);',
       [GetFPCType(Map.MapFieldPBType.KeyPBType,
         CreateContext(Map.FieldType.Parent.Message))]));

    if IsAnEnumType(Map.MapFieldPBType.ValuePBType) then
      Unitcode.ImplementationCode.Methods.Add('    SaveInt32(Stream, Int32(it.Current.Value), 2);')
    else if IsSimpleType(Map.MapFieldPBType.ValuePBType) then
      Unitcode.ImplementationCode.Methods.Add(Format('    Save%s(Stream, it.Current.Value, 2);',
    [GetFPCType(Map.MapFieldPBType.ValuePBType,
      CreateContext(Map.FieldType.Parent.Message))]))
    else
      Unitcode.ImplementationCode.Methods.Add('    SaveMessage(Stream, it.Current.Value, 2);');

    Unitcode.ImplementationCode.Methods.Add(
      '    SizeNode.WriteLength(SizeNode.TotalSize);' + sLineBreak);
    Unitcode.ImplementationCode.Methods.Add('  end;');
    Unitcode.ImplementationCode.Methods.Add('  it.Free;' + sLineBreak);
    Unitcode.ImplementationCode.Methods.Add('end;' + sLineBreak);
    Unitcode.ImplementationCode.Methods.Add('');

  end;

begin
  GenerateDeclaration;
  GenerateImplementation;

end;

function TPBCodeGeneratorV1.GetFPCType(PBType: TPBBaseType; Context: TContext
  ): AnsiString;

  function GetOneOfClassName(anOneOf: TOneOfPBType): AnsiString;
  begin
    Result := Format('T%s', [Canonicalize(anOneOf.Name)]);

  end;

  function GetMessageClassName(aMessage: TMessagePBType): AnsiString;
  begin
    Result := Format('T%s', [Canonicalize(aMessage.Name)]);

  end;

var
  Parent: TParent;

begin
  if PBType.PackageName <> '' then
    FmtFatalLn('Interesting %s', [PBType.PackageName]);

  Result := '';
  if IsSimpleType(PBType) and not IsAnEnumType(PBType) then
    Exit(UtilsUnit.GetNonRepeatedType4FPC(PBType.Name));

  if PBType is TMessagePBType then
    Result := GetMessageClassName(PBType as TMessagePBType)
  else if PBType is TOneOfPBType then
    Result := GetOneOfClassName(PBType as TOneOfPBType)
  else if PBType is TMapPBType then
    Result := Format('T%sTo%sMap', [
      Canonicalize((PBType as TMapPBType).KeyPBType.Name),
      Canonicalize((PBType as TMapPBType).ValuePBType.Name)])
  else if PBType.IsRepeated then
    Result := 'T' + Canonicalize(PBType.Name)
  else
    Result := UtilsUnit.GetNonRepeatedType4FPC(PBType.Name);

  Parent := PBType.Parent;

  while (Parent.Message <> Context.Message) and (Parent.Message <> nil) do
  begin
    Result := Format('T%s.%s', [Canonicalize(Parent.Message.Name), Result]);
    Parent := Parent.Message.Parent;

  end;

end;

procedure TPBCodeGeneratorV1.GenerateCode;
var
  UnitCode: TUnitCode;
  Enum: TEnum;
  Message: TMessage;
  Code: AnsiString;

begin
  UnitCode := TUnitCode.Create(GetUnitName(Proto.InputProtoFilename));

  GenerateCodeForImports(Proto.Imports, UnitCode, '', '');

  for Enum in Proto.Enums do
    GenerateCodeForEnum(Enum, UnitCode, CreateContext(nil), '  ');

  for Message in Proto.Messages do
    UnitCode.InterfaceCode.TypeList.Add(
     Format('%s%s = class;', ['  ',
       GetFPCType(Message.MessageType, CreateContext(nil))]));

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

class procedure TPBBaseCodeGenerator.GenerateCode(ProtoMap: TProtoMap);
var
  AllProtos: TProtos;
  Proto: TProto;
  it: TProtoMap.TPairEnumerator;
  cg: TPBBaseCodeGenerator;

begin
  AllProtos := TProtos.Create;
  it := ProtoMap.GetEnumerator;
  while it.MoveNext do
    AllProtos.Add(it.Current.Value);
  it.Free;

  it := ProtoMap.GetEnumerator;
  while it.MoveNext do
  begin
    Proto := it.Current.Value;

    cg := GetCodeGenerator(Proto, AllProtos);
    cg.GenerateCode;
    cg.Free;

  end;

  AllProtos.Clear;
  Allprotos.Free;
end;

destructor TPBBaseCodeGenerator.Destroy;
begin
  inherited;

end;


end.

