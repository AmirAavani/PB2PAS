program PB2PAS;

{$mode objfpc}{$H+}
uses
  cthreads, Classes, sysutils, ParameterManagerUnit, PBParserUnit,
  StringUnit, UtilsUnit, PBDefinitionUnit, PBCodeGeneratorUnit, ProtoHelperUnit,
  ALoggerUnit, PBOptionUnit, NamedObjectListUnit;

var
  ProtoMap: TProtoMap;
  it: TProtoMap.TPairEnumerator;

begin
  WriteLn('<A>');

  ProtoMap := TBaseProtoParser.ParseAll(
    GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString);

  it := ProtoMap.GetEnumerator;
  while it.MoveNext do
    WriteLn(it.Current.Value.ToXML);
  it.Free;

  TPBBaseCodeGenerator.GenerateCode(ProtoMap);

  WriteLn('</A>');

  ProtoMap.Free;
end.

