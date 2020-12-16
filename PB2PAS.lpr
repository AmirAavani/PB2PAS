program PB2PAS;

{$mode objfpc}{$H+}
uses
  cthreads, Classes, sysutils, ParameterManagerUnit, PBParserUnit, StringUnit,
  UtilsUnit, PBDefinitionUnit, PBCodeGeneratorUnit,
  ProtoHelperUnit, ALoggerUnit, fgl, PBOptionUnit, ObjectListUnit;


var
  ProtoMap: TProtoMap;
  i: Integer;

begin
  WriteLn('<A>');

  ProtoMap := TBaseProtoParser.ParseAll(
    GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString);

  for i := 0 to ProtoMap.Count - 1 do
    WriteLn(ProtoMap.Data[i].ToXML);

  TPBBaseCodeGenerator.GenerateCode(ProtoMap);

  WriteLn('</A>');
end.

