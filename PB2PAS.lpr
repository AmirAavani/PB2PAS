program PB2PAS;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
uses
  cthreads, Classes, sysutils, ParameterManagerUnit, PBParserUnit, StringUnit,
  UtilsUnit, PBDefinitionUnit, PBCodeGenerator, PBCodeGeneratorUnit,
  ProtoHelperUnit, ALoggerUnit, fgl, PBTypeUnit, PBOptionUnit, ObjectListUnit;


type
  TIntIntMap = specialize TFPGMap<Integer, Integer>;

begin
  WriteLn('<A>');
  WriteLn(GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString);

  PBCodeGenerator.GenerateCode(GetRunTimeParameterManager.ValueByName['--InputFile'].AsAnsiString);

  WriteLn('</A>');
end.

