program PB2PAS;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
uses
  Classes, sysutils, ParameterManagerUnit, PBParserUnit, StringUnit, UtilsUnit,
  PBDefinitionUnit, PBCodeGenerator, PBCodeGeneratorUnit;

begin
  WriteLn('<A>');
  WriteLn(GetRunTimeParameterManager.ValueByName['--InputFile']);

  PBCodeGenerator.GenerateCode(GetRunTimeParameterManager.ValueByName['--InputFile']);

  WriteLn('</A>');
end.

