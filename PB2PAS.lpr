program PB2PAS;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
uses
  Classes, sysutils, ParameterManagerUnit, PBParserUnit;


begin
  WriteLn(ExtractFileDir(GetRunTimeParameterManager.ValueByName['--InputFile']));
  PBParserUnit.GenerateCode(GetRunTimeParameterManager.ValueByName['--InputFile']);

end.

