program PB2PAS;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
uses
  Classes, sysutils, ParameterManagerUnit, PBParserUnit, StringUnit;

var
  Files: TStringList;
  f: AnsiString;

begin
  WriteLn(GetRunTimeParameterManager.ValueByName['--InputFile']);

  Files := Split(GetRunTimeParameterManager.ValueByName['--InputFile'], ',');
  for f in Files do
    PBParserUnit.GenerateCode(f);
  Files.Free;

end.

