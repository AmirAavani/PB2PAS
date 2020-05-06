program PB2PAS;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}
uses
  Classes, sysutils, ParameterManagerUnit, PBParserUnit, StringUnit, UtilsUnit,
  PBDefinitionUnit, PBCodeGenerator, PBCodeGeneratorUnit;

var
  Files: TStringList;
  f: AnsiString;

begin
  WriteLn('<A>');
  WriteLn(GetRunTimeParameterManager.ValueByName['--InputFile']);

  Files := Split(GetRunTimeParameterManager.ValueByName['--InputFile'], ',');
  for f in Files do
  begin
    WriteLn(f);
    PBCodeGenerator.GenerateCode(f);

  end;

  Files.Free;
  WriteLn('</A>');

end.

