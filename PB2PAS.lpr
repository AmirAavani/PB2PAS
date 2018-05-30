program PB2PAS;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, ParameterManagerUnit, PBParserUnit, heaptrc,
  ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit
  { you can add units after this };


begin
  WriteLn(ExtractFileDir(TRunTimeParameterManager.GetInstance.ValueByName['--InputFile']));
  TProto.GenerateCode(TRunTimeParameterManager.GetInstance.ValueByName['--InputFile']);
end.

