unit PBCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure GenerateCode(InputFilename: AnsiString);

implementation

uses
  PBParserUnit, PBDefinitionUnit, PBCodeGeneratorUnit, UtilsUnit, fgl;

type
  TAllProtos = specialize TFPGList<TProto>;

procedure GenerateCode(InputFilename: AnsiString);
  procedure Process(ProtoFile: AnsiString; AllProtos: TAllProtos; Marked: TStringList);
  var
    Proto: TProto;
    Parser: TBaseProtoParser;
    Import: AnsiString;

  begin
    Parser := TBaseProtoParser.GetParser(ProtoFile);

    Proto := Parser.ParseProto;
    WriteLn(Format('<OutputUnitName Name = "%s" />', [Proto.OutputUnitName]));
    WriteLn(Proto.ToXML);

    Parser.Free;
    AllProtos.Add(Proto);

    for Import in Proto.Imports do
      if Marked.IndexOf(Import) = -1 then
        Process(Import, AllProtos, Marked);
  end;

var
  AllProtos: TAllProtos;
  CodeGenerator: TPBBaseCodeGenerator;
  i: Integer;
  Proto: TProto;
  Marked: TStringList;

begin
  AllProtos := TAllProtos.Create;
  Marked := TStringList.Create;
  Marked.Sorted := True;

  Process(InputFilename, AllProtos, Marked);

  for Proto in AllProtos  do
  begin
    CodeGenerator := TPBBaseCodeGenerator.GetCodeGenerator(Proto, AllProtos);

    CodeGenerator.GenerateCode;

    CodeGenerator.Free;
  end;

  for Proto in AllProtos do
    Proto.Free;
  AllProtos.Free;

end;


end.

