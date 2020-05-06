unit PBCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure GenerateCode(InputFilename: AnsiString);

implementation

uses
  PBParserUnit, PBDefinitionUnit, UtilsUnit;

procedure GenerateCode(InputFilename: AnsiString);
var
  Proto: TProto;
  Parser: TBaseProtoParser;
  InputStream: TFileStream;
  OutputStream: TFileStream;
  FilePath: AnsiString;
  OutputUnitName: AnsiString;

begin
  Parser := TBaseProtoParser.GetParser(InputFilename);

  OutputUnitName := GetUnitName(InputFilename);
  Proto := Parser.ParseProto;
  WriteLn(Format('<OutputUnitName Name = "%s" />', [OutputUnitName]));
  WriteLn(Proto.ToXML);

  OutputStream := TFileStream.Create(ConcatPaths([ExtractFileDir(InputFilename),
    OutputUnitName + '.pp']), fmCreate);

  Proto.PrepareForCodeGeneration;
  Proto.GenerateCode(OutputUnitName, OutputStream);

  OutputStream.Free;
  Proto.Free;
end;


end.

