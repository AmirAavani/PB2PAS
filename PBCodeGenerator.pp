unit PBCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure GenerateCode(InputFilename: AnsiString);

implementation

uses
  PBParserUnit, PBDefinitionUnit;

procedure GenerateCode(InputFilename: AnsiString);
var
  Proto: TProto;
  Parser: TBaseProtoParser;
  InputStream: TFileStream;
  OutputStream: TFileStream;
  FilePath: AnsiString;
  OutputUnitName: AnsiString;

begin
  InputStream := TFileStream.Create(InputFilename, fmOpenRead);
  Parser := TBaseProtoParser.GetParser(InputStream);
  InputStream.Free;

  Proto := Parser.ParseProto;
  WriteLn(Proto.ToString);
{
  OutputUnitName := GetUnitName(InputFilename);
  OutputStream := TFileStream.Create(ConcatPaths([ExtractFileDir(InputFilename),
    OutputUnitName + '.pp']), fmCreate);

  Proto.PrepareForCodeGeneration;
  Proto.GenerateCode(OutputUnitName, OutputStream);

  OutputStream.Free;
  Proto.Free;
  }
end;


end.

