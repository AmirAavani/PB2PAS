unit PBCodeGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure GenerateCode(InputFilename: AnsiString);

implementation

uses
  PBParserUnit, PBDefinitionUnit, PBCodeGeneratorUnit, UtilsUnit;

procedure GenerateCode(InputFilename: AnsiString);
var
  Proto: TProto;
  Parser: TBaseProtoParser;
  CodeGenerator: TPBBaseCodeGenerator;

begin
  Parser := TBaseProtoParser.GetParser(InputFilename);

  Proto := Parser.ParseProto;
  WriteLn(Format('<OutputUnitName Name = "%s" />', [Proto.UnitName]));
  WriteLn(Proto.ToXML);

  CodeGenerator := TPBBaseCodeGenerator.GetCodeGenerator(Proto);
  CodeGenerator.GenerateCode;
  CodeGenerator.Free;

  Proto.Free;
  Parser.Free;

end;


end.

