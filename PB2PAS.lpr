program PB2PAS;

{$mode objfpc}{$H+}
uses
  cthreads, PBParserUnit, PBDefinitionUnit, ALoggerUnit, ParamsUnit,
  PBCodeGeneratorUnit, ListUnit, ParamManagerUnit;

var
  Params: TPB2PASParams;
  ProtoMap: TProtoMap;
  it: TProtoMap.TPairEnumerator;

begin
  Params := TPB2PASParams.Create;
  ParamManagerUnit.InitAndParse('Verbosity=0', Params);
  ParamManagerUnit.InitFromParameters(Params);

  ALoggerUnit.InitLogger(Params.Verbosity.Value);
  WriteLn('<A>');

  ProtoMap := TBaseProtoParser.ParseAll(
    Params.InputFileName.Value);

  it := ProtoMap.GetEnumerator;
  while it.MoveNext do
    WriteLn(it.Current.Value.ToXML);
  it.Free;

  TPBBaseCodeGenerator.GenerateCode(ProtoMap);

  WriteLn('</A>');

  ProtoMap.Free;
end.

