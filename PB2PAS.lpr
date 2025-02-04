program PB2PAS;

{$mode objfpc}{$H+}
uses
  cthreads, PBParserUnit, PBDefinitionUnit, ALoggerUnit, ParamsUnit, ParamManagerUnit,
  PBCodeGeneratorUnit;

var
  Params: TParam;
  ProtoMap: TProtoMap;
  it: TProtoMap.TPairEnumerator;

begin
  Params := TParam.Create;
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

