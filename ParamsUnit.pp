unit ParamsUnit;

{$mode ObjFPC}{$H+}

interface

uses
  ParamManagerUnit;

type
  { TPB2PASParams }
  TPB2PASParams = class(TValue)
  published
    InputFileName: TStringValue;
    Verbosity: TIntValue;
  end;

  { TZIODumpParams }
  TZIODumpParams = class(TValue)
  published
    InputFileName: TStringValue;
    MessageName: TStringValue;
    ProtoFile: TStringValue;
    Verbosity: TIntValue;
  end;

implementation

end.

