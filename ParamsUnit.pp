unit ParamsUnit;

{$mode ObjFPC}{$H+}

interface

uses
  ParamManagerUnit;

type
  { TParam }

  // All the Fields that we want to be assignable using InitAndParse must be
  // defined as "published" and inherit from TValue.
  TParam = class(TValue)
  published
    InputFileName: TStringValue;
    Verbosity: TIntValue;

  public

  end;


implementation

end.

