unit SampleUnitUnit;
{$Mode objfpc}
interface

uses 
    classes, sysutils;

type

  TToken = Class(TObject)
  private

    Ffingerprint: UInt64;
    FTokenStr: String;
  end;

  TTitle = Class(TObject)
  private

    Ftokens: specialize TObjectList<TRepeatedUnit>;
  end;
