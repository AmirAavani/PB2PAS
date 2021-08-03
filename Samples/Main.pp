program main;
{$R+}
{$O+}
uses
  SysUtils, Classes, TestUnit, ProtoHelperUnit;

procedure LoadFromFile(aFilename: AnsiString; var e: TMyMessage);
begin
  WriteLN(Format('Loading from %s', [aFilename]));
  e.Free;
  e := TMyMessage.Create;
  if not e.LoadFromStream(TFileStream.Create(aFilename, fmOpenRead)) then
  begin
    WriteLn('Error while loading ', aFilename);
    Halt(2);
    
  end;

end;

var
  e, ec: TMyMessage;
  s: TMyMessage.TSubMessage;

begin
  e := TMyMessage.Create;

  e.Dd := 1.234;
  e.SaveToStream(TFileStream.Create('/tmp/e1~', fmCreate));
  LoadFromFile('/tmp/e1~', e);

  e.F := 1.235;
  e.SaveToStream(TFileStream.Create('/tmp/e2~', fmCreate));
  LoadFromFile('/tmp/e2~', e);

  e.I32 := 202;
  e.SaveToStream(TFileStream.Create('/tmp/e3~', fmCreate));
  LoadFromFile('/tmp/e3~', e);

  e.I64 := -203;
  e.SaveToStream(TFileStream.Create('/tmp/e4~', fmCreate));
  LoadFromFile('/tmp/e4~', e);

  e.Ui32 := 204;
  e.SaveToStream(TFileStream.Create('/tmp/e5~', fmCreate));
  LoadFromFile('/tmp/e5~', e);

  e.Ui64 := 205;
  e.SaveToStream(TFileStream.Create('/tmp/e6~', fmCreate));
  LoadFromFile('/tmp/e6~', e);

  e.Si32 := -206;
  e.SaveToStream(TFileStream.Create('/tmp/e7~', fmCreate));
  LoadFromFile('/tmp/e7~', e);

  e.Si64 := 207;
  e.SaveToStream(TFileStream.Create('/tmp/e8~', fmCreate));
  LoadFromFile('/tmp/e8~', e);

  e.F32 := 208;
  e.SaveToStream(TFileStream.Create('/tmp/e9~', fmCreate));
  LoadFromFile('/tmp/e9~', e);

  e.F64 := 209;
  e.SaveToStream(TFileStream.Create('/tmp/e10~', fmCreate));
  LoadFromFile('/tmp/e10~', e);

  e.B := True;
  e.SaveToStream(TFileStream.Create('/tmp/e11~', fmCreate));
  LoadFromFile('/tmp/e11~', e);

  e.B := False;
  e.SaveToStream(TFileStream.Create('/tmp/e12~', fmCreate));
  LoadFromFile('/tmp/e12~', e);

  e.AnS := 'testing';
  e.SaveToStream(TFileStream.Create('/tmp/e13~', fmCreate));
  LoadFromFile('/tmp/e13~', e);

  e.MutableRdd.Add(300);   e.MutableRdd.Add(-300);
  e.SaveToStream(TFileStream.Create('/tmp/e14~', fmCreate));
  LoadFromFile('/tmp/e14~', e);

  e.MutableRf.Add(301); e.MutableRf.Add(-301);
  e.SaveToStream(TFileStream.Create('/tmp/e15~', fmCreate));
  LoadFromFile('/tmp/e15~', e);

  e.MutableRi32.Add(302); e.MutableRi32.Add(-302);
  e.SaveToStream(TFileStream.Create('/tmp/e16~', fmCreate));
  LoadFromFile('/tmp/e16~', e);

  e.MutableRi64.Add(303); e.MutableRi64.Add(-303);
  e.SaveToStream(TFileStream.Create('/tmp/e17~', fmCreate));
  LoadFromFile('/tmp/e17~', e);

  e.MutableRui32.Add(304); e.MutableRui32.Add(304304);
  e.SaveToStream(TFileStream.Create('/tmp/e18~', fmCreate));
  LoadFromFile('/tmp/e18~', e);

  e.MutableRui64.Add(305); e.MutableRui64.Add(305305305);
  e.SaveToStream(TFileStream.Create('/tmp/e19~', fmCreate));
  LoadFromFile('/tmp/e19~', e);

  e.MutableRsi32.Add(306); e.MutableRsi32.Add(-306306);
  e.SaveToStream(TFileStream.Create('/tmp/e20~', fmCreate));
  LoadFromFile('/tmp/e20~', e);

  e.MutableRsi64.Add(307); e.MutableRsi64.Add(-307307307);
  e.SaveToStream(TFileStream.Create('/tmp/e21~', fmCreate));
  LoadFromFile('/tmp/e21~', e);

  e.MutableRf32.Add(308); e.MutableRf32.Add(308308);
  e.SaveToStream(TFileStream.Create('/tmp/e22~', fmCreate));
  LoadFromFile('/tmp/e22~', e);

  e.MutableRf64.Add(309); e.MutableRf64.Add(+309309309);
  e.SaveToStream(TFileStream.Create('/tmp/e23~', fmCreate));
  LoadFromFile('/tmp/e23~', e);

  e.MutableRs32.Add(310); e.MutableRs32.Add(310310);
  e.SaveToStream(TFileStream.Create('/tmp/e24~', fmCreate));
  LoadFromFile('/tmp/e24~', e);

  e.MutableRs64.Add(311); e.MutableRs64.Add(-311311311);
  e.SaveToStream(TFileStream.Create('/tmp/e25~', fmCreate));
  LoadFromFile('/tmp/e25~', e);

  e.MutableRb.Add(False); e.MutableRb.Add(True); e.MutableRb.Add(True);
  e.SaveToStream(TFileStream.Create('/tmp/e26~', fmCreate));
  LoadFromFile('/tmp/e26~', e);

  e.MutableRs.Add('testing'); e.MutableRs.Add('-testingtesting');
  e.SaveToStream(TFileStream.Create('/tmp/e27~', fmCreate));
  LoadFromFile('/tmp/e27~', e);

  e.MutableAnIntIntMap.Add(1, 1);
  e.SaveToStream(TFileStream.Create('/tmp/e28~', fmCreate));
  LoadFromFile('/tmp/e28~', e);
  e.AnIntIntMap.Add(3, -102345); 
  e.MutableAnIntIntMap.Add(2, 102345); 
  e.SaveToStream(TFileStream.Create('/tmp/e29~', fmCreate));
  LoadFromFile('/tmp/e29~', e);

  e.IdSubMessageMap := TMyMessage.TInt32ToSubMessageMap.Create;
  e.IdSubMessageMap.Add(1, TMyMessage.TSubMessage.Create);
  e.IdSubMessageMap[1].Id := 1;
  e.SaveToStream(TFileStream.Create('/tmp/e30~', fmCreate));
  LoadFromFile('/tmp/e30~', e);

  e.IdSubMessageMap.Add(20, TMyMessage.TSubMessage.Create); 
  e.IdSubMessageMap[20].Id := 20;
  e.SaveToStream(TFileStream.Create('/tmp/e31~', fmCreate));
  LoadFromFile('/tmp/e31~', e);

  e.MyOneOf := TMyMessage.TMyOneOf.Create;
  e.MyOneOf.IsCity := 'Sari';
  e.SaveToStream(TFileStream.Create('/tmp/e32~', fmCreate));
  LoadFromFile('/tmp/e32~', e);

  e.MyOneOf.IsCountry := 'Iran';
  e.SaveToStream(TFileStream.Create('/tmp/e33~', fmCreate));
  LoadFromFile('/tmp/e33~', e);
	WriteLn(e.MyOneOf.IsCountry);
	WriteLn(e.MyOneOf.IsCity);
	WriteLn(e.MyOneOf.AnEnum);
	WriteLn(e.ConstA.GetAId());

	s := TMyMessage.TSubMessage.Create;
	s.Id := 234;
  e.A := TMyMessage.TA.Create;
	e.A.AId := 234;
	e.A.ASubMessage := s;
  e.SaveToStream(TFileStream.Create('/tmp/e34~', fmCreate));
  LoadFromFile('/tmp/e34~', e);

	e.IdAMap := TMyMessage.TInt32ToAMap.Create;
	e.IdAMap.Add(245, TMyMessage.TA.Create);
	e.IdAMap[245].Aid := 245;
  e.SaveToStream(TFileStream.Create('/tmp/e35~', fmCreate));
  LoadFromFile('/tmp/e35~', e);
  
	WriteLn('-Copied');
	ec := e.DeepCopy;
	WriteLn(ec.ToJSON);
	WriteLn('+Copied');
  ec.SaveToStream(TFileStream.Create('/tmp/ec1~', fmCreate));
 
  e.Free;
  ec.SaveToStream(TFileStream.Create('/tmp/ec2~', fmCreate));
  ec.Free;

end.
