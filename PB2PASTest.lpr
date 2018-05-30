program PB2PASTest;

{$mode objfpc}{$H+}
{$ASSERTIONS on}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, heaptrc, fgl, SampleUnit, ProtoHelperUnit
  { you can add units after this };

{
function EmptyProto: Boolean;
var
  Document: TDocument;
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create('Sample.bin', fmCreate);

  Document := TDocument.Create;
  // Document.SaveToStream(FileStream);
  FileStream.Free;
  Document.Free;

  Document := TDocument.Create;
  FileStream := TFileStream.Create('Sample.bin', fmOpenRead);
  // Document.LoadFromStream(FileStream);
  Assert(Document.ToString = '');
  Document.Free;
  FileStream.Free;

  Result := True;
end;


function DocWithTitle: Boolean;
var
  Document: TDocument;
  FileStream: TFileStream;

begin
  { Empty Title}
  Document := TDocument.Create;
  Assert(Document.Title.ConstAllTokens.Count = 0);
  Document.Title := TTitle.Create;
  Assert(Document.Title.ConstAllTokens.Count = 0);
  Assert(Document.ToString = '');

  Assert(Document.Title.ConstAllTokens = nil);

  Document.Title.AllTokens.Add(TToken.Create(1, 'a', nil, 1));
  Document.Free;

  Result := True;

end;


begin
  EmptyProto;
  DocwithTitle;

end.
}

var
  IDList, IDList2: TTestID;
  P: TParams;

begin
  IDList := TTestID.Create;

  IDList.id32 := 4;
  IDList.AllId32s.Add(2);
  IDList.AllId32s.Add(127);
  IDList.AllId32s.Add(128);
  IDList.AllId32s.Add(130);
  Assert(IDList.ConstAllId32s.Count = 4);

  IDList.IdFloat := 9999e5;
  IDList.AllIdFloats.Add(12.3);
  IDList.AllIdFloats.Add(14.3);

  IDList.IdDouble := 9999e5;
  IDList.AllIdDoubles.Add(120.3);
  IDList.AllIdDoubles.Add(140.3);

  IDList.IdStr := 'amirRIMA';
  IDList.AllIdStrs.Add('1amir');
  IDList.AllIdStrs.Add('a1mir');
  IDList.AllIdStrs.Add('am1ir');
  IDList.AllIdStrs.Add('ami1r');
  IDList.AllIdStrs.Add('amir1');

  IDList.id64 := 3;
  IDList.AllId64s.Add(-20123);
  IDList.AllId64s.Add(-201230);

  IDList.Uid64 := 1;
  IDList.AllUId64s.Add(20123);
  IDList.AllUid64s.Add(201230);

  IDList.Parameter := TParams.Create;
  IDList.Parameter.AllParamStr.Add('a');
  IDList.Parameter.AllParamStr.Add('b');
  IDList.Parameter.AllParamStr.Add('c');

  p := TParams.Create;
  p.A32 := 123;
  p.AllParamStr.Add('a');
  p.AllParamStr.Add('b');
  p.AllParamStr.Add('c');
  IDList.AllParameters.Add(p);

  p := TParams.Create;
  p.A32 := 112233;
  p.AllParamStr.Add('Aa');
  p.AllParamStr.Add('Bb');
  p.AllParamStr.Add('Cc');
  IDList.AllParameters.Add(p);

  p := TParams.Create;
  p.A32 := 111222;
  p.AllParamStr.Add('1Aa');
  p.AllParamStr.Add('2Bb');
  IDList.AllParameters.Add(p);

  IDList.Uid32 := (1 shl 31) + (1 shl 28);
  IDList.AllUid32s.Add((1 shl 31) + (1 shl 30));
  IDList.AllUid32s.Add(1 shl 31 + 1 shl 29);

  IDList.IDBool := True;
  IDList.AllIdBools.Add(False);
  IDList.AllIdBools.Add(True);
  IDList.AllIdBools.Add(False);

  WriteLn(IDList.ToString);
  IDList.SaveToStream(TFileStream.Create('/tmp/t1.out', fmCreate));

  IDList2 := TTestID.Create;

  IDList2.LoadFromStream(TFileStream.Create('/tmp/t1.out', fmOpenRead));
  if IDList.ToString <> IDList2.ToString then
  begin
    WriteLn(IDList.ToString);
    WriteLn(IDList2.ToString);
    assert(False);
  end;
  IDList.Free;
  IDList2.Free;
end.
