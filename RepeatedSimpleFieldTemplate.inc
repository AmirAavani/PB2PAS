const
 DeclareRepeatedSimpleFieldTemplate : AnsiString =
  '  // Methods for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '  private' + sLineBreak +
  '    F[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
  '' + sLineBreak +
  '    // Getter Functions' + sLineBreak +
  '    function Get[[CanName]](Index: Integer): [[FieldType]];' + sLineBreak +
  '    function GetAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
  '    function GetOrCreateAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
  '' + sLineBreak +
  '  public' + sLineBreak +
  '    property [[CanName]][Index: Integer]: [[FieldType]] read Get[[CanName]];' + sLineBreak +
  '    property ConstAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]> read GetAll[[CanName]];' + sLineBreak +
  '    property All[[CanName]]: specialize TSimpleTypeList<[[FieldType]]> read GetOrCreateAll[[CanName]];' + sLineBreak;

 ImplementRepeatedSimpleFieldTemplate : AnsiString =
   '// Methods for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '// Getter Functions' + sLineBreak + sLineBreak +
   'function [[ClassName]].Get[[CanName]](Index: Integer): [[FieldType]];' + sLineBreak +
   'begin' + sLineBreak +
   '  Result := F[[CanName]][Index];' + sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak +
   'function [[ClassName]].GetAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
   'begin' + sLineBreak +
   '  if Self = nil then' + sLineBreak +
   '    Exit(nil);' + sLineBreak +
   '  Result := F[[CanName]];'+ sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak +
   'function [[ClassName]].GetOrCreateAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
   '' + sLineBreak +
   'begin' + sLineBreak +
   '  if F[[CanName]] = nil then' + sLineBreak +
   '    F[[CanName]] := specialize TSimpleTypeList<[[FieldType]]>.Create(''%[[FormatString]]'');' + sLineBreak +
   '  Result := F[[CanName]];'+ sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak;

 ImplementRepeatedBooleanTemplate : AnsiString =
   '// Methods for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '// Getter Functions' + sLineBreak + sLineBreak +
   'function [[ClassName]].Get[[CanName]](Index: Integer): [[FieldType]];' + sLineBreak +
   'begin' + sLineBreak +
   '  Result := F[[CanName]][Index];' + sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak +
   'function [[ClassName]].GetAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
   'begin' + sLineBreak +
   '  if Self = nil then' + sLineBreak +
   '    Exit(nil);' + sLineBreak +
   '  Result := F[[CanName]];'+ sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak +
   'function [[ClassName]].GetOrCreateAll[[CanName]]: specialize TSimpleTypeList<[[FieldType]]>;' + sLineBreak +
   '' + sLineBreak +
   'begin' + sLineBreak +
   '  if F[[CanName]] = nil then' + sLineBreak +
   '    F[[CanName]] := TBooleanList.Create(''%[[FormatString]]'');' + sLineBreak +
   '  Result := F[[CanName]];'+ sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak;

 ImplementRepeatedSimpleFieldToStringTemplate : AnsiString =
   '  if F[[CanName]] <> nil then' + sLineBreak +
   '  begin' + sLineBreak +
   '    Result += ''[[Field.Name]] = '';' + sLineBreak +
   '    Result += F[[CanName]].ToString;' + sLineBreak +
   '    Result += sLineBreak;' + sLineBreak +
   '  end;' + sLineBreak;

