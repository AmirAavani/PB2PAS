const
 DeclareRepeatedNonSimpleFieldTemplate : AnsiString =
  '  // Methods for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '  private' + sLineBreak +
  '    F[[CanName]]: specialize TObjectList<[[FieldType]]>;' + sLineBreak +
  '' + sLineBreak +
  '    // Getter Functions' + sLineBreak +
  '    function Get[[CanName]](Index: Integer): [[FieldType]];' + sLineBreak +
  '    function GetAll[[CanName]]: specialize TObjectList<[[FieldType]]>;' + sLineBreak +
  '    function GetOrCreateAll[[CanName]]: specialize TObjectList<[[FieldType]]>;' + sLineBreak +
  '' + sLineBreak +
  '  public' + sLineBreak +
  '    property [[CanName]][Index: Integer]: [[FieldType]] read Get[[CanName]];' + sLineBreak +
  '    property ConstAll[[CanName]]: specialize TObjectList<[[FieldType]]> read GetAll[[CanName]];' + sLineBreak +
  '    property All[[CanName]]: specialize TObjectList<[[FieldType]]> read GetOrCreateAll[[CanName]];' + sLineBreak;

  ImplementRepeatedNonSimpleFieldTemplate : AnsiString =
  '// Methods for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '// Getter Functions' + sLineBreak +
  'function [[ClassName]].Get[[CanName]](Index: Integer): [[FieldType]];' + sLineBreak +
  'begin' + sLineBreak +
  '  Result := F[[CanName]][Index];' +
  'end;' + sLineBreak + sLineBreak +
  'function [[ClassName]].GetAll[[CanName]]: specialize TObjectList<[[FieldType]]>;' + sLineBreak +
  'begin' + sLineBreak +
  '  if Self = nil then' + sLineBreak +
  '    Exit(nil);' + sLineBreak +
  '  if F[[CanName]] = nil then' + sLineBreak +
  '    Exit(nil);' + sLineBreak +
  '  Result := F[[CanName]]; ' + sLineBreak +
  'end;' + sLineBreak + sLineBreak +
  'function [[ClassName]].GetOrCreateAll[[CanName]]: specialize TObjectList<[[FieldType]]>;' + sLineBreak +
  '' + sLineBreak +
  'begin' + sLineBreak +
  '  if Self = nil then' + sLineBreak +
  '    Exit(nil);' + sLineBreak +
  '  if Self.F[[CanName]] = nil then' + sLineBreak +
  '    F[[CanName]] := specialize TObjectList<[[FieldType]]>.Create;' + sLineBreak +
  '  Result := F[[CanName]]; ' + sLineBreak +
  'end;' + sLineBreak + sLineBreak;

  ImplementRepeatedNonSimpleFieldToStringTemplate : AnsiString =
  '  if F[[CanName]] <> nil then' + sLineBreak +
  '  begin' + sLineBreak +
  '    Result += ''[[Field.Name]] = '';' + sLineBreak +
  '    Result += F[[CanName]].ToString;' + sLineBreak +
  '    Result += sLineBreak;' + sLineBreak +
  '  end;' + sLineBreak;

