const
 DeclareRepeatedNonSimpleFieldTemplate : AnsiString =
  '  // Declarations for repeated [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '  public type' + sLineBreak +
  '    T[[CanName]] = specialize TObjectList<[[Field.UnitNameWithDot]][[Field.InnerFPCType]]>;' + sLineBreak + sLineBreak +
  '  private' + sLineBreak +
  '    F[[CanName]]: T[[CanName]];' + sLineBreak +
  '' + sLineBreak +
  '    // Getter Functions' + sLineBreak +
  '    function Get[[CanName]](Index: Integer): [[Field.UnitNameWithDot]][[Field.InnerFPCType]];' + sLineBreak +
  '    function Get[[CanName]]: T[[CanName]];' + sLineBreak +
  '    function GetOrCreate[[CanName]]: T[[CanName]];' + sLineBreak +
  '' + sLineBreak +
  '  public' + sLineBreak +
  '    property [[CanName]][Index: Integer]: [[Field.UnitNameWithDot]][[Field.InnerFPCType]] read Get[[CanName]];' + sLineBreak +
  '    property Const[[CanName]]: T[[CanName]] read Get[[CanName]];' + sLineBreak +
  '    property Mutable[[CanName]]: T[[CanName]] read GetOrCreate[[CanName]];' + sLineBreak;

  ImplementRepeatedNonSimpleFieldTemplate : AnsiString =
   sLineBreak +
  '// Implementation for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '// Getter Functions' + sLineBreak + sLineBreak +
  'function [[ClassName]].Get[[CanName]](Index: Integer): [[Field.UnitNameWithDot]][[Field.InnerFPCType]];' + sLineBreak +
  'begin' + sLineBreak +
  '  Result := F[[CanName]][Index];' + sLineBreak + sLineBreak +
  'end;' + sLineBreak + sLineBreak +
  'function [[ClassName]].Get[[CanName]]: T[[CanName]];' + sLineBreak +
  'begin' + sLineBreak +
  '  if Self = nil then' + sLineBreak +
  '    Exit(nil);' + sLineBreak + sLineBreak +
  '  if F[[CanName]] = nil then' + sLineBreak +
  '    Exit(nil);' + sLineBreak +  sLineBreak +
  '  Result := F[[CanName]]; ' + sLineBreak + sLineBreak +
  'end;' + sLineBreak + sLineBreak +
  'function [[ClassName]].GetOrCreate[[CanName]]: T[[CanName]];' + sLineBreak +
  'begin' + sLineBreak +
  '  if Self = nil then' + sLineBreak +
  '    Exit(nil);' + sLineBreak + sLineBreak +
  '  if Self.F[[CanName]] = nil then' + sLineBreak +
  '    F[[CanName]] := T[[CanName]].Create;' + sLineBreak + sLineBreak +
  '  Result := F[[CanName]]; ' + sLineBreak + sLineBreak +
  'end;' + sLineBreak + sLineBreak;

  ImplementRepeatedNonSimpleFieldToStringTemplate : AnsiString =
  '  if F[[CanName]] <> nil then' + sLineBreak +
  '  begin' + sLineBreak +
  '    Result += ''[[Field.Name]] = '';' + sLineBreak +
  '    Result += F[[CanName]].ToString;' + sLineBreak +
  '    Result += sLineBreak;' + sLineBreak +
  '  end;' + sLineBreak;

