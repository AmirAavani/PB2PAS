const
 DeclareNonRepeatedNonSimpleFieldTemplate: AnsiString =
 '  // Declarations for [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
 '  private' + sLineBreak +
 '    F[[CanName]]: [[Field.UnitNameAndFPCType]];' + sLineBreak +
 '    function GetOrCreate[[CanName]]: [[Field.UnitNameAndFPCType]];' + sLineBreak +
 '  public' + sLineBreak +
 '    property [[CanName]]: [[Field.UnitNameAndFPCType]] read F[[CanName]] write  F[[CanName]];' + sLineBreak +
 '    property Mutable[[CanName]]: [[Field.UnitNameAndFPCType]] read GetOrCreate[[CanName]];' + sLineBreak;

 ImplementNonRepeatedNonSimpleFieldTemplate: AnsiString =
 'function [[ClassName]].GetOrCreate[[CanName]]: [[Field.UnitNameAndFPCType]];' + sLineBreak +
 'begin' + sLineBreak +
 '  if Self = nil then' + sLineBreak +
 '    Exit(nil);' + sLineBreak + sLineBreak +
 '  if Self.F[[CanName]] = nil then' + sLineBreak +
 '    F[[CanName]] := [[Field.UnitNameAndFPCType]].Create;' + sLineBreak + sLineBreak +
 '  Result := F[[CanName]]; ' + sLineBreak + sLineBreak +
 'end;' + sLineBreak + sLineBreak;


