const
  DeclareNonRepeatedNonSimpleFieldTemplate: AnsiString =
    '[[Indent]]private' + sLineBreak +
    '[[Indent]]  F[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak +
    '' + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  function Get[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak  +
    '[[Indent]]  function GetOrCreate[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak  + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  // [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
    '[[Indent]]  property [[Field.CanName]]: [[Field.ShortFPCType]] read F[[Field.CanName]] write F[[Field.CanName]];' + sLineBreak +
    '[[Indent]]  property Const[[Field.CanName]]: [[Field.ShortFPCType]] read Get[[Field.CanName]];' + sLineBreak +
    '[[Indent]]  property Mutable[[Field.CanName]]: [[Field.ShortFPCType]] read GetOrCreate[[Field.CanName]];' + sLineBreak;

  ImplementNonRepeatedNonSimpleFieldTemplate: AnsiString =
    'function [[Field.ClassName]].Get[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak +
    'begin' + sLineBreak +
    '  if Self = nil then' + sLineBreak +
    '    Exit([[Field.DefaultValue]]);' + sLineBreak + sLineBreak +
    '  Result := F[[Field.CanName]]; ' + sLineBreak + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'function [[Field.ClassName]].GetOrCreate[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak +
    'begin' + sLineBreak +
    '  if Self = nil then' + sLineBreak +
    '    Exit([[Field.DefaultValue]]);' + sLineBreak + sLineBreak +
    '  if Self.F[[Field.CanName]] = nil then' + sLineBreak +
    '    F[[Field.CanName]] := [[Field.ShortFPCType]].Create;' + sLineBreak + sLineBreak +
    '  Result := F[[Field.CanName]]; ' + sLineBreak + sLineBreak +
    'end;' + sLineBreak + sLineBreak;

