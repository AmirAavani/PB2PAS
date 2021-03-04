const
  DeclareNonRepeatedSimpleFieldTemplate: AnsiString =
    '[[Indent]]private' + sLineBreak +
    '[[Indent]]  F[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  function Get[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  // [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
    '[[Indent]]  property [[Field.CanName]]: [[Field.ShortFPCType]] read F[[Field.CanName]] write F[[Field.CanName]];' + sLineBreak;

    ImplementNonRepeatedSimpleFieldTemplate: AnsiString =
    'function [[Field.ClassName]].Get[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak +
    'begin' + sLineBreak +
    '  if Self = nil then' + sLineBreak +
    '    Exit([[Field.DefaultValue]]);' + sLineBreak + sLineBreak +
    '  Result := F[[Field.CanName]]; ' + sLineBreak + sLineBreak +
    'end;' + sLineBreak + sLineBreak;
