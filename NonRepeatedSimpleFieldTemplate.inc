const
  DeclareNonRepeatedSimpleFieldTemplate : AnsiString =
    '[[Indent]]private' + sLineBreak +
    '[[Indent]]  F[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  // [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
    '[[Indent]]  property [[Field.CanName]]: [[Field.ShortFPCType]] read F[[Field.CanName]] write F[[Field.CanName]];' + sLineBreak;

