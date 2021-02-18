const
 DeclareNonRepeatedSimpleFieldTemplate : AnsiString =
'[[Indent]]// Declarations for [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
'[[Indent]]private' + sLineBreak +
'[[Indent]]  F[[Field.CanName]]: [[Field.ShortFPCType]];' + sLineBreak +
'[[Indent]]public' + sLineBreak +
'[[Indent]]  property [[Field.CanName]]: [[Field.ShortFPCType]] read F[[Field.CanName]] write F[[Field.CanName]];' + sLineBreak;
