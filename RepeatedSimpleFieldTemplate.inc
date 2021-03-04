const
  DeclareRepeatedSimpleFieldTemplate: AnsiString =
    '[[Indent]]public type' + sLineBreak +
    '[[Indent]]  T[[Field.CanName]] =  T[[Field.ShortInnerFPCType]]s;' + sLineBreak + sLineBreak +
    '[[Indent]]private' + sLineBreak +
    '[[Indent]]  F[[Field.CanName]]: T[[Field.CanName]];' + sLineBreak + sLineBreak +
    '' + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  function Get[[Field.CanName]]: T[[Field.CanName]];' + sLineBreak +
    '[[Indent]]  function GetOrCreate[[Field.CanName]]: T[[Field.CanName]];' + sLineBreak + sLineBreak +
    '[[Indent]]public' + sLineBreak +
    '[[Indent]]  // repeated [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
    '[[Indent]]  property [[Field.CanName]]: T[[Field.CanName]] read F[[Field.CanName]] write F[[Field.CanName]];' + sLineBreak +
    '[[Indent]]  property Const[[Field.CanName]]: T[[Field.CanName]] read Get[[Field.CanName]];' + sLineBreak +
    '[[Indent]]  property Mutable[[Field.CanName]]: T[[Field.CanName]] read GetOrCreate[[Field.CanName]] write F[[Field.CanName]];' + sLineBreak;

  ImplementRepeatedSimpleFieldTemplate : AnsiString =
    sLineBreak +
    'function [[Field.ClassName]].Get[[Field.CanName]]: T[[Field.CanName]];' + sLineBreak +
    'begin' + sLineBreak +
    '  if Self = nil then' + sLineBreak +
    '    Exit([[Field.DefaultValue]]);' + sLineBreak + sLineBreak +
    '  Result := F[[Field.CanName]]; ' + sLineBreak + sLineBreak +
    'end;' + sLineBreak + sLineBreak +
    'function [[Field.ClassName]].GetOrCreate[[Field.CanName]]: T[[Field.CanName]];' + sLineBreak +
    '' + sLineBreak +
    'begin' + sLineBreak +
    '  if Self = nil then' + sLineBreak +
    '    Exit([[Field.DefaultValue]]);' + sLineBreak + sLineBreak +
    '  if F[[Field.CanName]] = nil then' + sLineBreak +
    '    F[[Field.CanName]] := T[[Field.CanName]].Create;' + sLineBreak +
    '  Result := F[[Field.CanName]];'+ sLineBreak + sLineBreak +
    'end;' + sLineBreak;
