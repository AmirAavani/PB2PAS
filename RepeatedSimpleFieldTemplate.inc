const
 DeclareRepeatedSimpleFieldTemplate: AnsiString =
  '  // Methods for repeated [[Field.PackageNameWithDot]][[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '  public type' + sLineBreak +
  '    T[[CanName]] =  T[[Field.InnerFPCType]]s;' + sLineBreak + sLineBreak +
  '  private' + sLineBreak +
  '    F[[CanName]]: T[[CanName]];' + sLineBreak + sLineBreak +
  '' + sLineBreak +
  '    // Getter Functions' + sLineBreak +
  '    function Get[[CanName]](Index: Integer): [[Field.UnitNameWithDot]][[Field.InnerFPCType]];' + sLineBreak +
  '    function GetAll[[CanName]]: T[[CanName]];' + sLineBreak +
  '    function GetOrCreate[[CanName]]: T[[CanName]];' + sLineBreak + sLineBreak +
  '  public' + sLineBreak +
  '    property [[CanName]][Index: Integer]: [[Field.UnitNameWithDot]][[Field.InnerFPCType]] read Get[[CanName]];' + sLineBreak +
  '    property Const[[CanName]]: T[[CanName]] read GetAll[[CanName]];' + sLineBreak +
  '    property Mutable[[CanName]]: T[[CanName]] read GetOrCreate[[CanName]];' + sLineBreak;

 ImplementRepeatedSimpleFieldTemplate : AnsiString =
  sLineBreak +
   '// Methods for repeated [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
  '// Getter Functions' + sLineBreak + sLineBreak +
   'function [[ClassName]].Get[[CanName]](Index: Integer): [[Field.UnitNameWithDot]][[Field.InnerFPCType]];' + sLineBreak +
   'begin' + sLineBreak +
   '  Result := F[[CanName]][Index];' + sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak +
   'function [[ClassName]].GetAll[[CanName]]: T[[CanName]];' + sLineBreak +
   'begin' + sLineBreak +
   '  if Self = nil then' + sLineBreak +
   '    Exit(nil);' + sLineBreak +
   '  Result := F[[CanName]];'+ sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak +
   'function [[ClassName]].GetOrCreate[[CanName]]: T[[CanName]];' + sLineBreak +
   '' + sLineBreak +
   'begin' + sLineBreak +
   '  if F[[CanName]] = nil then' + sLineBreak +
   '    F[[CanName]] := T[[CanName]].Create;' + sLineBreak +
   '  Result := F[[CanName]];'+ sLineBreak + sLineBreak +
   'end;' + sLineBreak + sLineBreak;

 ImplementRepeatedSimpleFieldToStringTemplate : AnsiString =
   '  if F[[CanName]] <> nil then' + sLineBreak +
   '  begin' + sLineBreak +
   '    Result += ''[[Field.Name]] = '';' + sLineBreak +
   '    Result += F[[CanName]].ToString;' + sLineBreak +
   '    Result += sLineBreak;' + sLineBreak +
   '  end;' + sLineBreak;

