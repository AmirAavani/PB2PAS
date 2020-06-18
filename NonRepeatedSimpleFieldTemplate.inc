const
 DeclareNonRepeatedSimpleFieldTemplate : AnsiString =
'  // Declarations for [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
'  private' + sLineBreak +
'    F[[CanName]]: [[Field.PackageNameWithDot]][[Field.FPCType]];' + sLineBreak +
'  public' + sLineBreak +
'    property [[CanName]]: [[Field.PackageNameWithDot]][[Field.FPCType]] read F[[CanName]] write F[[CanName]];' + sLineBreak;
 ImplementNonRepeatedSimpleFieldToStringTemplate : AnsiString =
 '  if F[[CanName]] <> [[Field.DefaultValue]] then' + sLineBreak +
 '  begin' + sLineBreak +
 '    Result += Format(''[[Field.Name]]: %[[FormatString]] '', [F[[CanName]]]);' + sLineBreak +
 '    Result += sLineBreak;' + sLineBreak +
 '  end;' + sLineBreak;
