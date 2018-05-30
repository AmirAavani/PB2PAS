const
 DeclareNonRepeatedSimpleFieldTemplate : AnsiString =
'  // Declarations for [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
'  private' + sLineBreak +
'    F[[CanName]]: [[FieldType]];' + sLineBreak +
'  public' + sLineBreak +
'    property [[CanName]]: [[FieldType]] read F[[CanName]] write F[[CanName]];' + sLineBreak;
 ImplementNonRepeatedSimpleFieldToStringTemplate : AnsiString =
 '  if F[[CanName]] <> [[Field.DefaultValue]] then' + sLineBreak +
 '  begin' + sLineBreak +
 '    Result += Format(''[[Field.Name]]: %[[FormatString]] '', [F[[CanName]]]);' + sLineBreak +
 '    Result += sLineBreak;' + sLineBreak +
 '  end;' + sLineBreak;
