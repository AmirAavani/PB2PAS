const
 DeclareNonRepeatedNonSimpleFieldTemplate : AnsiString =
 '  // Declarations for [[Field.Type]] [[Field.Name]] = [[Field.Number]];' + sLineBreak +
 '  private' + sLineBreak +
 '    F[[CanName]]: [[FieldType]];' + sLineBreak +
 '  public' + sLineBreak +
 '    property [[CanName]]: [[FieldType]] read F[[CanName]] write F[[CanName]];' + sLineBreak;
 ImplementNonRepeatedNonSimpleFieldToStringTemplate : AnsiString =
  '  if F[[CanName]] <> nil then' + sLineBreak +
  '  begin' + sLineBreak +
  '    Result += ''[[Field.Name]] = '';' + sLineBreak +
  '    Result += F[[CanName]].ToString;' + sLineBreak +
  '    Result += sLineBreak;' + sLineBreak +
  '  end;' + sLineBreak;

