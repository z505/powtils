// ~NRCOL
Unit MIMETable;

Interface

Function FindMIMEOf(Ext : String): String;

Implementation
Uses Classes, BreakTokens;

Var
  MIMEList : TStringList;

Function FindMIMEOf(Ext : String): String;
Begin
  FindMIMEOf := UnQuote(MIMEList.Values[Ext]);
End;

Initialization

  MIMEList            := TStringList.Create;
  MIMEList.Duplicates := dupIgnore;
  MIMEList.Sorted     := True;
  MIMEList.Delimiter  := ' ';
  MIMEList.QuoteChar  := #00;
  MIMEList.LoadFromFile('mime.tab');

Finalization

  MIMEList.Free;

End.
