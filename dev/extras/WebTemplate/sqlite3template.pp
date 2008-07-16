// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit SQLite3Template;

Uses
  PWMain,
  XMLBase,
  WebTemplate,
  DB,
  SQLite3DS;

Interface

Procedure RegisterToTemplate(Template : TWebTemplate);

Implementation

Var
  DS : TSQLite3DataSet;

Procedure SQLite3DataSource(Caller : TXMLTag);
Begin
  DS := TSQLite3DataSet.Create(Nil);
  DS.FileName := Caller.GetAttribute('filename');
  DS.TableName := Caller.GetAttribute('table');
  DS.PrimaryKey := Caller.GetAttribute('key');
  Caller.EmitChilds;
  DS.Free;
End;

Procedure SQLite3Query(Caller : TXMLTag);
Begin
  DS.SQL := Caller.GetAttribute('query');
  DS.Open;
  Caller.EmitChilds;
  DS.Close;
End;

Procedure SQLite3Iterate(Caller : TXMLTag);
Begin
  DS.First;
  While Not(DS.EOF) Do
  Begin
    Caller.EmitChilds;
    DS.Next;
  End;
End;

Procedure SQLite3MakeVar(Caller : TXMLTag);
Begin
  SetVar(Caller.GetAttribute('becomes'),
    DS.FieldByName(
      Caller.GetAttribute('name')
    ).AsString
  );
End;

Procedure RegisterToTemplate(Template : TWebTemplate);
Begin
  Template.Tag['datasource'] := SQLite3DataSource;
  Template.Tag['query']      := SQLite3Query;
  Template.Tag['iterate']    := SQLite3Iterate;
  Template.Tag['makevar']    := SQLite3MakeVar;
End;

End.
