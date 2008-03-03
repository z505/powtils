// ~NRCOL
Unit PWVL;

Interface
Uses
  WebApplication,
  WebTemplate,
  XMLBase,
  PWMain,
  Sysutils,
  Classes;

Type
  TArrayGrid = Array Of Array Of String;

  TWebArrayGrid = Class(TWebComponent)
  Private
    fMatrix   : TArrayGrid;
    fCurRow,
    fCurCol   : LongInt;
    fEditable : Boolean;
    Function GridConditionals(Name : String): Boolean;
    Procedure GridRows(Caller : TXMLTag);
    Procedure GridCols(Caller : TXMLTag);
    Procedure GridElement(Caller : TXMLTag);
    Procedure GridHiddenColVar(Caller : TXMLTag);
    Procedure GridHiddenRowVar(Caller : TXMLTag);
    Procedure GridHiddenActionVar(Caller : TXMLTag);
    Procedure GridSubmitAsButton(Caller : TXMLTag);
    Procedure GridSubmitAsImage(Caller : TXMLTag);
    Procedure GridEditForm(Caller : TXMLTag);
    Procedure GridSubmit;
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Property Matrix   : TArrayGrid Read fMatrix Write fMatrix;
    Property Editable : Boolean Read fEditable Write fEditable;
  End;

  TWebMemo = Class(TWebComponent)
  Private
    fLines    : TStringList;
    fCurLine  : LongWord;
    fEditable : Boolean;
    Function MemoConditionals(Name : String): Boolean;
    Procedure MemoShow(Caller : TXMLTag);
    Procedure MemoLine(Caller : TXMLTag);
  Public
    Constructor Create(Name, Tmpl : String; Owner : TWebComponent);
    Property Lines    : TStringList Read fLines Write fLines;
    Property Editable : Boolean Read fEditable Write fEditable;
  End;


Implementation

Function TWebArrayGrid.GridConditionals(Name : String): Boolean;
Begin
  If Name = 'editmode' Then
    GridConditionals := GetCGIVar('action') = InstanceName + '_edit'
  Else
    GridConditionals := False;
End;

Procedure TWebArrayGrid.GridRows(Caller : TXMLTag);
Begin
  fCurRow := Low(fMatrix);
  While fCurRow <= High(fMatrix) Do
  Begin
    Caller.EmitChilds;
    Inc(fCurRow);
  End;
End;

Procedure TWebArrayGrid.GridCols(Caller : TXMLTag);
Begin
  fCurCol := Low(fMatrix[fCurRow]);
  While fCurCol <= High(fMatrix[fCurRow]) Do
  Begin
    Caller.EmitChilds;
    Inc(fCurCol);
  End;
End;

Procedure TWebArrayGrid.GridElement(Caller : TXMLTag);
Begin
  WebWrite(
  '<a href="' + SelfReference + '?action=' + InstanceName + '_edit&' +
  'row=' + IntToStr(fCurRow) + '&col=' + IntToStr(fCurCol) + '">');
  WebWrite(fMatrix[fCurRow][fCurCol]);
  WebWrite('</a>');
End;

Procedure TWebArrayGrid.GridHiddenColVar(Caller : TXMLTag);
Begin
  WebWrite('<input name="col" value="' + GetCGIVar('col') + '" type="hidden"/>');
End;

Procedure TWebArrayGrid.GridHiddenRowVar(Caller : TXMLTag);
Begin
  WebWrite('<input name="row" value="' + GetCGIVar('row') + '" type="hidden"/>');
End;

Procedure TWebArrayGrid.GridHiddenActionVar(Caller : TXMLTag);
Begin
  WebWrite('<input name="action" value="' + InstanceName + '_submit" type="hidden"/>');
End;

Procedure TWebArrayGrid.GridSubmitAsButton(Caller : TXMLTag);
Begin
  WebWrite('<input name="submit" value=' + Caller.Attributes.Values['name'] + ' type="submit"/>');
End;

Procedure TWebArrayGrid.GridSubmitAsImage(Caller : TXMLTag);
Begin
  WebWrite('<img src=' + UnQuote(Caller.Attributes.Values['name']) +
  'onclick="javascript: ' + InstanceName + '_form.submit()"/>');
End;

Procedure TWebArrayGrid.GridEditForm(Caller : TXMLTag);
Begin
  WebWrite('<form name="' + InstanceName + '_form" action="' + SelfReference + '">');
  Caller.EmitChilds;
  WebWrite('</form>');
End;

Procedure TWebArrayGrid.GridSubmit;
Begin
  If fEditable Then
    fMatrix[StrToInt(GetCGIVar('row'))][StrToInt(GetCGIVar('col'))] := GetCGIVar('value');
End;

Constructor TWebArrayGrid.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['grid'] := Self.GridRows;
  Template.Tag['gridrow'] := Self.GridCols;
  Template.Tag['gridelement'] := Self.GridElement;
  Template.Tag['gridcolvar'] := Self.GridHiddenColVar;
  Template.Tag['gridrowvar'] := Self.GridHiddenRowVar;
  Template.Tag['gridactionvar'] := Self.GridHiddenActionVar;
  Template.Tag['gridbutton'] := Self.GridSubmitAsButton;
  Template.Tag['gridimage'] := Self.GridSubmitAsImage;
  Template.Tag['grideditform'] := Self.GridEditForm;
  RegisterAction(InstanceName + '_submit', Self.GridSubmit);
  Template.Conditional := Self.GridConditionals;
End;

Function TWebMemo.MemoConditionals(Name : String): Boolean;
Begin
  MemoConditionals := False;
End;

Procedure TWebMemo.MemoShow(Caller : TXMLTag);
Begin
  fCurLine := 0;
  While fCurLine < fLines.Count Do
    Caller.EmitChilds;
End;

Procedure TWebMemo.MemoLine(Caller : TXMLTag);
Begin
  If Caller.Attributes.IndexOfName('class') <> -1 Then
    WebWrite('<span class=' + Caller.Attributes.Values['class'] + '>' +
    fLines[fCurLine] + '</span>')
  Else
    WebWrite(fLines[fCurLine]);
  Inc(fCurLine);
End;

Constructor TWebMemo.Create(Name, Tmpl : String; Owner : TWebComponent);
Begin
  Inherited Create(Name, Tmpl, Owner);
  Template.Tag['memo'] := Self.MemoShow;
  Template.Tag['memoline'] := Self.MemoLine;
  Template.Conditional := Self.MemoConditionals;
End;

End.
