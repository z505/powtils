// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit XMLLoader;

Interface

Uses
  Classes,
  SysUtils,
  XMLBase,
  XMLParser;

Type
  EXMLLoader = Class(Exception);

Function XMLLoad(Source: TTokenIterator;
  NameSpace: TXMLNodeList): TXMLTagCollection;

Implementation

Function XMLLoad(Source: TTokenIterator;
  NameSpace: TXMLNodeList): TXMLTagCollection;
Var
  RootTag: TXMLTagCollection;

  Procedure ParseTag(Previous: TXMLTagCollection); Forward;

  Procedure ParseTagOrText(Previous: TXMLTagCollection; Finishing: String);
  Begin
    While Not (Source.IsEOS) Do
      If Source.TkType = ccXMLParser_BGTag Then
        Begin
        Source.Skip;
        If Source.TkType = ccXMLParser_Slash Then
          Begin
          Source.Skip;
          If Source.TkType = ccXMLParser_Ident Then
            If Source.Token = Finishing Then
              Begin
              Source.Skip;
              If Source.TkType <> ccXMLParser_EDTag Then
                Raise EXMLLoader.Create('Expected > at ' +
                  IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
              Source.Skip;
              Exit;
              End
            Else
              Raise EXMLLoader.Create(Source.Token + ' cannot end ' +
                Finishing + ' sequence of childs at ' +
                IntToStr(Source.Row) + ',' + IntToStr(Source.Col))
          Else
            Raise EXMLLoader.Create('Expected tag name at ' +
              IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
          End
        Else
          ParseTag(Previous);
        End
      Else
        Begin
        Previous.AppendChild(TXMLText.Create(Source.Token));
        Source.Skip;
        End;
  End;

  Procedure ParseTag(Previous: TXMLTagCollection);
  Var
    TagName:   String;
    TagParameters: TStringList;
    TempTag:   TXMLTag;
    ValuePair: Record
      Name, Value : String;
    End;
    HasChilds: Boolean;
  Begin
    TagParameters := TStringList.Create;
    TagParameters.Sorted := False;
    TagParameters.Duplicates := dupIgnore;
    TagParameters.CaseSensitive := False;
    TagParameters.Delimiter := ' ';
    TagParameters.QuoteChar := #00;
    If Source.TkType <> ccXMLParser_Ident Then
      Raise EXMLLoader.Create('Expected tag name at ' +
        IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
    TagName := Source.Token;
    Source.Skip;
    While Not (Source.TkType In [ccXMLParser_EDTag, ccXMLParser_Slash]) And
      Not (Source.IsEOS) Do
      Begin
      If Source.TkType <> ccXMLParser_Ident Then
        Raise EXMLLoader.Create('Expected attribute name at ' +
          IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
      ValuePair.Name := Source.Token;
      Source.Skip;
      If Source.TkType <> ccXMLParser_Equal Then
        Raise EXMLLoader.Create('Expected = at ' +
          IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
      Source.Skip;
      If Source.TkType <> ccXMLParser_Str Then
        Raise EXMLLoader.Create('Expected attribute value at ' +
          IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
      ValuePair.Value := Source.Token;
      Source.Skip;
      TagParameters.Add(ValuePair.Name + '="' + ValuePair.Value + '"');
      End;
    Case Source.TkType Of
      ccXMLParser_EDTag:
        HasChilds := True;
      ccXMLParser_Slash:
        Begin
        HasChilds := False;
        Source.Skip;
        If Source.TkType <> ccXMLParser_EDTag Then
          Raise EXMLLoader.Create('Expected > at ' +
            IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
        End;
      Else
        Raise EXMLLoader.Create('Unexpected end of file at ' +
          IntToStr(Source.Row) + ',' + IntToStr(Source.Col));
      End;
    Source.Skip;
    If NameSpace.Find(TagName) Then
      TempTag := TXMLTag.Create(TagName, HasChilds, NameSpace[TagName])
    Else
      TempTag := TXMLTag.Create(TagName, HasChilds, Nil);
    TempTag.Attributes.DelimitedText := TagParameters.DelimitedText;
    TagParameters.Free;
    If HasChilds Then
      ParseTagOrText(TempTag, TagName);
    Previous.AppendChild(TempTag);
  End;

Begin
  RootTag := TXMLTagCollection.Create;
  While Not (Source.IsEOS) Do
    If Source.TkType = ccXMLParser_BGTag Then
      Begin
      Source.Skip;
      If Source.TkType = ccXMLParser_Slash Then
        Raise EXMLLoader.Create('Cannot start template with end tag marker ' +
          IntToStr(Source.Row) + ',' + IntToStr(Source.Col))
      Else
        ParseTag(RootTag);
      End
    Else
      Begin
      RootTag.AppendChild(TXMLText.Create(Source.Token));
      Source.Skip;
      End;
  XMLLoad := RootTag;
End;

End.
