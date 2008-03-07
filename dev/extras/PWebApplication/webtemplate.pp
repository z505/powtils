// ~NRCOL

{****************************************************
 See Licence.txt for details
 Copyright(C) 2007 2008 - Jorge Aldo G. de F. Junior
 Colaborators:
 <put your name here if you improve this software>
*****************************************************}

Unit WebTemplate;

Interface

Uses
  Classes,
  PWMain,
  SysUtils,
  XMLBase,
  XMLLoader,
  XMLParser;

Type
  EWebTemplate = Class(Exception);

  TWebTemplate = Class
  Protected
    fFileName:  String;
    fNameSpace: TXMLNodeList;
    fRootTag:   TXMLTagCollection;
    fLoaded:    Boolean;
  Public
    Constructor Create(Arq: String);
    Destructor Destroy; Override;
    Procedure Load;
    Procedure Emit;
    Procedure LoadAndEmit;
    Property NameSpace: TXMLNodeList Read fNameSpace;
    Property Tag: TXMLNodeList Read fNameSpace;
  End;

Implementation

Constructor TWebTemplate.Create(Arq: String);
Begin
  Inherited Create;
  fFileName  := Arq;
  fLoaded    := False;
  fNameSpace := TXMLNodeList.Create;
End;

Destructor TWebTemplate.Destroy;
Begin
  fNameSpace.Free;
  Inherited Destroy;
End;

Procedure TWebTemplate.Load;
Var
  fSourceTokens:  TTokenList;
  fSourceStream:  TFileStream;
  fTokenIterator: TTokenIterator;
Begin
  Try
    fSourceStream := TFileStream.Create(fFileName, fmOpenRead);
    fSourceTokens := ParseXML(fSourceStream);
    fTokenIterator := TTokenIterator.Create(fSourceTokens);
    fRootTag := XMLLoad(fTokenIterator, fNameSpace);
    fLoaded  := True;
  Except
  On E: Exception Do
    WriteLn(fFileName, ': ', E.Message);
  End;
  fSourceStream.Free;
  fTokenIterator.Free;
End;

Procedure TWebTemplate.Emit;
Begin
  If fLoaded Then
    fRootTag.Emit
  Else
    Raise EWebTemplate.Create('Cannot emit until a template is loaded !');
End;

Procedure TWebTemplate.LoadAndEmit;
Begin
  Load;
  fRootTag.Emit;
End;

End.
