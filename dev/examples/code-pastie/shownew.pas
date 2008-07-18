(****************************************************************************** 
   Code Pastie
 ******************************************************************************
   See LICENSE.TXT for this demo

   Author: Lars aka L505
           http://z505.com   
*******************************************************************************)

unit shownew; {$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface

procedure ShowNewPasties;

implementation

uses
  pwmain, pwdirutil, pwsubstr, pwenvvar, pwtypes;


type
  TStrArray = array of string;
  PPastes = ^TPastes;
  TPastes = record
    items: AStrArray;
    add: procedure(ASelf: PPastes; const s: string);
  end;

procedure AddPasteId(ASelf: PPastes; const s: string);
var
  oldlen, newlen: integer;
begin
  if s = '' then exit;
  // append string to array
  oldlen:= length(ASelf^.items);
  newlen:= oldlen+1;
  setlength(ASelf^.items, newlen);
  ASelf^.items[newlen-1]:= s; 
end;

procedure FlushWebBrowser;
begin
  out('<small>');
  out('<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  out('</small>');
  out('<br>');
end;

procedure ShowNewPasties;
  
  function MakePastieLink(id: string): string;
  begin
    result:= '<a href="' + SERV.ScriptName() + '?p=' + id +'">Id #' + id + '</a>';
  end;

var
  Pastes: TPastes {= (items: nil; add: @AddPasteId)};

  procedure GetAllPastieIds;
    procedure TrimHtmExtension;
    var i: integer;
    begin
      for i:= low(Pastes.items) to high(Pastes.items) do
        setlength(Pastes.items[i], length(Pastes.items[i]) - 4); // deletes .htm extension
    end;
  var
    fnames: TFileNames;
  begin
    GetFiles('pastes' + DirectorySeparator, Fnames);
    Pastes.items:= fnames.files;
    TrimHtmExtension;
  end;

var
  i: integer;
begin
  pastes.items:= nil; pastes.add:= {$ifdef fpc}@{$endif}AddPasteId;
  out('<br>May take a while...');
  out('<br>Showing all pastie ID''s...');
  // hopefully trigger web browser flush
  FlushWebBrowser;
  out('<hr style="border-style:dashed;">');
  // now find all pasties in sub directory
  GetAllPastieIds;
  // display pasties
  for i:= low(pastes.items) to high(pastes.items) do
  begin
    out('<p />');
    if pastes.items[i] <> 'main' then // we don't want main shown since it is main page, not a pastie
      out(MakePastieLink(pastes.items[i]));
  end;
end;

end.
