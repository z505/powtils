(****************************************************************************** 
   Code Pastie
 ******************************************************************************
   See LICENSE.TXT for this demo

   Author: Lars aka L505
           http://z505.com   
*******************************************************************************)

unit shownew; {$mode objfpc} {$H+}

interface

procedure ShowNewPasties;

implementation

uses
  pwmain, pwdirutil, pwsubstr, pwenvvar;

type
  TStrArray = array of string;
  PPastes = ^TPastes;
  TPastes = record
    items: TStrArray;
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

{ tricks some web browsers into flushing output, maybe if they detect Kilobytes in buffer }
procedure FlushWebBrowser;
begin
  webwrite('<small>');
  webwrite('<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;');
  webwrite('</small>');
  webwrite('<br>');
end;

procedure ShowNewPasties;
  
  function MakePastieLink(id: string): string;
  begin
    result:= '<a href="' + CgiEnvVars.ScriptName + '?p=' + id +'">Id #' + id + '</a>';
  end;

var 
  Pastes: TPastes = (items: nil; add: @AddPasteId);

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
  webwrite('<br>May take a while...');
  webwrite('<br>Showing all pastie ID''s...');
  // hopefully trigger web browser flush
  FlushWebBrowser;
  webwrite('<br>.......................................................');
  // now find all pasties in sub directory
  GetAllPastieIds;
  // display pasties
  for i:= low(pastes.items) to high(pastes.items) do
  begin
    webwrite('<p />');
    if pastes.items[i] <> 'main' then // we don't want main shown since it is main page, not a pastie
      webwrite(MakePastieLink(pastes.items[i]));
  end;
end;

end.
