unit wikitohtm2;  {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF} {$APPTYPE CONSOLE}

interface

procedure ParseWikiTxt(const s: string);
var textfile: text;

implementation

uses
  wikiparserb, sysutils, wikfilefuncs;

procedure out(s: string);
begin
  write(textfile, s);
end;

procedure outln(s: string);
begin
  writeln(textfile, s);
end;

procedure OnUrl(const url: string; user: pointer);
begin
  out(MakeFriendlyUrl(url));
end;

procedure OnAnchorUrl(const url: TUrlAnchor; user: pointer);
begin                                             
  out('<a href="' + url.url + '">' + url.anchortxt + '</a>');
end;

procedure OnWikiLink(const title: string; user: pointer);
var link: string;
    dashedtitle: string;
begin
  dashedtitle:= lowercase(title);
  SpacesToDashes(dashedtitle);
  link:= 'wik?p=' + dashedtitle;
  out('<a href="'+link+'">'+title+'</a>');
end;


procedure OnTxt(const txt: string; user: pointer);
begin
  out(txt);
end;

procedure OnCode(const code: string; user: pointer);
begin
  outln('<pre>'+code+'</pre>');
end;

{ other CRLF's are ones where we don't want break to occur such as right after
  preformatted text snippet or a heading.. putting an html break after a 
  heading looks ugly because a heading already has space underneath }
procedure OnOtherLnFeed(user: pointer);
begin
//  out('');
end;

procedure OnLineFeed(user: pointer);
begin
  outln('<br />');
end;

procedure OnBoldBegin(user: pointer);
begin
  out('<b>');  
end;

procedure OnBoldEnd(user: pointer);
begin
  out('</b>');  
end;

procedure OnItalicBegin(user: pointer);
begin
  out('<i>');  
end;

procedure OnItalicEnd(user: pointer);
begin
  out('</i>');  
end;

procedure OnHrule(user: pointer);
begin
  outln('<hr>');  
end;

procedure OnHeading(s: string; sz: THeadingSize; user: pointer);
begin
  case sz of
    hs2: outln('<h2>' + s + '</h2>');  
    hs3: outln('<h3>' + s + '</h3>');  
    hs4: outln('<h4>' + s + '</h4>');  
  end;
end;


procedure ParseWikiTxt(const s: string);
var
  events: TParseEvents = (
      user: nil;  
      OnUrl:         TStringEvent(OnUrl); 
      OnAnchorUrl:   TAnchorUrlEvent(OnAnchorUrl); 
      OnTxt:         TStringEvent(OnTxt);
      OnCode:        TStringEvent(OnCode);
      OnLnFeed:      TSimpleEvent(OnLineFeed);
      OnBoldBegin:   TSimpleEvent(OnBoldBegin);
      OnBoldEnd:     TSimpleEvent(OnBoldEnd);
      OnItalBegin:   TSimpleEvent(OnItalicBegin);
      OnItalEnd:     TSimpleEvent(OnItalicEnd);
      OnHrule:       TSimpleEvent(OnHrule);
      OnHeading:     THeadingEvent(OnHeading);
      OnOtherLnFeed: TSimpleEvent(OnOtherLnFeed);
      OnWikiLink:    TStringEvent(OnWikiLink);
  );
begin
  ParseStr(s, @events);
end;


end.