{ Wik parser version b
  Copyright Lars Olson 2008-2011 }

unit wikiparserb; {$mode objfpc} {$H+} {$R+}
{$DEFINE DBUG}

interface

type 
  TUrlAnchor = record
    url: string;
    anchortxt: string;
  end;
  
  THeadingSize = (hsNotFound = 0, hs2 = 2, hs3 = 3, hs4 = 4);

  TStringEvent    = procedure (const s: string; user: pointer);
  TAnchorUrlEvent = procedure (const url: TUrlAnchor; user: pointer);
  THeadingEvent   = procedure (const txt: string; headsz: THeadingSize; user: pointer);
  TSimpleEvent    = procedure (user: pointer);

  // holds events & optional persistent data
  PParseEvents = ^TParseEvents;
  TParseEvents = record
    user: pointer; // optional carry around a user pointer. For class, record, or anything
    onUrl:         TStringEvent;
    onAnchorUrl:   TAnchorUrlEvent;
    onTxt:         TStringEvent;    // on all other text 
    onCode:        TStringEvent;    // on code/preformatted text 
    onLnFeed:      TSimpleEvent;    // line feeds but not in code snippets, nor after headings, nor right after code snippets
    onBoldBegin:   TSimpleEvent;    // on bold
    onBoldEnd:     TSimpleEvent;
    onItalBegin:   TSimpleEvent;    // on italic
    onItalEnd:     TSimpleEvent;
    onHrule:       TSimpleEvent;    // on horizontal rule 
    onHeading:     THeadingEvent;    
    onOtherLnFeed: TSimpleEvent;    // on a line feed that we don't want an HTML break for (heading, or <pre>)
    onWikiLink:    TStringEvent;
  end;

function parseStr(const s: string; const events: PParseEvents): boolean;
function makeFriendlyUrl(const url: string): string;

//function EatHeading(const s: string; headsz: THeadingSize; idx: integer; out endpos: integer): string;


procedure defaultDebug(const s: string);
var debugln: procedure(const s: string) = @defaultdebug;


implementation

uses 
  strutils, sysutils;

const
  CODE_BEGIN    = '--code';
  CODE_END      = 'code--';
  CODE_END_CRLF = 'code--'#13#10;
  CODE_END_LF   = 'code--'#10;


procedure defaultDebug(const s: string);
begin
  writeln('DEBUG: ', s);
end;

{------'dummy' events incase you do not assign some events in your program-----}
procedure defaultStringEvent(const s: string; user: pointer);
begin
  // do nothing by default. Then we don't have to check 'if assigned() then'
  // in the parser loop. Improves loop readability & maybe performance 
end;  

procedure defaultAnchorUrlEvent(const url: TUrlAnchor; user: pointer);
begin // do nothing by default
end;

procedure defaultSimpleEvent(user: pointer);
begin // do nothing by default
end;

procedure defaultHeadingEvent(const s: string; headsz: THeadingSize; user: pointer);
begin // do nothing by default
end;

{-----------------------------------------------------------------------------}

{ peek into string, verify substring exists at specified idx}
function peekFwd(const s: string; const slen: integer; sub: string; 
  const idx: integer): boolean;
var i: integer;
    extracted: string = '';
    sublen, subidx: integer;
begin
  result:= false;
  sublen:= length(sub);
  // do some checks to make sure we won't be out of range
  if idx < 1 then exit;
  if sublen > (slen - (idx-1)) then exit;
  if sublen < 1 then exit;
  subidx:= 1;
  setlength(extracted, sublen);
  uniqueString(extracted);
  for i:= idx to (sublen + idx) - 1 do begin 
    pchar(extracted)[subidx-1]:= s[i];
    inc(subidx);
  end;
  if extracted = sub then result:= true;        
end;

{ converts long url to friendly dotted shorter url }
function makeFriendlyUrl(const url: string): string;
const URLMAX = 40;  // how long friendly URL is
var shorturl: string = ''; 
    newurl: string = '';
    i: integer;
    urllen: integer;
begin 
  result:= '';
  urllen:= length(url);
  if urllen < 6 then exit; // ftp:// minimum 6 chars
  for i:= 1 to URLMAX do begin
    if i > urllen then break;
    shorturl:= shorturl + url[i];
  end;
  if urllen > URLMAX then begin
    newurl:= shorturl;
     // shorten like: http://site.com...10char.htm
    if length(newurl) > 5 then
      newurl:= newurl + '...' + rightStr(url, 10); 
  end else
    newurl:= shorturl;
  result:= '<a href="' + url + '">' + newurl + '</a>'; 
end;

{ verifies start of a URL exists in string at specified index }
function urlFound(const idx: integer; const s: string; const slen: integer): boolean;
begin
  result:= false;      
  if peekFwd(s, slen, 'http://', idx) then result:= true;
  if peekFwd(s, slen, 'https://', idx) then result:= true;
  if peekFwd(s, slen, 'ftp://', idx) then result:= true;
end;

{ verifies start of a [[URL]] exists in string at specified index }
function anchorUrlFound(const idx: integer; const s: string; 
  const slen: integer): boolean;
begin
  result:= false;      
  if peekFwd(s, slen, '[[http://', idx) then result:= true;
  if peekFwd(s, slen, '[[https://', idx) then result:= true;
  if peekFwd(s, slen, '[[ftp://', idx) then result:= true;
end;

function headingFound(const idx: integer; const s: string; 
  const slen: integer): THeadingSize;
begin
  result:= hsNotFound;      
  // order is important .. 4, 3, 2, 1 otherwise == will find === and ====
  if   peekFwd(s, slen, '====', idx) then result:= hs4 
  else
    if peekFwd(s, slen, '===', idx) then result:= hs3
  else
    if peekFwd(s, slen, '==', idx) then result:= hs2;
end;


{ Extracts http://url.com/ , returns zero endpos if problem
  Return end of URL zone as OUT param, return URL as result }
function eatUrl(const s: string; const slen: integer; const idx: integer; 
  out endpos: integer): string; 
var i: integer;
begin
  endpos:= 0;
  result:= '';
  if (idx < 1) or (idx > slen) then exit;
  for i:= idx to slen do
  begin 
    // signals end of url
    case s[i] of
      ' ', #13, #10, ')', '"', '''', ']': 
      begin
        endpos:= i;      
        //debugln('breaking loop. Char #:'+inttostr(ord(s[i])) );        
        break;
      end;
      // often people type punctuation marks after a url i.e. http://url.com,. 
      '.', ',', '!', '?', ';': 
      begin
        // end of string
        if i+1 > slen then begin
          endpos:= i;      
          break;
        end;
        // if http://site.com, or http://site.com. or http://site.com.CRLF
        case s[i+1] of
          ' ', #13, #10: 
          begin
            endpos:= i;      
            break;
          end;
        end;
      end;
    end;
    result:= result + s[i]; // concat
  end;
end;


function codeBeginFound(const idx: integer; const s: string; 
  const slen: integer): boolean;
begin
  result:= false;
  if peekFwd(s, slen, CODE_BEGIN, idx) then result:= true;
end;

{ Extracts code snippets (preformatted text) 
  Return end of code zone as OUT param, return code text as result }
function eatCode(const s: string; const slen: integer; idx: integer; 
  out endpos: integer): string; 
var i: integer;
begin
  endpos:= 0;
  result:= '';
  if (idx < 1) or (idx > slen) then exit;
  idx:= idx + length(CODE_BEGIN); // skip to start of actual code
  for i:= idx to slen do
  begin 
    case s[i] of
      'c': // code-- signals end of code
        if peekFwd(s, slen, CODE_END, i) then begin
          endpos:= i + length(CODE_END);
          break;
        end;
    end;
    result:= result + s[i]; // concat
  end;
end;

{ Extracts heading text ==some heading===, returns zero as endpos if problem}
function eatHeading(const s: string; const slen: integer; sz: THeadingSize; 
  const idx: integer; out endpos: integer): string;

var extracted: string = '';

const HEADING: array [hs2..hs4] of string = ('==', '===', '====');

  procedure processHeading;
  var i: integer;
  begin
    for i:= idx + byte(sz) to slen do begin
      if peekFwd(s, slen, HEADING[sz], i) then begin 
        endpos:= i + byte(sz); // return position after closing ==
        break;
      end;
      extracted:= extracted + s[i];
    end;
  end;

begin
  if slen < 1 then exit;
  endpos:= 0;
  result:= '';
  ProcessHeading;
  result:= extracted;
end;

{ Extracts [[http://url.com/ anchortitle]] }
function eatAnchorUrl(const s: string; const slen: integer; const idx: integer; 
  out endpos: integer): TUrlAnchor; 
var i: integer; 
    inanchor: boolean = false;
begin
  endpos:= 0;
  result.anchortxt:= '';
  result.url:= '';
  if (idx < 1) or (idx > slen) then exit;
  // start past 2 starting chars [[
  i:= idx + 2;
  while i <= slen do begin 
    case s[i] of
      ' ': // whitepace signals end of url and near text anchor
        if not inanchor then begin 
          inc(i); // skip space, but only first one before anchor text.. since we can have: [[http://url.com anchor with spaces]]
          inanchor:= true; 
        end;

      ']': 
        if s[i+1] = ']' then begin
          endpos:= i+2;  // skip ]], and done
          break;
        end;
    end;
        
    if inanchor then 
      result.anchortxt:= result.anchortxt + s[i] // concat url anchor txt
    else 
      result.url:= result.url + s[i]; // concat http:// url

    inc(i);
  end;

end;

{ Extracts [wiki link] }
function eatWikiLink(const s: string; const slen: integer; const idx: integer; 
  out endpos: integer): string; 
var i: integer; 
begin
  if (idx < 1) or (idx > slen) then exit;
  endpos:= 0;
  result:= '';
  // skip past bracket [
  i:= idx + 1;
  while i <= slen do begin 
    case s[i] of
      ']': 
        begin
          endpos:= i+1;  // skip ], and done
          break;
        end;
    end;
    result:= result + s[i]; // concat wiki link title
    inc(i);
  end;

end;


type TLineFeedKind = (lfNotFound, lf1, lf2);

function lineFeedExists(const s: string; const slen: integer; 
  const idx: integer): TLineFeedKind;
var up1: integer;
begin
  if (idx < 1) or (idx > slen) then exit;
  up1:= idx+1;
  result:= lfNotFound;
  if idx > slen then exit;
  if (s[idx] = #10) then result:= lf1;
  if up1 > slen then exit;
  if (s[idx] = #13) and (s[up1] = #10) then result:= lf2;
end;


// valid chars in name/identifier keywords such as this[] or this123[] or th-is[] or th_is[] or tHiS[]
const ValidNameChrs = ['A'..'Z', 'a'..'z', '_', '0'..'9', '-'];


{ returns false if problem or exited early }
function parseStr(const s: string; const events: PParseEvents): boolean;
var
  txtzonebuf: string = '';
  idx: integer = 1;   // main string buffer index
  up1: integer = 0;   // char idx peeking ahead one char 
  up2: integer = 0;   // char idx peeking ahead two chars
  slen: integer;      // length of incoming string to parse

  // this way 'if assigned()' checks are not needed in loop (possibly added performance, but done for clearer loop code)
  procedure setDummyEvents;
  begin
    with events^ do begin
      if not assigned(onUrl)         then onUrl:=         @defaultStringEvent; 
      if not assigned(onAnchorUrl)   then onAnchorUrl:=   @defaultAnchorUrlEvent; 
      if not assigned(onTxt)         then onTxt:=         @defaultStringEvent;
      if not assigned(onCode)        then onCode:=        @defaultStringEvent;
      if not assigned(onLnFeed)      then onLnFeed:=      @defaultSimpleEvent;
      if not assigned(onItalBegin)   then onItalBegin:=   @defaultSimpleEvent;
      if not assigned(onItalEnd)     then onItalEnd:=     @defaultSimpleEvent;
      if not assigned(onBoldBegin)   then onBoldBegin:=   @defaultSimpleEvent;
      if not assigned(onBoldEnd)     then onBoldEnd:=     @defaultSimpleEvent;
      if not assigned(onHrule)       then onHrule:=       @defaultSimpleEvent;
      if not assigned(onHeading)     then onHeading:=     @defaultHeadingEvent;
      if not assigned(onOtherLnFeed) then onOtherLnFeed:= @defaultSimpleEvent;
      if not assigned(onWikiLink)    then onWikiLink:=    @defaultStringEvent;
    end;
  end;

  { checks if token really exists by verifying space or other chars before token
    In many cases, space can include things like brackets since (b[this]b) is 
    bold text in brackets, and the brackets are valid space characters }
  function spaceBeforeToken(const sidx: integer): boolean;
  begin
    if sidx < 1 then exit;    // before beginning of file
    if sidx > slen then exit; // after end of file
    if s[sidx] in ValidNameChrs then result:= false else result:= true;
  end;
  
  procedure concatTxt;
  begin
    if idx < 1 then exit;
    if idx > slen then exit;
    if (s[idx] <> #10) and (s[idx] <> #13) then txtzonebuf:= txtzonebuf+s[idx]; 
  end;
  
  procedure flushTxt;
  begin
    if txtzonebuf <> '' then with events^ do onTxt(txtzonebuf, user); 
  end;

  procedure concatAndFlushTxt;
  begin
    concatTxt;
    flushTxt;
  end;

  var 
    newpos: integer = 0;  // new char idx to skip ahead to after done eating zone
    tmpzonebuf: string = '';  // temporary backbuffer for zone contents
    anchorzonebuf: TUrlAnchor = (url: ''; anchortxt: '');

  { reset temporary variables and buffers and other loop state info }
  procedure resetTmp;
  begin
    newpos:= 0;
    tmpzonebuf:= '';  
    txtzonebuf:= '';
    anchorzonebuf.anchortxt:= ''; 
    anchorzonebuf.url:= '';
  end;
  
  procedure setUpChars;
  begin
    // useful to peek one char ahead 
    up1:= idx+1;
    up2:= idx+2;
  end;

var 
  HeadingSize: THeadingSize;
begin
  result:= false;
  if events = nil then begin
    //debugLn('must specify custom events');
    exit;
  end;
  setDummyEvents;
  slen:= length(s);
  if slen < 1 then exit;
  while idx <=  slen do begin
    setUpChars;
    case s[idx] of
      '-': 
        begin
          // --code code-- found
          if codeBeginFound(idx, s, slen) then 
          begin
            tmpzonebuf:= eatCode(s, slen, idx, newpos);
            // check problems, i.e. no closing code tag 
            if newpos = 0 then begin
              concatTxt;
              inc(idx); // skip char and continue on
              continue;
            end;
            // else we found a good code snippet
            flushTxt;
            with events^ do onCode(tmpzonebuf, user); // normally check for IF ASSIGNED() but we have dummy events so we are okay 
            idx:= newpos; // skip to end of code zone
            setUpChars;
            case lineFeedExists(s, slen, idx) of
              lf1: 
                begin
                  with events^ do onOtherLnFeed(user); 
                  inc(idx, 1);
                end;
              lf2: 
                begin
                  with events^ do onOtherLnFeed(user); 
                  inc(idx, 2);
                end;
            end;
            resetTmp;
            continue;
          end else
            // --- horizontal rule
            if (s[up1] = '-') and (s[up2] = '-') then 
            begin
              flushTxt;
              with events^ do onHrule(user); // normally check for IF ASSIGNED() but we have dummy events so we are okay 
              inc(idx, 3); // fastfwd past horizontal rule zone
              setUpChars;
              case lineFeedExists(s, slen, idx) of
                lf1: 
                  begin
                    with events^ do onOtherLnFeed(user); 
                    inc(idx, 1);
                  end;
                lf2: 
                  begin
                    with events^ do onOtherLnFeed(user); 
                    inc(idx, 2);
                  end;
              end;
              resetTmp;
              continue;
            end else
              concatTxt;
        end;

      '=': 
        begin
          // ==heading2==, ===heading3===, ====heading4====,
          HeadingSize:= headingFound(idx, s, slen);
          if HeadingSize <> hsNotFound then
          begin
            tmpzonebuf:= eatHeading(s, slen, HeadingSize, idx, newpos);
            // check problems, example no closing tag found
            if newpos = 0 then 
            begin
              concatTxt;
              inc(idx); // skip char and continue on
              continue;
            end;
            // else we found a good heading
            flushTxt;
            with events^ do onHeading(tmpzonebuf, HeadingSize, user); 
            idx:= newpos; // fastfwd past heading zone
            setUpChars;
            // now check for line feed which we want to ignore since <br> after a heading looks bad
            case lineFeedExists(s, slen, idx) of
              lf1: 
                begin
                  with events^ do onOtherLnFeed(user); 
                  inc(idx, 1);
                end;
              lf2: 
                begin 
                  with events^ do onOtherLnFeed(user); 
                  inc(idx, 2);
                end;
            end;

            resetTmp;
            continue;
           end else
             concatTxt;
        end; { = }

      '[':
      begin
        // [[http://url.com with anchor]], 
        if anchorUrlFound(idx, s, slen) then 
        begin
          anchorzonebuf:= eatAnchorUrl(s, slen, idx, newpos);
          // check problems, i.e. if no closing tag found
          if newpos = 0 then begin
            concatTxt;
            inc(idx); // skip char and continue on
            continue;
          end;
          // else we found a good anchored url
          flushTxt;
          with events^ do onAnchorUrl(anchorzonebuf, user); 
          idx:= newpos; // skip to end of url zone
          resetTmp;
          continue;
        end;
  
        // parse wiki [link]
        tmpzonebuf:= eatWikiLink(s, slen, idx, newpos);
        // check problems, i.e. if no closing tag found
        if newpos = 0 then begin
          concatTxt;
          inc(idx); // skip char and continue on
          continue;
        end;
        // else we found a good anchored wiki link title
        flushTxt;
        with events^ do onWikiLink(tmpzonebuf, user); 
        idx:= newpos; // skip to end of url zone
        resetTmp;
        continue;
      end;


      'f', 'h': { ftp://, http:// }
        if urlFound(idx, s, slen) then 
        begin
          tmpzonebuf:= eatUrl(s, slen, idx, newpos);
          // check problems
          if newpos = 0 then begin
            //debugln('newpos is 0');
            concatTxt;
            inc(idx); // skip char and continue on
            continue;
          end;
          // else we found a good url
          flushTxt;
          with events^ do onUrl(tmpzonebuf, user);
          idx:= newpos; // fastfwd past end of url
          resetTmp;
          continue; 
        end else
          concatTxt;


      'b': // b[ open bold
        if spaceBeforeToken(idx-1) then // must be a space/carriage return, whitespace or other chars not belonging to the name token, right before the token
        begin
          if s[up1] = '[' then 
          begin
            flushTxt;
            with events^ do onBoldBegin(user); 
            inc(idx,2);
            resetTmp;
            continue;
          end else
            concatTxt;
        end else
          concatTxt;

      'i': // i[ open italic
        if spaceBeforeToken(idx-1) then
        begin
          if s[up1] = '[' then 
          begin
            flushTxt;
            with events^ do onItalBegin(user); 
            inc(idx,2);
            resetTmp;
            continue;            
          end else
            concatTxt;
        end else
          concatTxt;

      ']': // closing tag
        if spaceBeforeToken(up2) then
        begin
          case s[up1] of 
            'b':
              begin // close bold ]b 
                flushTxt;
                with events^ do onBoldEnd(user); 
                inc(idx,2); // fastfwd past closer tag
                resetTmp;
                continue;
              end;
            'i':
              begin // close itallic ]i 
                flushTxt;
                with events^ do onItalEnd(user);
                inc(idx,2); // fastfwd past closer tag
                resetTmp;
                continue;
              end;
          else
            concatTxt;
          end;
        end else
          concatTxt;

      #13:
        begin
          //debugln('#13 char');        
          flushTxt;
          resetTmp;
        end;

      #10:
        begin // line feed 
          //debugln('#10 char line feed');
          flushTxt;
          with events^ do onLnFeed(user);
          resetTmp;
        end;

    else 
      { concat every other char if not in special zone, don't flush each char, 
        save them in snippets and flush when we hit zone beginning }
      concatTxt; 
    end; {case}

    // fastfwd one char by default
    inc(idx);
  end;
  
  // flush main txt buffer if done 
  if idx > slen then begin
    concatAndFlushTxt;
    resetTmp;
  end;
  
  result:= true;
end;


end.

