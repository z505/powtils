{*******************************************************************************

                               PasTokenizer Project

********************************************************************************
 Pascal tokenizer for general purpose parsing of Pascal code.

 Note: thanks to Julian Bucknall and Thaddy de Koning, many ideas and code
 in this parser comes from those people.

 Author: Lars (L505)
 Site: http://z505.com
*******************************************************************************}
unit pastokenize;  {$IFDEF FPC}{$mode objfpc} {$H+}{$ENDIF}
// {$DEFINE DEBUG} // turn this on for debugging
interface

{$ifndef FPC}{$DEFINE SYSUTILS_ON}{$endif}

uses
  compactutils,
  {$ifndef SYSUTILS_ON}compactsysutils{$else}sysutils{$endif},
  ChrStream,
  tokentypes;

type
  astr = ansistring;

{------------------------------------------------------------------------------}
{$i PasParserDef.inc}

 { constructor }
  function NewPasParser(aStrm: PChrStrm): PPasParser;

 { destructor }
  procedure FreePasParser(P: PPasParser);

 { public methods }
  procedure GetToken(var ATokType: TPasToken; var ATok: astr; 
  ParserSelf: PPasParser; self: PChrStrm);

procedure Getatok(aInStm: PChrStrm; var aTokType: TPasToken; var aTok: astr; 
  self: PChrStrm);


implementation

{-- private unit data ---------------------------------------------------------}
const // very clean storage of language keywords! 
  KeywordCount = 106;
  KeywordList : array [0..pred(KeywordCount)] of string = (
    {reserved words} 
    'AND', 'ARRAY', 'AS', 'ASM', 'BEGIN',                          
    'CASE', 'CLASS', 'CONST', 'CONSTRUCTOR', 'DESTRUCTOR',
    'DISPINTERFACE', 'DIV', 'DO', 'DOWNTO', 'ELSE',
    'END', 'EXCEPT', 'EXPORTS', 'FILE', 'FINALIZATION',
    'FINALLY', 'FOR', 'FUNCTION', 'GOTO', 'IF',
    'IMPLEMENTATION', 'IN', 'INHERITED', 'INITIALIZATION', 'INLINE',
    'INTERFACE', 'IS', 'LABEL', 'LIBRARY', 'MOD',
    'NIL', 'NOT', 'OBJECT', 'OF', 'OR',
    'OUT', 'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY',
    'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING', 'SET',
    'SHL', 'SHR', 'STRING', 'THEN', 'THREADVAR',
    'TO', 'TRY', 'TYPE', 'UNIT', 'UNTIL',
    'USES', 'VAR', 'WHILE', 'WITH', 'XOR',
    {directives}
    'ABSOLUTE', 'ABSTRACT', 'ASSEMBLER', 'AUTOMATED', 'CDECL',     
    'CONTAINS', 'DEFAULT', 'DISPID', 'DYNAMIC', 'EXPORT',
    'EXTERNAL', 'FAR', 'FORWARD', 'IMPLEMENTS', 'INDEX',
    'MESSAGE', 'NAME', 'NEAR', 'NODEFAULT', 'OVERLOAD',
    'OVERRIDE', 'PACKAGE', 'PASCAL', 'PRIVATE', 'PROTECTED',
    'PUBLIC', 'PUBLISHED', 'READ', 'READONLY', 'REGISTER',
    'REINTRODUCE', 'REQUIRES', 'RESIDENT', 'SAFECALL', 'STDCALL',
    'STORED', 'VIRTUAL', 'WRITE', 'WRITEONLY',
    {others}
    'AT', 'ON'
    );
{------------------------------------------------------------------------------}

{--- protected methods --------------------------------------------------------}

procedure ppInitKeywords(self: PPasParser);
var
  i : integer;
begin
{$ifdef debug}
  Assert(self^.privf.FKeywords <> nil, 'ppInitKeywords: nil Stringlist');
{$endif}
  for i:= 0 to pred(KeywordCount) do
    self^.privf.FKeywords^.Add(KeyWordList[i]);
   self^.privf.FKeywords^.sort(True);
end;

{------------------------------------------------------------------------------}


{-- public methods ------------------------------------------------------------}

procedure GetToken(var ATokType: TPasToken; var ATok: astr;
  ParserSelf: PPasParser; self: PChrStrm);
var
  DummyObj: Integer;
begin
  GetaTok(parserself^.privf.FStrm, atokType, atok, self);
  if (aTokType = ptIdentifier) then
    if parserself^.privf.FKeywords^.Find(UpperCase(aTok), DummyObj) then
      aTokType:= ptKeyword;
end;

function NewPasParser(aStrm: PChrStrm): PPasParser;
begin
  { create }
  new(Result);
  result^.GetToken:= {$ifdef fpc}@{$endif}GetToken;
  { save the stream }
  result^.privf.FStrm := astrm;
  { create the keywords list }
  result^.privf.FKeywords:= NewstrList;
  ppInitKeywords(result);
end;

procedure FreePasParser(p: PPasParser);
begin
  { destroy the keywords list }
  p^.privf.FKeywords^.Free;
  p^.privf.FKeywords:= nil;

  { destroy PasParser }
  dispose(p);
end;

{------------------------------------------------------------------------------}


{--- private unit methods -----------------------------------------------------}

procedure ReadNumber(aInStm : PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch : char;
  State : (BeforeDecPt, GotDecPt, AfterDecPt, Finished);
begin
  State:= BeforeDecPt;
  while (State <> Finished) do 
  begin
     ch:= aInStm^.GetChar(self);
    if (Ch = #0) then begin
      State := Finished;
      aInStm^.PutBackChar(Ch, self);
    end
    else begin
      case State of
        BeforeDecPt :
          begin
            if (Ch = '.') then 
     begin
              State := GotDecPt;
            end
            else if (Ch < '0') or (Ch > '9') then 
     begin
              State := Finished;
              aInStm^.PutBackChar(Ch, self);
            end else
              aTok := aTok + Ch;
          end;
        GotDecPt :
          begin
            if (Ch = '.') then 
     begin
              aInStm^.PutBackChar(Ch, self);
              aInStm^.PutBackChar(Ch, self);
              State := Finished;
            end else 
     begin
              aTok := aTok + '.';
              aTok := aTok + Ch;
              State := AfterDecPt;
            end;
          end;
        AfterDecPt :
          begin
            if (Ch < '0') or (Ch > '9') then 
     begin
              State := Finished;
              aInStm^.PutBackChar(Ch, self);
            end else
              aTok := aTok + Ch;
          end;
      end;
    end;
  end;
end;

procedure ReadHexNumber(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch : char;
  State : (NormalScan, Finished);
begin
  State := NormalScan;
  while (State <> Finished) do
  begin
     ch:= aInStm^.GetChar(self);
    if (Ch = #0) then
      State := Finished
    else 
    begin
      case State of
        NormalScan :
          begin
            if not (Ch in ['A'..'F', 'a'..'f', '0'..'9']) then begin
              State := Finished;
            aInStm^.PutBackChar(Ch, self);
            end else
              aTok:= aTok + Ch;
          end;
      end;
    end;
  end;
end;

procedure ReadIdentifier(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch : char;
begin
  ch:=aInStm^.GetChar(self);
  while Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do 
  begin
    aTok := aTok + Ch;
    ch:=aInStm^.GetChar(self);
  end;
  aInStm^.PutBackChar(Ch, self);
end;

procedure ReadString(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch : char;
begin
  ch:=aInStm^.GetChar(self);
  while (Ch <> '''') and (Ch <> #0) do 
  begin
    aTok := aTok + Ch;
    ch:=aInStm^.GetChar(self);
  end;
  if (Ch = '''') then
    aTok := aTok + Ch
  else
    aInStm^.PutBackChar(Ch, self);
end;

procedure ReadBraceComment(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch : char;
begin
  ch:=aInStm^.GetChar(self);
  while (Ch <> '}') and (Ch <> #0) do 
  begin
    aTok := aTok + Ch;
    ch:=aInStm^.GetChar(self);
  end;
  if (Ch = '}') then
    aTok := aTok + Ch
  else
    aInStm^.PutBackChar(Ch, self);
end;

procedure ReadSlashComment(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch : char;
begin
  ch:= aInStm^.GetChar(self);
  while (Ch <> #10) and (Ch <> #0) do 
  begin
    aTok := aTok + Ch;
    ch:= aInStm^.GetChar(self);
  end;
  aInStm^.PutBackChar(Ch, self);
end;

procedure ReadWhitespace(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch: char;
begin
  aTok:= '';
  ch:= aInStm^.GetChar(self);
  while Ch in [#1..#32] do 
  begin
    aTok:= aTok + Ch;
    ch:= aInStm^.GetChar(self);
  end;
  aInStm^.PutBackChar(Ch, self);
end;

procedure ReadParenComment(aInStm: PChrStrm; var aTok: astr; self: PChrStrm);
var
  Ch: char;
  State: (NormalScan, GotStar, Finished);
begin
  State:= NormalScan;
  while (State <> Finished) do 
  begin
     ch:= aInStm^.GetChar(self);
    if (Ch = #0) then begin
      State:= Finished;
      aInStm^.PutBackChar(Ch, self);
    end else 
    begin
      aTok := aTok + Ch;
      case State of
        NormalScan :
          if (Ch = '*') then
            State:= GotStar;
        GotStar :
          if (Ch = ')') then
            State:= Finished
          else
            State:= NormalScan;
      end;
    end;
  end; { state finished }
end;
{------------------------------------------------------------------------------}


{ Parses one token, sends back information in the var parameters.
  aTokType: type of token found
  aTok: token text found     }
procedure GetaTok(aInStm: PChrStrm; var aTokType: TPasToken; var aTok: astr; 
  self: PChrStrm);
var
  ch : char;
begin
  { assume invalid token }
  aTokType:= ptInvalidToken;
  aTok:= '';

  ch:= aInStm^.GetChar(self);
  aTok:= ch;

  { parse the token character by character }
  case ch of    
    '#' : aTokType:= ptHash;
    '$' : begin
            aTokType:= ptHexNumber; 
            ReadHexNumber(aInStm, aTok, self);
          end;
    '''': begin
            aTokType:= ptString;
            aTok:= '''';
            ReadString(aInStm, aTok, self);
          end;
    '(' : begin
            ch:=aInStm^.GetChar(self);
            if (Ch <> '*') then begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptOpenParen;
            end
            else begin
              aTokType:= ptComment;
              aTok:= '(*';
              ReadParenComment(aInStm, aTok, self);
            end;
          end;
    ')' : aTokType:= ptCloseParen;
    '*' : aTokType:= ptMultiply;
    '+' : aTokType:= ptPlus;
    ',' : aTokType:= ptComma;
    '-' : aTokType:= ptMinus;
    '.' : begin
            ch:=aInStm^.GetChar(self);
            if (Ch = '.') then
            begin
              aTok:= '..';
              aTokType:= ptRange;
            end
            else begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptPeriod;
            end;
          end;
    '/' : begin
            ch:=aInStm^.GetChar(self);
            if (Ch <> '/') then begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptDivide;
            end
            else begin
              aTokType:= ptComment;
              aTok:= '//';
              ReadSlashComment(aInStm, aTok, self);
            end;
          end;
    '0'..'9' :
          begin
            aTokType:= ptNumber;
            ReadNumber(aInStm, aTok, self);
          end;
    ':' : begin
            ch:=aInStm^.GetChar(self);
            if (Ch = '=') then
            begin
              aTokType:= ptAssign;
              aTok:= ':=';
            end
            else begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptColon;
            end;
          end;
    ';' :aTokType:= ptSemicolon;
    '<' : begin
            ch:=aInStm^.GetChar(self);
            if (Ch = '=') then
            begin
              aTok:= '<=';
              aTokType:= ptLessEqual;
            end
            else if (Ch = '>') then
            begin
              aTok:= '<>';
              aTokType:= ptNotEquals;
            end
            else begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptLess;
              aTok:= '<';
            end;
          end;
    '=' : aTokType:= ptEquals;
    '>' : begin
             ch:= aInStm^.GetChar(self);
            if (Ch = '=') then
            begin
              aTok:='>=';
              aTokType:= ptGreaterEqual;
            end
            else begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptLess;
              aTok:='>';
            end;
          end;
    '@' : aTokType:= ptAddress;
    'A'..'Z', 'a'..'z', '_' :
          begin
            aTokType:= ptIdentifier;
            ReadIdentifier(aInStm, aTok, self);
          end;
    '[' : aTokType:= ptOpenBracket;
    ']' : aTokType:= ptCloseBracket;
    '^' : aTokType:= ptCaret;
    '{' : begin
             ch:= aInStm^.GetChar(self);
            if ch<>'$' then
            begin
              aInStm^.PutBackChar(Ch, self);
              aTokType:= ptComment;
              aTok:= '{';
              ReadBraceComment(aInStm, aTok, self);
            end else
            begin
              aTokType:= ptDirective;
              aTok:= '{$';
              ReadBraceComment(aInstm, aTok, self);
            end;
          end;
    #13: aTokType:= ptCR;
    #10: aTokType:= ptLF;
    #32: aTokType:= ptSpace;
    
    #1..#9, #11..#12, #14..#31: //tabs and separators to skip past
       begin
         aInStm^.PutBackChar(Ch, self);//
         ReadWhitespace(aInStm, aTok, self);
         aTokType:= ptWhitespace;
       end;
    #0 :
      begin // null char
        aTokType:= ptEndOfFile;
        exit;
      end;
    else
      begin
       // aTok = ch at this point,
      end;
  end;
{$ifdef debug}
  Assert(aTokType <> ptInvalidToken,  'Parser found invalid token.');
{$endif}
end;

end. { of unit }
