{*******************************************************************************

                               PasTokenizer Project

********************************************************************************
 ChrStream implementation

 Note: thanks to Julian Bucknall and Thaddy de Koning, many ideas and code
 in this parser comes from those people.

 Author: Lars (L505)
 Site: http://z505.com
*******************************************************************************}

unit ChrStream; {$IFDEF FPC}{$mode objfpc} {$H+}{$ENDIF}

//{$DEFINE DEBUG} // comment out, unless you want debugging

interface

uses
  multitype,
  PCHarUtils,
  strwrap1
  {$ifdef windows} ,Windows {$endif};

{$ifdef debug}
var debugt: text;
{$endif}

type
  PByteArray = ^TByteArray;
  TByteArray = array[0..maxint -1] of byte;
  PFile = ^file;
  astr = ansistring;


{$i ChrStrmDef.inc}

type
  TEndOfLine = (eolCRLF, eolLF);

 (*** string stream ***)
  { constructors }
  function NewChrStrStrm(const s: astr): PChrStrm;

  { destructor }
  procedure FreeChrStrStrm(Strm: PChrStrm);

 (*** file stream ***)
  { constructors }
  function NewChrFileStrm1(const filename: astr): PChrStrm;

  function NewChrFileStrm2(const filename: astr; ABufSize: Integer): PChrStrm;

  { destructor }
  procedure FreeChrFileStrm(Strm: PChrStrm);
  
 { public methods }
  function GetChar(self: PChrStrm): char;
  procedure PutBackChar(aCh: char; self: PchrStrm);

implementation


{-- private unit variables ----------------------------------------------------}
const
  BUF_SIZE = 32768;
  CR = #13;
  LF = #10;

{------------------------------------------------------------------------------}

{-- protected methods ---------------------------------------------------------}

{ get all contents of file into our buffer stream }
procedure GetBufferFromFile(var self: TChrStrmPriv);
begin
  self.fbufpos:= 0;
  // reads until BUF_SIZE, returns total chars read from file into Fbufend
  BlockRead(self.FTextContent.afile, self.fbuffer^, BUF_SIZE, self.fbufend);
end;

{ get all contents from a string into our buffer stream.  }
procedure GetBufferFromString(var self: TChrStrmPriv);
var
  SLen: integer;
begin
  self.fbufpos:= 0;
  inc(self.sReadCount); // only read from the string once, emulating one blockread on a file.. but this is a string
  if self.sReadCount = 2 then self.fbufend:= 0; 
  if self.sReadCount = 1 then 
  begin
    SLen:= length(self.ftextcontent.astring) + 1;
    // copy ansistring into ByteArray buffer
    move(self.ftextcontent.astring[1], self.fbuffer[0], SLen); 
    self.fbufend:= SLen;  
  end;
end;
  {Note: For bigger strings one could write their own GetBufferFromString function to 
   grab string contents in several chunks instead of all at once if memory is an issue.. 
   regularily, it isn't an issue! Strings aren't usually gigantic in size and most
   PC's and servers have tons of memory available.}


{------------------------------------------------------------------------------}


{-- public methods ------------------------------------------------------------}

{ attempt to grab a char from buffer }
function GetChar(self: PchrStrm): char;
begin
//  repeat
    { use putback chars if available }
    if (self^.privf.FPutBackInx <> 0) then
    begin
      dec(self^.privf.FPutBackInx);
      Result := self^.privf.FPutBackBuf[self^.privf.FPutBackInx];
    end
    { otherwise use the buffer }
    else begin
      {make sure the buffer has data}
      if (self^.privf.FBufPos = self^.privf.FBufEnd) then
        self^.privm.GetBuffer(self^.privf);
      { if there is no more data, return #0 to signal end of stream }
      if (self^.privf.FBufEnd = 0) then 
        result := #0 
      else { return current character }
      begin
        result:= char(self^.privf.FBuffer^[self^.privf.FBufPos]);
       {$ifdef debug}if result = #0 then writeln(debugt, 'GetChar: input stream not valid text, read null');
       {$endif}
        inc(self^.privf.FBufPos);
      end;
    end;
//  until (Result <> CR); // if carriage return found, skip past it
end;

procedure PutBackChar(aCh: char; self: PChrStrm);
begin
  {$ifdef debug}  if not self^.privf.FPutBackInx < 2 then  writeln(debugt,'PutBackChar: putback buffer full');
  {$endif}
  self^.privf.FPutBackBuf[self^.privf.FPutBackInx] := aCh;
  inc(self^.privf.FPutBackInx);
end;

{ create new file stream with default buffer read size }
function NewChrFileStrm1(const filename: astr): PChrStrm;
begin
  result:= NewChrFileStrm2(filename, BUF_SIZE);
end;

{ create new file stream with custom buffer size. Can't export overloaded 
 functions in DLL (certain FPC versions) so we name them '1' and '2' }
function NewChrFileStrm2(const filename: astr; ABufSize: integer): PChrStrm;
begin
  new(result);
  result^.privf.FBufPos:= 0;
  result^.privf.FBufEnd:= 0;
  result^.privf.FPutBackInx:= 0;

  { set ftextcontent up as a file stream}
  OpenFileRead(result^.privf.ftextcontent.afile, filename, 1);
  { create buffer for file data }
  GetMem(result^.privf.FBuffer, ABufSize);

  {- setup methods -} // ifdef fpc makes code ugly :-( But I don't like {$mode delphi} as much. 
  // public
  result^.GetChar:= {$ifdef fpc}@{$endif}getchar;
  result^.Putbackchar := {$ifdef fpc}@{$endif}putbackchar;
  // private
  result^.privm.GetBuffer:= {$ifdef fpc}@{$endif}GetBufferFromFile; {class abstraction without VMT - we control the getbuffer method ourselves}
end;

{ create string stream }
function NewChrStrStrm(const s: astr): PChrStrm;
begin
  new(Result);
  // init
  result^.privf.FBufEnd:= 0;
  result^.privf.FBufPos:= 0;
  result^.privf.FPutBackInx:= 0;
  result^.privf.sReadCount:= 0;
  { set textcontent up as a string }
  result^.privf.FTextContent.astring:= s;
  { create buffer for string data}
  GetMem(result^.privf.FBuffer, length(s)+1); 
  { length of buf is length of string plus null. Todo: maybe null not needed in bytearray. }

  { setup methods } // ifdef fpc makes code look ugly :-( But I don't like {$mode delphi} as much. 
  result^.GetChar:= {$ifdef fpc}@{$endif}getchar;
  result^.Putbackchar:= {$ifdef fpc}@{$endif}putbackchar;
  result^.privm.GetBuffer:= {$ifdef fpc}@{$endif}GetBufferFromString; {class abstraction without VMT - we control the getbuffer method ourselves}
end;

{ destroy file stream }
procedure FreeChrFileStrm(Strm: PChrStrm);
begin
  CloseFile(strm^.privf.FTextContent.afile); // open file must be closed
  { destroy the bytearray buffer }
  if (Strm^.privf.FBuffer <> nil) then
    freemem(Strm^.privf.FBuffer);
  { destroy record structure (fake object) }
  dispose(Strm);
end;

{ destroy string stream }
procedure FreeChrStrStrm(Strm: PChrStrm);
begin
  { destroy the bytearray buffer }
  if (Strm^.privf.FBuffer <> nil) then
    freemem(Strm^.privf.FBuffer);
  { destroy record structure (fake object) }
  dispose(Strm);
end;

{------------------------------------------------------------------------------}


initialization
{$ifdef debug}
  assign(debugt, 'chrstream.log');
  rewrite(debugt);
{$endif}
finalization
{$ifdef debug}
  close(debugt);
{$endif}

end.


