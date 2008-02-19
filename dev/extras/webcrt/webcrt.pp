unit webcrt;

Interface

Const
  { Controlling consts }
  Flushing     = true;               {if true then don't buffer output}


// Assigns write and writeln to the webwrite routine.
Procedure AssignWeb(Var F : Text);

// Restores Normal functionality of write and writeln;
Procedure RestoreNormalWrite;

Implementation

uses 
 {$IFNDEF DYNWEB}
   pwumain;
{$ELSE}
   dynpwu;
{$ENDIF}

{
  The definitions of TextRec and FileRec are in separate files.
}
{$i textrec.inc}


Var
// To save the old input and Output Variables 
 OldInput, 
 OldOutput : Text;

{*****************************************************************************
                    Some Handy Functions Not in the System.PP
*****************************************************************************}

Function Str(l:longint):string;
{
  Return a String of the longint
}
var
  hstr : string[32];
begin
  System.Str(l,hstr);
  Str:=hstr;
end;

Function Max(l1,l2:longint):longint;
{
  Return the maximum of l1 and l2
}
begin
  if l1>l2 then
   Max:=l1
  else
   Max:=l2;
end;

Function Min(l1,l2:longint):longint;
{
  Return the minimum of l1 and l2
}
begin
  if l1<l2 then
   Min:=l1
  else
   Min:=l2;
end;


const
  ttyIn=0;  {Handles for stdin/stdout}
  ttyOut=1;
  Web_Flush:boolean=true;

{Buffered Output}
  OutSize=1024;

var
 OutBuf : array[0..OutSize-1] of char;
 OutCnt : longint;

{Flush Output Buffer}
procedure WebFlushOutput;
Var 
 S : Ansistring;

begin
  if OutCnt>0 then
   begin
     SetLength(S,OutCnt + 1);
     Move(Outbuf[0], s[1], OutCnt);
     S[OutCnt + 1] := #0;
     Webwrite(S);
     OutCnt:= 0;
   end;
end;


Function WebSetFlush(b:boolean):boolean;
begin
  WebSetFlush:=Web_Flush;
  Web_Flush:=b;
  if Web_Flush then
   WebFlushOutput;
end;

{Send Char to Remote}
Procedure tty_WebSendChar(c:char);
Begin
  if OutCnt<OutSize then
   begin
     OutBuf[OutCnt]:=c;
     inc(OutCnt);
   end;
{Full ?}
  if (OutCnt>=OutSize) then
   WebFlushOutput;
End;

{Send String to Remote}
procedure tty_WebSendStr(const hstr:string);
var
  i : longint;
begin
  for i:=1to length(hstr) do
   tty_WebSendChar(hstr[i]);
  if Web_Flush then
   WebFlushOutput;
end;


procedure tty_WebWrite(const s:string);
begin
  tty_WebSendStr(s);
end;


procedure LineWrite(const temp:String);
begin
  tty_WebSendStr(Temp);
end;



{****************************************************************************
                        Write(ln) support
****************************************************************************}


Function WebCRTWrite(Var F: TextRec): Integer;
{
  Top level write function for WebCRT
}
Var
  Temp : ShortString;
  idx,i : Longint;
  oldflush : boolean;

Begin
  oldflush:=WebSetFlush(Flushing);
  idx:=0;
  while (F.BufPos>0) do
   begin
     i:=F.BufPos;
     if i>255 then
      i:=255;
     Move(F.BufPTR^[idx],Temp[1],i);
     SetLength(Temp,i);
// ** ALL THIS SETUP CODE IS TO ACCOMPLISH THIS
// ** When the programmer uses write, writeln then
// ** WebWrite gets Called :) 
     WebWrite(Temp);
     dec(F.BufPos,i);
     inc(idx,i);
   end;
  WebSetFlush(oldFLush);
  WebCRTWrite:=0;
End;


//  ******** I DON'T THINK I NEED THIS ANYMORE ***
//  ****     I AM STILL A LITTLE FUZZY ON WHAT I'M DOING HERE
//  ****     SO CODE STAYS BEHIND REMARKS FOR NOW. :)
// Function CrtReturn(Var F:TextRec):Integer;
// Begin
//  CrtReturn:=0;
// end;


Function WebClose(Var F: TextRec): Integer;
{
  Close Web associated file.
}
Begin
  F.Mode:=fmClosed;
  WebClose:=0;
End;


Function WebOpen(Var F: TextRec): Integer;
{
  Open CRT associated file.
}
Begin
  If F.Mode=fmOutput Then
   begin
     TextRec(F).InOutFunc:=@WebCRTWrite;
     TextRec(F).FlushFunc:=@WebCRTWrite;
   end
  Else
   begin
     F.Mode:=fmInput;
     TextRec(F).InOutFunc:= TextRec(OldInput).InOutFunc;
     TextRec(F).FlushFunc:= TextRec(OldInput).FlushFunc;
   end;
  TextRec(F).CloseFunc:=@WebClose;
  WebOpen:=0;
End;


procedure AssignWeb(var F: Text);
{
  Assign a file to the WebWrite Function
  All output on file goes to WebWrite instead.
}
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@WebOpen;
end;

Procedure RestoreNormalWrite;
begin
 WebFlushOutput;
 Move(OldOutput, Output, SizeOf(TextRec));
end;

{******************************************************************************
                               Initialization
******************************************************************************}

Initialization
// Save the old Input and OutputVars;
 Move(Output, OldOutput, SizeOf(TextRec));
 Move (Input, OldInput, SizeOf(TextRec));

{ Redirect the standard output }
  assignWeb(Output);
  Rewrite(Output);
  TextRec(Output).Handle:= StdOutputHandle;
 

Finalization
  WebFlushOutput;

End.
