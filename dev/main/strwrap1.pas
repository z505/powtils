{ Authors: Lars (L505), JKP (YetAnotherGeek).
  Miscellaneous string and related functions }

unit StrWrap1;

{$ifdef FPC}
  {$mode objfpc}
  {$H+} //force ansi strings on
{$endif}

{$I DelphiDefines.inc}

interface
uses 
  pwfileutil, pwtypes;

{--- Public ------------------------------------------------------------------}
const 
  FILE_ERR = '-1NF';

  { File open modes }
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  { Share modes}
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;

 {$ifdef WINDOWS}
  CRLF: astr = #13#10; // windows uses CRLF (carriage return and a line feed)
 {$endif}

 {$ifdef UNIX}
  CRLF: astr = #10;    // linux uses LF. (line feed)
 {$endif}

 {$ifdef MACOS}
  CRLF: astr = #13;    // old mac use CR (carriage return)
 {$endif}
{------------------------------------------------------------------------------}

                               
{--- public type --------------------------------------------------------------}
  type
    // string array
    StrArray = array of astr;

    // store line numbers
    LnArray = array of int32;

    // string array with a count variable available
    StrArray_c = record
      Lines: array of astr;
      Count: int32;
    end;
{------------------------------------------------------------------------------}

{ BEGIN: BACKWARDS COMPATIBLE (JUST WRAPPERS }
  function openFileRead(var F:file; const fname: astr; recsize: int32): boo;
  function openFileReWrite(var F:file; const fname:astr; recsize:int32): boo;
  function openFile(var F: text; const fname: astr; mode: char): boo; overload;
  function openFile(var F: file; const fname: astr; recsize:int32; mode: byte): boo; overload;
  function openFile(var F: file; const fname: astr; mode: char): boo; overload;
  function openFile(var F: TFileOfChar; const fname: astr; mode: char): boo; overload;  
  function makeDir(s: astr): boo;
{ END: BACKWARDS COMPATIBLE (JUST WRAPPERS }

{--- public functions ---------------------------------------------------------}
  function readChunk(const fname:astr; chunksz: int32; buf: pointer): boo;
  function saveChunk(fname:astr; buf:pointer; chunksz:int32): boo;

  function getLineCount(const fname:astr): int32;
  function findLine(const fname:astr; linenum:longword): boo;  

{ DEPRECATED: use file2str from pwfileutil }
  function strLoadFile(const fname: astr): astr; overload;
  function strLoadFile(const fname: astr; chunksz: int32): astr; overload;
  function strSaveFile(const fname, inputStr: astr): boo; overload;
  function strSaveFile(const fname, inputStr, endlnfeed: astr): boo; overload;
{ END DEPRECATED }

  function strLoadLns(NumOfLines: int32; const fname:astr): astr;
  function strLoadRng(FromLine: int32; ToLine:int32; const fname: astr):astr;


  function getLn1(const fname: astr): astr;
  function getLn2(const fname: astr): astr;
  function getLnN(LineNumber: int32; const fname: astr): astr;

  function arrayLoadFile(const fname:astr): StrArray;
  function arrayLoadFile_c(const fname: astr): StrArray_c;
  function arrayLoadFile_0(const fname: astr): StrArray;
  function arrayLoadFile_c_0(const fname: astr): StrArray_c;
  function arrayLoadLns(NumOfLines: int32; const fname: astr): StrArray;
  function arrayLoadLns_0(NumOfLines: int32; const fname: astr): StrArray;
  function arrayLoadRng(FromLine: int32; ToLine:int32; const fname: astr): StrArray;
  function arrayLoadRng_0(FromLine: int32; ToLine:int32; const fname: astr): StrArray;

  function GetLastLns(const fname: astr; NumLines: int32): astr;

  function RemoveDupStrings(var InputStrArray: StrArray): int32;
(*  TO DO

  function StrLoadExact(LineNumsArray: LnArray; fname: string): string;
  function ArrayLoadExact(LineNumsArray: LnArray; fname: string): StrArray;
  function ArrayLoadExact_0(LineNumsArray: LnArray; fname: string): StrArray;
  function ArraySaveFile(InputArray: StrArray; fname: string): boo;
  function StrAddLine: boo;
  function FileAddLine: boo;
  function StrAppendToFile: boo;
  function FileAppendToStr: boo;
*)

{------------------------------------------------------------------------------}


implementation


uses
  {$IFDEF UNIX}baseunix{$ENDIF}
  {$IFDEF WINDOWS}windows{$ENDIF};


function MakeDir(s: astr): boo;
begin
  result:= pwfileutil.MakeDir(s);
end;

function openFile(var F: text; const fname: astr; mode: char): boo;
begin
  result:= pwfileutil.openFile(F, fname, mode);
end;

function openFileRead(var F: file; const fname: astr; recsize: int32): boo;
begin
  result:= pwfileutil.openFileRead(F, fname, recsize);
end;

function openFile(var F: file; const fname: astr; recsize: int32; mode: byte): boo;
begin
  result:= pwfileutil.openFile(F, fname, recsize, mode);
end;

function openFileRewrite(var F: file; const fname: astr; recsize: int32): boo;
begin
  result:= pwfileutil.openFileRewrite(F, fname, recsize);
end;

function openFile(var F: file; const fname: astr; mode: char): boo;
begin
  result:= pwfileutil.openFile(F, fname, mode);
end;

function openFile(var F: TFileOfChar; const fname: astr; mode: char): boo;
begin
  result:= openFile(f, fname, mode);
end;


{ Get Line Count of a file

  NOTE: Using this function separately from other functions may not be optimal.
        If you can, choose a function that has the count already built into it.
        It's not usually a big deal. Line counts are fast anyway.   

 JEFF: return -1 on error}
function GetLineCount(const fname: astr): int32;
var F: text;
begin
  Result:=-1;
  if not openFile(F, fname, 'r') then EXIT;
  Result:=0;
  while not EOF(F) do begin
    Readln(F);
    inc(result); //increment line count
  end;
  close(F); //close up file
end;


{ Verify line exists in file (i.e. on large files, does line 150000 exist?) }
function FindLine(const fname: astr; linenum: longword): boo;
var F: text;
    cnt: longword;
begin
  Result:= false;
  cnt:= 0;
  if not openFile(F, fname, 'r') then EXIT;
  while (result <> true) or (not EOF(F)) do begin
    Readln(F);
    inc(cnt); //increment line count
    if cnt = linenum then result:= true;
  end;
  close(F); //close file
end;

{ DEPRECATED (use File2Str in pwfileutil)
  Loads file into string. Do not use for large files of say 2GB }
function strLoadFile(const fname: astr): astr;
begin
  result:= strLoadFile(fname, pwfileutil.getFileSize(fname));
end;

{ DEPRECATED (use File2Str in pwfileutil)
  overloaded with ability to specify chunk read size  }
function StrLoadFile(const fname: astr; chunksz: int32): astr; 
var F: file;
    curRead: int32;  
    totalRead: int32;
begin
  result:= FILE_ERR; 
  totalRead:= 0;
  // open file with a record size of 1
  if openFile(F, fname, 1, fmOpenReadWrite) = false then EXIT;
  result:= '';
  curRead:= 1;
  // the file will be read as one big chunk
  while curRead > 0 do begin
    setlength(result, length(result)+chunksz);
    uniquestring(result);
    curRead:= 0;
    blockRead(F, pchar(result)[totalread], chunksz, curRead);
    totalRead:= totalRead + curRead;
  end;
  setlength(result, totalRead);
  closeFile(F);
end;

{ save a string directly to a file 
  TODO: int64 sized files }
function StrSaveFile(const fname, InputStr: astr): boo; overload;
var F: file;
    OutputFileSize: int32;
    Buffer: Pointer;
begin
  result:= false;
  if openFileRewrite(F, fname, 1) = false then EXIT; // open in write mode
  OutputFileSize:= length(inputstr);
  Buffer := PChar(inputstr);
  BlockWrite(F, Buffer^, OutputFileSize);
  CloseFile(F);
  result:= true;
end;

{ read a chunk from any file }
function ReadChunk(const fname: astr; chunksz: int32; buf: pointer): boo;
var f: file;
    readamt: int32;
begin
  result:= false;
  if openFileRead(F, fname, 1) = false then EXIT; // open in write mode
  BlockRead(F, Buf^, chunksz, readamt);
  if readamt = 0 then result:= false;
  CloseFile(F);
end;

{ save a chunk directly to a file }
function SaveChunk(fname: astr; buf: pointer; chunksz: int32): boo;
var F: file;
begin
  result:= false;
  // open in write mode  
  if openFileRewrite(F, fname, 1) = false then EXIT; 
  BlockWrite(F, Buf^, chunksz);
  CloseFile(F);
end;

{ same as above but ability to specify final line ending at end of file }
function StrSaveFile(const fname, InputStr, endlnfeed: astr): boo; overload;
var F: file;
    OutputFileSize: int32;
    Buffer: Pointer;
    tmpstr: astr;
begin
  result:= false;
  if openFileRewrite(F, fname, 1) = false then EXIT; // open in write mode
  tmpstr:= inputstr + endlnfeed; // add custom line ending for very end of file
  OutputFileSize:= Length(tmpstr);
  Buffer := PChar(tmpstr);
  BlockWrite(F, Buffer^, OutputFileSize);
  CloseFile(F);
  result:= true;
end;

{ Load the first N lines of a file into a string directly. Without the entire
  file getting loaded into memory!
  Note: an extra carriage return is appended to the end }
function StrLoadLns(NumOfLines: int32; const fname: astr): astr;
var F: text;
    str1: astr;
    Line: astr;
    i:int32;
begin
  result:= FILE_ERR; 
  if not openFile(F, fname, 'r') then EXIT; 
  str1:=''; result:= ''; i:=0;
  while ( i < NumOfLines ) and ( not  EOF(F) ) do begin
    inc(i); Readln(F,Line);
    str1:= str1 + Line + CRLF; //todo: optimize - concats are dead slow (L505)
  end;
  result:= str1;
  close(F); //close up file
end; { Todo: get rid of extra carriage return that is appended. Will require an
       extra if statement, so may cause some slow down }
     { NOTE: Personally, I think the extra CRLF is a good thing. (Jeff) }


{ Load certain range of lines from a text file into a string.
  Example: lines 10 to 15 only }
function StrLoadRng(FromLine:int32; ToLine:int32; const fname: astr): astr;
var F: text;
    str1: astr;
    Line: astr;
    i:int32;
    GotLines: boo; //to signal that we have 'got' all the lines we need
begin
  result:= FILE_ERR; 
  GotLines:= false; //initialize as false
  if not openFile(F, fname, 'r') then exit;
  str1:=''; result:= ''; i:=0;
  while ( GotLines <> true )do begin
    if EOF(F) then gotlines:= true;
    inc(i);
    //we are still before the specified range
    if i < FromLine then Readln(F);
    //we are at the start of the specified range
    if i >= FromLine then begin
      Readln(F,Line);
      str1:= str1+Line+CRLF; //todo: optimize - concats are dead slow (L505)
    end;
    //we've 'gotten' all the lines we need so exit loop
    if i = ToLine then GotLines:= true;          
  end;
  result:= str1;
  close(F); 
end;


{ Get the first line of text into a string, from a specified file
  Tiny bit more efficient than using GetLnN, if you are only accessing Line 1
  Returns string '-1NF' if file NOT FOUND or could not open}
function GetLn1(const fname: astr): astr;
var
  F: text;
  Line: astr;
begin
  result:= FILE_ERR; 
  if not openFile(F, fname, 'r') then exit; 
  // read only the first line of file and close it
  Readln(F, Line); 
  result:= Line;
  close(F); 
end;


{ Get the second line of text into a string, from a specified file
  Tiny bit more efficient than using GetLnN, if you are only accessing Line 2 }
function GetLn2(const fname: astr): astr;
var F: text;
    Line: astr;
begin
  result:= FILE_ERR; 
  if not openFile(F, fname, 'r') then EXIT; 
  ReadLn(F); //pass the first line of the file
  ReadLn(F,Line); //read only the second line of file
  result:= Line;
  close(F); //close file
end;


{ Get specified Nth line in a text file into a string }
function GetLnN(LineNumber: int32; const fname: astr): astr;
var
  F: text;
  i:int32; 
  Line: astr;
//  GotLine: boo;
begin
  result:= ''; 
  if ( LineNumber < 1 ) then  exit; //nothing to do, file lines start at line 1
  result:= FILE_ERR; 
  if not openFile(F, fname, 'r') then EXIT; 
  result:= ''; 
  if not EOF(f) then //only continue reading the lines of the file until we have GOT our line that we want
  begin
    //get to the beginning of the specified line
    for i:= 1 to LineNumber-1 do readln(F);                    
    readln(F, Line);
    result:= Line;
  end; //else past end of file
  close(F); 
end;


{ Load an entire text file into an array of strings. Each string in the array
  becomes each line of the text file, i.e. separated by carriage returns.
  Note: the array contents start at array[1], since we deal with line 1, not
  line 0, for clarity.
  todo:  error checking as separate param }
function ArrayLoadFile(const fname: astr): StrArray;
var
  F: text;
  Line: astr;
  i: int32;
begin
  setlength(result,1);
  result[0]:= FILE_ERR;  // error to array[0] if no file found
  if not openFile(F, fname, 'r') then EXIT; 
  i:= 0;
  while not Eof(F) do begin
    inc(i);
    readln(F, Line);
    setlength(result, i+1);
    result[i]:= Line;
  end;
  close(F); 
end; { Todo: 1. optimize: caparray concept 2.dedicated error return }


{ Same as ArrayLoadFile except we return the count of our file's lines and use
  a record for the function return values. The c suffix stands for "count". 
  Returns -1NF in array[0] contents if file not found }  
function ArrayLoadFile_c(const fname: astr): StrArray_c;
var
  F: text;
  Line: astr;
  i:int32;
begin
  setlength(result.Lines,1);
  result.Lines[0]:=FILE_ERR; // error to array[0] if no file found
  result.Count:= -1;   // return -1 if nothing found
  if not openFile(F, fname, 'r') then EXIT;
  i:=0;
  while not Eof(F) do begin
    inc(i); readln(F,Line);
    setlength(result.lines,i+1);
    result.Lines[i]:= Line;
  end;
  result.Count:= i;
  close(F); 
end; { Todo: 1. optimize: caparray concept 2.dedicated error return }


{ Same as ArrayLoadFile except array starts at [0] instead of [1]
  Returns -1NF in array[0] contents if file not found }
function ArrayLoadFile_0(const fname: astr): StrArray;
var F: text;
    Line: astr;
    i:int32;
begin
  setlength(result,1);
  if not openFile(F, fname, 'r') then 
  begin result[0]:= FILE_ERR;  EXIT;
  end;
  i:= -1;
  while not Eof(F) do begin
    inc(i);
    Readln(F,Line);
    setlength(result,i+1);
    result[i]:= Line;
  end;
  close(F); //close up file
end; { Todo: 1. optimize: caparray concept 2.dedicated error return }


{ Same as ArrayLoadFile_c except starts at [0] instead of [1] }
function ArrayLoadFile_c_0(const fname: astr): StrArray_c;
var F: text;
    Line: astr;
    i: int32;
begin
  setlength(result.Lines,1);
  result.Count:= -1;   //...
  if not openFile(F, fname, 'r') then
  begin result.Lines[0]:= FILE_ERR; EXIT; 
  end;
  i:= -1;
  while not eof(F) do begin
    inc(i); readln(F, Line);
    setlength(result.Lines, i + 1);
    result.Lines[i]:= Line;
  end;
  result.Count:= i + 1 ; //since it's 0 based, we add 1 to get line count
  close(F); //close file
end; { Todo: 1. optimize: caparray concept 2.dedicated error return }


{ Load the first N lines of a file into an array of strings. Each line becomes
  a string in the array. Example: load the first 15 lines of a certain file.
  Note: the array contents start at array[1], since we deal with line 1, not
  line 0, for clarity. }
function ArrayLoadLns(NumOfLines:int32; const fname: astr): StrArray;
var F: text;
    Line: astr;
    i: int32;
begin
  setlength(result,1);
  result[0]:= FILE_ERR; //safety
  i:= 0;
  setlength(result,NumOfLines+1); //we know exact length 
  if not openFile(F, fname, 'r') then EXIT;
  while not ( Eof(F) ) and ( i < NumOfLines ) do begin
    inc(i);
    readln(F,Line);
    result[i]:= Line;
  end;
  close(F); //close up file
end;


{ Same as ArrayLoadLns, except start at array[0] instead of array[1]  }
function ArrayLoadLns_0(NumOfLines:int32; const fname: astr): StrArray;
var F: text;
    Line: astr;
    i: int32;
begin
  setlength(result,NumOfLines); //we know the exact length that the array will be
  if not openFile(F, fname, 'r') then begin 
    result[0]:= FILE_ERR; EXIT;
  end;
  i:= -1;
  while( i < NumOfLines-1 )  and ( not Eof(F) ) do begin
    inc(i); Readln(F,Line);
    result[i]:= Line;
  end;
  close(F); 
end;


{ Load a range of lines into a string array. Example: lines 11 to 16 into
  array[1] to array[6]
  note: array starts at 1 not 0 }
function ArrayLoadRng(FromLine:int32; ToLine:int32; const fname: astr): StrArray;
var F: text;
    Line: astr;
    i: int32;
    RngAmt: int32;
begin
  RngAmt:= ToLine - FromLine + 1; //the range i.e. 4:6 is 4,5,6 so add 1 to subtraction
  setlength(result,(RngAmt)+2); //we know the exact length that the array will be  +2 because range 4 to 6 is three lines, not two. So +1,  and setlength is 0 based, so another +1.
  result[0]:= FILE_ERR; 
  if not openFile(F, fname, 'r') then EXIT;
  if not ( Eof(F) ) then begin
    // go to first line needed
    for i:= 1 to FromLine-1 do readln(F); 
    for i:= 1 to RngAmt do begin
      Readln(F,Line);
      result[i]:= Line;
    end;
  end; // done or file isn't big enough for specified range
  close(F); 
end;


{ Same as ArrayLoadRng but array starts at [0] instead of [1] }
function ArrayLoadRng_0(FromLine:int32; ToLine:int32; const fname: astr): StrArray;
var F: text;
    Line: astr;
    i,
    i2: int32;
begin
  setlength(result,1);
  if not openFile(F, fname, 'r') then begin  
    result[0]:=FILE_ERR; EXIT;
  end;
  i:= -1; i2:= 0;
  setlength(result,(ToLine-FromLine)+1); //we know the exact length that the array will be  +2 because range 4 to 6 is three lines, not two. So +1
  while not ( Eof(F) ) and ( i < ToLine-1 ) do
  begin
    inc(i);      //current line we are at
    Readln(F,Line);
    // in the correct range
    if i >= FromLine-1 then begin
      result[i2]:= Line;
      inc(i2);
    end;
  end;
  close(F); //close file
end;


{ get n amount of lines at the end of the text file, returns -1NF if file not found }
function GetLastLns(const fname: astr; NumLines: int32): astr;
var LineCnt: int32;
    StartAt: int32;
    F: text;
    i: int32; //loop
    C: char;
begin
  result:= ''; //safety
  LineCnt:= GetLineCount(fname);
  if not openFile(F, fname, 'r') then begin 
    result:= FILE_ERR; 
    exit;
  end;
  // the line we will start reading at
  StartAt:= LineCnt - NumLines;
  // pass by all lines until we arrive at the designated line
  for i:= 1 to StartAt do Readln(F);
  //now we can grab all the characters
  while not Eof(f) do begin
    Read(F, C);
    result:= result + C;
  end;
  close(f);
end;

// remove duplicate strings from an array of strings that is sorted. Returns
// number of duplicates found, 0 if none are found
// note: will not work unles string array is sorted!
function RemoveDupStrings(var InputStrArray: StrArray): int32;
var i, i2: int32; 
    NonDupeStrArray: StrArray;
begin
  result:= -1; //safety
  SetLength(NonDupeStrArray, 0);
  if length(InputStrArray) = 0 then EXIT;
  if length(InputStrArray) = 1 then EXIT; //must be at least length of 2 to have a duplicate
//  setlength(NonDupeStrArray, 1);

  //give ourselves an extra buffer zone (1 additional blank string) at the end of the array, so we can check all the strings in the below loop (otherwise runtime error when adding 1 to i)
  setlength(InputStrArray, length(InputStrArray) + 1);

  i2:= 0;
  for i:= 0 to length(InputStrArray) - 2 do //up until the second last string in the array (last string is checked second last)
  begin
    // only record the string if it is not a duplicate
    if (InputStrArray[i] <> InputStrArray[i + 1]) then begin
      setlength(NonDupeStrArray, length(NonDupeStrArray) + 1);
      NonDupeStrArray[i2]:= InputStrArray[i];
      inc(i2);
    end;
  end;

  result:= length(InputStrArray) - length(NonDupeStrArray) - 1;
  setlength( InputStrArray, length(NonDupeStrArray) );
  InputStrArray:= NonDupeStrArray;
  //get rid of extra buffer zone at end of array
//  setlength(InputStrArray, length(InputStrArray) - 1 );
  { todo: optimize length() calls and setlength() where possible }
end;


(* Other functions to work on, or not working yet...





{ NOT WORKING YET

  Load only certain lines into a string var. Example: lines 11,13,21
  Note: appends an extra carriage return to end of string }
function StrLoadExact(LineNumsArray:LnArray; fname:string): astr;
begin
//LnArray
end;

*)

(*

{ NOT WORKING YET
  Load only certain lines into a string array. Example: lines 11,13,21,8,6
  into array[1] to array[5]
  note: array starts at 1 not 0
        -lines specified in LineNumsArray can be in any order.. but
         ArrayLoadExactC will be faster, if you specify 6,8,11,13,21 instead.
         This is because ArrayLoadExact has to sort your stuff for you first,
         but this may be convenient at times.}
function ArrayLoadExact(LineNumsArray:LnArray; fname:astr): StrArray;
var
  F: text;
  Line: astr;
  i,
  i2,
  i3:int32;
  GotLines: boo; //to signal that we have 'got' all the lines we need
begin
  setlength(result,1);
  if FileExists(fname) = false then
  begin
    result[0]:=''; //returns nothing if file not found
    exit;
  end;
  result[0]:=''; //safety
  GotLines:= false; //initialize as false
  Assign(F, fname);
  Reset(F);
  i:= 0; i2:= 1; i3:= 0;
  setlength(result,length(LineNumsArray)+1); //we know the exact length that the array will be
  while not ( Eof(F) ) and ( GotLines <> true ) do
  begin
    inc(i);      //current line we are at
    Readln(F,Line);
   {if LineNumsArray[i3] = i then
    begin
      inc(i3);
      result[i3+1]:= Line;
    end;  }
    for i3:= 0 to length(LineNumsArray)-1 do //optimize this
    begin
     if LineNumsArray[i3] = i then //we are on a line specified
       result[i3+1]:= Line;
    end;
    if i = LineNumsArray[length(LineNumsArray)-1] then   //we've 'gotten' all the lines we need
      GotLines:= true;         //so exit this while loop
  end;
  close(F); //close up file
end;

*)
(*

{ NOT WORKING YET
  Same as ArrayLoadExact except array starts at [0] instead of [1] }
function Array0LoadExact(LineNumsArray:LnArray; fname:astr): StrArray;
var
  F: text;
  Line: astr;
  i,
  i2,
  i3:int32;
  GotLines: boo; //to signal that we have 'got' all the lines we need
begin
  setlength(result,1);
  if FileExists(fname) = false then
  begin
    result[0]:=''; //returns nothing if file not found
    exit;
  end;
  result[0]:=''; //safety
  GotLines:= false; //initialize as false
  Assign(F, fname);
  Reset(F);
  i:= -1; i2:= 0; i3:= 0;
  setlength(result,length(LineNumsArray)); //we know the exact length that the array will be
  while not ( Eof(F) ) and ( GotLines <> true ) do
  begin
    inc(i);      //current line we are at
    Readln(F,Line);
   {if LineNumsArray[i3] = i then
    begin
      inc(i3);
      result[i3]:= Line;
    end;  }
    for i3:= 0 to length(LineNumsArray)-1 do
    begin
     if LineNumsArray[i3] = i then //we are on a line specified
       result[i3]:= Line;
    end;
    if i = LineNumsArray[length(LineNumsArray)-1] then   //we've 'gotten' all the lines we need
      GotLines:= true;         //so exit this while loop
  end;
  close(F); //close up file
end;

*)

{ TODO FUNCTIONS }

// Save a string directly to a file
{ function StrSaveFile(InputString:astr; fname: astr): boo; }

// Save an array of strings directly to a file
{ function ArraySaveFile(InputArray: StrArray; fname: astr): boo; }


{ OLD FUNCTIONS }


(* This one was a little bit slower

function StrLoadFile(const fname: astr): astr;
var
  F: file;
  InputFileSize: int32;
  Buffer: Pointer;
begin
  result:= ''; // safety
  FileMode:= fmOpenRead;
  // TODO: Binary open file routine like Jeff's for text files, w/error checking
  AssignFile(F, fname);
  Reset(F, GetFileSize(fname));
  InputFileSize:= GetFileSize(fname);
  SetLength(Result, InputFileSize);
  Buffer:= PChar(Result);
  BlockRead(F, Buffer^, 1);
  CloseFile(F);
end;
*)

end.





