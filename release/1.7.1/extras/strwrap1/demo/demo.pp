{ Demo program to show usefulness of StrWrap1 unit.
   -Load a file directly into a string var
   -Get first line of a file into a string var
   -Get Nth line of a file into a string var
   -Get a range of lines from a file into a string var
   -Get a range of lines from a file into a string array
   -plus more

 The advantages of loading a piece of your text file into a string instead
 of a stringlist are numerous. Many times you don't need any stringlist
 sorting.. you are just doing something simple. Many times you don't
 want to bloat up your code with free, create, etc. and other object
 oriented class code.. just to do something simple.

 Memory is saved, because a stringlist loads the entire file into memory!
 Imagine if you just wanted to grab the first line of a large file because
 the first line contained a setting, or the title of the file? The GetLn
 functions then can help you there.
 
 Sometimes, you do want to load the entire file into memory, and you want to
 access many of the lines. But you if don't need a stringlist and all that
 bloat that comes with it? Well, use ArrayLoadFile, StrLoadFile, and load a
 file directly into an array or string.

 Regards,
  Lars aka L505
  http://z505.com

}

program demo;

{$mode objfpc}{$H+}

uses
 sysutils, // for IntToStr
 StrWrap1; // wrapper unit for some string functions
 

var
  FileLoaded: string;   // the string we are going to load a file directly into
  SomeLines: string;   // first few lines got from a file
  RngLines: string;   // a range of lines got from a file
  FileLines,              // the array we are going to load a file into
  FileLines2,
  FileLines3: StrArray;
  RngLinesAry: array of string; //a range of lines put into an array
  ExactLns: array of string; //some lines picked out from a file
  iLoc: integer;
  LengthOfFile: integer;
  LinesArray: array of integer;

const
  DemoFile = 'demotext.txt';



begin

  writeln('Welcome to the StrWrap demo program <ENTER>');
  writeln;
  readln;
  
{-- get last N lines from demo file ..                                       --}
  writeln('FIRST DEMO: GetLastLns function <ENTER>');
  readln;
  writeln('The last 10 lines of the file are:');
  writeln;
  writeln(GetLastLns(DemoFile, 10));
  writeln;
  writeln('Next example...<ENTER>');
  readln;
{------------------------------------------------------------------------------}

  
{-- get line count from demo file ..                                         --}
  writeln('FIRST DEMO: GetLineCount function <ENTER>');
  readln;
  writeln('The line count of the file is:');
  writeln;
  writeln(GetLineCount(DemoFile));
  writeln;
  writeln('Next example...<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- get line 1 from demo file ..                                             --}
  writeln('NEXT DEMO: GetLn1 function <ENTER>');
  readln;
  writeln('The first line of the file is:');
  writeln;
  writeln(GetLn1(DemoFile));
  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}

{-- Load a text file right into a string..and display it                     --}
  writeln('DEMO: StrLoadFile function <ENTER>');
  readln;
  writeln('The entire contents of your file are as follows:');
  writeln;
  FileLoaded:= StrLoadFile(DemoFile);
  writeln(FileLoaded);

  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- get line 5 from demo file ..                                             --}
  writeln('DEMO: GetLnN function <ENTER>');
  readln;
  writeln('Line 9 in your file is as follows...');
  writeln;
  writeln(GetLnN(9, DemoFile));

  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- Load a text file right into an array of strings..and display some lines  --}
  writeln('DEMO: ArrayLoadFile function example 1 <ENTER>');
  readln;
  writeln('Lines 7, 8, 9, and 15 of your file are as follows:');
  writeln;
 // You must error check for array length, incase file does not exist..or the
 // file is for some reason not the one you meant to load. Otherwise you may be
 // out of range. This is only if you are displaying some random lines from the
 // file. This is because length of array is 1 if the file doesn't exist, and
 // you are trying to access [7] [15], etc.
  FileLines:= ArrayLoadFile(DemoFile);
  if length(FileLines) >= 15 then  //error check to make sure file was loaded, and that there are in fact at least 15 lines in the file for us to display
  begin
    writeln(FileLines[7]);
    writeln(FileLines[8]);
    writeln(FileLines[9]);
    writeln(FileLines[15]);
  end;

  writeln;
  writeln('DEMO: ArrayLoadFile function example 2 <ENTER>');
  readln;
  writeln('Entire contents of your file are as follows:');
 // If you want to display the entire file contents:
 // not as much checking needed as above example. Just a blank will be
 // displayed if file does not exist - since you already are checking length in
 // the loop, and it will be of length 1 and blank if the file does not exist.
  FileLines3:= ArrayLoadFile(DemoFile);
  LengthOfFile:= length(FileLines3)-1; //since array starts at [1] we must take into account extra [0]
  writeln('(Length of file is: '+ inttostr(LengthOfFile) + ' lines)');
  for iLoc:= 1 to LengthOfFile do
    writeln(FileLines3[iLoc]); //display entire file contents

  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- Load only the first few lines of a text file into a string directly.     --}
{-- Without loading the entire file into the memory                          --}
  writeln('DEMO: StrLoadLns function <ENTER>');
  readln;
  writeln('First 7 Lines of your file are as follows:');
  writeln;
  // load the first 7 lines of the file into a string, without loading entire
  // file into memory
  SomeLines:= StrLoadLns(7,DemoFile);
  writeln(SomeLines);
  
  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- Load only the first few lines of a text file into an array of strings.   --}
{-- without loading the entire file into the memory                          --}
  writeln('DEMO: ArrayLoadLns function <ENTER>');
  readln;
  writeln('First 10 Lines of your file are as follows:');
  writeln;
  // load the first 10 lines of the file only
  FileLines2:= ArrayLoadLns(10, DemoFile);
  for iLoc:= 1 to 10 do
   writeln(FileLines2[iLoc]);

  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- Load a range of lines into an array of strings, without loading the      --}
{-- entire file into the memory                                              --}
  writeln('DEMO: ArrayLoadRng function <ENTER>');
  readln;
  writeln('Lines 18 to 24 of your file are as follows:');
  writeln;
  RngLinesAry:= ArrayLoadRng(18, 24, DemoFile); //Load lines 18 to 24 from file into a string
  for iLoc:= 1 to 7 do //we know 18,19,20,21,22,23,24 is 7 lines total (NOT 24-18=6! Rather, 18 THROUGH 24 = 7)
   writeln(RngLinesAry[iLoc]);

  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}


{-- Load a range of lines into a string, without loading the entire file     --}
{-- into the memory                                                          --}
  writeln('DEMO: StrLoadRng function <ENTER>');
  readln;
  writeln('Lines 8 to 16  of your file are as follows:');
  writeln;
  RngLines:= StrLoadRng(8, 16, DemoFile); //Load lines 8 to 16 from file into a string
  writeln(RngLines);
  
  writeln;
  writeln('Done that demo..<ENTER>');
  readln;
{------------------------------------------------------------------------------}



end.
