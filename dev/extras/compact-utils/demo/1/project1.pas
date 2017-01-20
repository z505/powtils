{ On windows, this program compiled in only 31 Kilobytes, yet still uses a
  stringlist with equivilent features of the one in Classes.pp but no
  fancy stuff like rude exceptions thrown in the end users face.. 

  So watch yourself more carefully when doing things like IntToStr.. use
  the IsNum function from StrFilter.pas if you need to verify a string really
  is a number... and don't expect the program to be interupted with some 
  rude message that the end user knows nothing about (exception) 
  
  Lars (L505)
  http://z505.com  }

program project1;

{$MODE delphi}{$H+}

uses
 CompactUtils, CompactSysUtils;

var
  SmallList: PStrList;
  s: string;
  i: integer;
  
const
 {$ifdef windows}
  TEST_PATH = 'c:\test\test.txt';
 {$endif}
 {$ifdef unix}
  TEST_PATH = '/var/test.txt';
 {$endif}
begin

  SmallList:= NewStrList;
  SmallList.add('test0');
  SmallList.add('test1');
  SmallList.add('test2');
  writeln(SmallList.text);
  SmallList.free; SmallList:= nil;

  writeln('Extracted file path: ', ExtractFilePath(TEST_PATH));
  writeln('Extracted file name: ', ExtractFileName(TEST_PATH));
  writeln('Extracted file drive: ', ExtractFileDrive(TEST_PATH));
  
  s:= inttostr(649);
  writeln(s);

  // does the same thing, but compatible with KOL naming scheme
  s:= int2str(649);
  writeln(s);

  i:= strtoint('64366');
  writeln(i);
  
  writeln(uppercase('somE UPpErCasE tExt FoR the pC to spit'));
  writeln(lowercase('somE lowerCaSe tExt FoR the pC to spit'));

  s:= BoolToStr(true);
  writeln(s);
  
  s:= BoolToStr(false);
  writeln(s);

  s:= FloatToStr(1.342);
  writeln(s);

  readln;
end.

