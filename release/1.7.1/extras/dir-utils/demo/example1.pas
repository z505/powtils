program example1; {$ifdef FPC}{$mode objfpc}{$H+}{$endif} {$apptype console}

uses
  pwdirutil in '..\..\..\main\pwdirutil.pas';



var
  DirContent: TDirContents;
  DirNames: TDirNames;
  FileNames: TFileNames;
  i: integer;


begin
  GetDirContent_NoDots('c:\winnt', DirContent);

  // note: read only directories are skipped, and ../ ./ are skipped
  writeln('DirCount: ', dircontent.Dircount);
  writeln('DIRECTORIES IN C:\WINNT (SKIPPING READ-ONLY):');
  writeln;
  // list all the directories
  for i:= 0 to dircontent.dircount-1 do
    writeln('  ' + dircontent.Dirs[i]);
  writeln;

  // note: read only files are skipped
  writeln('FileCount: ', dircontent.FileCount);
  writeln('FILES IN C:\WINNT (SKIPPING READ-ONLY):');
  writeln;
  // list all the files
  for i:= 0 to dircontent.filecount-1 do
    writeln('  ' + dircontent.files[i]);
  writeln;
  
  // if you only want to retrieve directories and not files, use GetSubDirs
  GetSubDirs('C:\', DirNames);
  writeln('DirCount: ', DirNames.Count);
  writeln('DIRECTORIES IN C:\ (SKIPPING READ-ONLY):');
  writeln;
  for i:= 0 to dirnames.count-1 do
    writeln('  ' + dirnames.dirs[i]);
  writeln;
    
  // if you only want to retrieve files and not directories, use GetFiles
  GetFiles('C:\', FileNames);
  writeln('DirCount: ', FileNames.Count);
  writeln('FILES IN C:\ (SKIPPING READ-ONLY):');
  writeln;
  for i:= 0 to filenames.count-1 do
    writeln('  ' + filenames.files[i]);
  writeln;


  readln;
end.
