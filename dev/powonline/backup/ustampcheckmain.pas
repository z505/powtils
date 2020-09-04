unit uStampCheckMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, pwdirutil;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bCheckTimeStamps: TButton;
    bAddFileToProject: TButton;
    bChangeFile: TButton;
    edFname: TEdit;
    edProjectDir: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mStatus: TMemo;
    procedure bAddFileToProjectClick(Sender: TObject);
    procedure bChangeFileClick(Sender: TObject);
    procedure bCheckTimeStampsClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure status(s: string);
begin
  frmMain.mStatus.lines.add(s);
end;

procedure TfrmMain.bCheckTimeStampsClick(Sender: TObject);
var
  FilesChanged: boolean;
  paths1, paths2, paths3, paths4, paths5: TPaths;
  AllPaths: TPaths;
  i, j: integer;
  SL: TStringList;
begin
  FilesChanged := true;
  GetSubdirFiles(ExtractFileDir(Application.ExeName), '*.pas', paths1);
  GetSubdirFiles(ExtractFileDir(Application.ExeName), '*.pp', paths2);
  GetSubdirFiles(ExtractFileDir(Application.ExeName), '*.dpr', paths3);
  GetSubdirFiles(ExtractFileDir(Application.ExeName), '*.lpr', paths4);
  GetSubdirFiles(ExtractFileDir(Application.ExeName), '*.inc', paths5);
  AddPaths([paths1,paths2,paths3,paths4,paths5], AllPaths, SL);

  for i:= 0 to SL.count-1 do
  begin
    status('SL count: ' + inttostr(SL.count));
    status('Checking..' +SL.items[i]);
    status('Changed');
  end;

  for j:= 0 to AllPaths.count-1 do
  begin
    status('count: ' + inttostr(AllPaths.count));
    status('Checking..' +AllPaths.items[j].path + PathDelim + AllPaths.items[j].fname);
    status('Changed');
  end;

  if FilesChanged = false then
    status('nothing has changed');
end;

function ProjectFilePath: string;
begin
  result := frmMain.edProjectDir.text + PathDelim + frmMain.edFname.text;
end;

procedure TfrmMain.bAddFileToProjectClick(Sender: TObject);
var
  FileSL: TStringList;
  fpath: string;
begin
  fpath := ProjectFilePath;
  FileSL := TStringList.create;
  try
    FileSL.Add('unit example;');
    FileSL.Add('');
    FileSL.Add('interface');
    FileSL.Add('');
    FileSL.Add('implementation');
    FileSL.Add('');
    FileSL.Add('end.');
    FileSL.SaveToFile(fpath);
  finally
    FileSL.free; FileSL := nil;
  end;
end;

procedure TfrmMain.bChangeFileClick(Sender: TObject);
var
  FileSL: TStringList;
  RandomNum1, RandomNum2: integer;
  NewNum: string;
begin
  RandomNum1 := Random(10000);
  RandomNum2 := Random(2000);
  NewNum := inttostr(RandomNum1+RandomNum2);
  FileSL := TStringList.create;
  try
    FileSL.LoadFromFile(ProjectFilePath);
    // add to file deleting last "end." line
    FileSL.Delete(FileSL.count-1);
    FileSL.Add('function ReturnInteger' + NewNum + ': integer;');
    FileSL.Add('begin');
    FileSL.Add('  result := '+ NewNum + ';');
    FileSL.Add('end;');
    FileSL.Add('');
    FileSL.Add('end.');
    FileSL.SaveToFile(ProjectFilePath);
  finally
    FileSL.free; FileSL := nil;
  end;

end;



end.

