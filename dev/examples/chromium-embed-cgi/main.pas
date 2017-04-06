Unit Main;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType, ExtCtrls,
  process,
  cef3types, cef3lib, cef3intf, cef3lcl,
  Handler, cef3gui; // custom render process handler

Type

  { TMainform }

  TMainform = class(TForm)
    BLoadExeOutput : TButton;
    Button1: TButton;
    Button2: TButton;
    Chromium : TChromium;
    Label1: TLabel;
    lbColor: TListBox;
    Log : TMemo;
    procedure BLoadExeOutputClick(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

Var
  Mainform : TMainform;

Implementation

{$R *.lfm}

{ TMainform }

procedure DoLog(s: string);
begin
  // dummy, fill in logging code here
end;

function RunProcess(const Binary: string; w,h, color: string; args: TStrings; out CgiOut: TStringList): boolean;
const
  BufSize = 2048;
var
  p: TProcess;
  Buf: string;
  Count: integer;
  i: integer;
  LineStart: integer;
  OutputLine: string;
begin
  p := TProcess.Create(nil);
  try
    p.Environment.Add('HTTP_REFERER=test');
    p.Environment.Add('OTHERVAR=testdata');

    p.Environment.Add('SVGWIDTH='+w);
    p.Environment.Add('SVGHEIGHT='+h);
    p.Environment.Add('SVGCOLOR='+color);

    p.Executable := Binary;

    p.Options := [poUsePipes, poStdErrToOutPut];
    //    p.CurrentDirectory := ExtractFilePath(p.Executable);
    p.ShowWindow := swoHIDE;

    p.Parameters.Assign(args);
    DoLog('Running command '+ p.Executable +' with arguments: '+ p.Parameters.Text);
    p.Execute;

    { Now process the output }
    OutputLine := '';
    SetLength(Buf, BufSize);
    repeat
      if (p.Output <> nil) then
      begin
        Count := p.Output.Read(Buf[1],Length(Buf));
      end
      else
        Count := 0;
      LineStart := 1;
      i := 1;
      while i<=Count do
      begin
        if Buf[i] in [#10,#13] then
        begin
          OutputLine := OutputLine+Copy(Buf,LineStart,i-LineStart);
          CgiOut.Add(OutputLine);
          OutputLine := '';
          if (i < Count) and (Buf[i+1] in [#10,#13]) and (Buf[i] <> Buf[i+1]) then
            inc(i);
          LineStart := i+1;
        end;
        inc(i);
      end;
      OutputLine := Copy(Buf,LineStart,Count-LineStart+1);
    until Count = 0;
    if OutputLine <> '' then
      CgiOut.add(OutputLine);
    p.WaitOnExit;
    Result := p.ExitStatus = 0;
    if not Result then
      CgiOut.add('Command '+ p.Executable +' failed with exit code: '+ inttostr(p.ExitStatus));
  finally
    FreeAndNil(p);
  end;
end;

// load executable that displays html/javascript to stdout
function LoadCGI(prog, w, h, color: string): string;
var
  output, args: TStringList;
begin
  args := TStringList.create;
  args.text := '';
  output := TStringList.create;
  RunProcess(prog, w, h, color, args, output);
  result := output.text;
  output.free;
  args.free;
end;

function MakeColor: string;
var i: integer;
begin
  result := '';
  i := Mainform.lbColor.ItemIndex;
  if i < 0 then exit;
  result := Mainform.lbColor.Items[i];
end;

const
  EXTENSION = {$ifdef windows}'.exe'{$else}''{$endif};
  CGIPROG = 'progcgi' + EXTENSION;

procedure TMainform.BLoadExeOutputClick(Sender: TObject);
var s: string;
begin
  if not FileExists(CGIPROG) then begin
    ShowMessage('Program not found: '+ CGIPROG +'. Did you compile '+ CGIPROG +' first?');
    exit;
  end;
  s := LoadCGI(CGIPROG, '100', '100', MakeColor);
  Chromium.Browser.MainFrame.LoadString(s, 'about:blank');
end;


end.
