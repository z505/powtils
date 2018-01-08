{ Not tested on MacOS yet, this wants to be done, before Canada day

  Demo to ensure pwdirutils.pas will work on MacOS too and compile with FMX
  as it was originally not intended for it

}

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  pwdirutil;

{$R *.fmx}


procedure StatusLn(s: string); overload;
begin
  form1.Memo1.Lines.Add(s);
end;

procedure StatusLn(s: string; i: integer); overload;
begin
  StatusLn(s + inttostr(i));
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  fnames: TFileNames;
  i: integer;
begin
  // GetFiles(dir: astr; var res: TFileNames);
  GetFiles('sampledir', fnames);
  StatusLn('File count: ', fnames.Count);
  for i := Low(fnames.Files) to High(fnames.Files) do begin
    StatusLn(fnames.Files[i]);
    showmessage(fnames.Files[i]);
  end;

end;


end.
