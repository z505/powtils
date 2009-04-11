unit PipeWrapperUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type

  { TWrapperClass }

  TWrapperClass= class (TObject)
  private
    FileHandle: TextFile;
  public
    constructor Create (PipeFileName: String; CreateIfNotExists: Boolean= True);
    
    function CreatePipe (PipeFileName: String): Boolean;

  end;
  
implementation

{ TWrapperClass }

constructor TWrapperClass.Create (PipeFileName: String;
  CreateIfNotExists: Boolean);
begin
  inherited Create;
  
end;

function TWrapperClass.CreatePipe (PipeFileName: String): Boolean;
begin

end;

end.

