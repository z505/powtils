unit CgiVariableUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CollectionUnit;

type

  { TCgiVar }

  TCgiVar= class (TNameStrValue)
  private

  public
    function ToString: AnsiString;

  end;

  {
    This class loads and hold the cgi variables information.
  }

  { TCgiVariableCollection }

  TCgiVariableCollection= class (TNameValueCollection)
  private
    function GetCgiVar (Index: Integer): TCgiVar;
    function GetCgiVarByName (VarName: AnsiString): TCgiVar;
    function GetCgiVarValueByName (VarName: AnsiString): AnsiString;
    function GetText: AnsiString;

  public
    property CgiVar [Index: Integer]: TCgiVar read GetCgiVar;
    property CgiVarByName [VarName: AnsiString]: TCgiVar read GetCgiVarByName;
    property CgiVarValueByName [VarName: AnsiString]: AnsiString read GetCgiVarValueByName;

    property Text: AnsiString read GetText;

    procedure LoadFromString (const Str: AnsiString);

  end;



implementation
uses
  WebStringUnit;

{ TCgiVar }

function TCgiVar.ToString: AnsiString;
begin
  Result:= FName+ ':'+ Value;

end;

{ TCgiVariableCollection }

function TCgiVariableCollection.GetCgiVar (Index: Integer): TCgiVar;
begin
  Result:= NameValue [Index] as TCgiVar;

end;

function TCgiVariableCollection.GetCgiVarByName (VarName: AnsiString): TCgiVar;
begin
  Result:= NameValueByName [VarName] as TCgiVar;

end;

function TCgiVariableCollection.GetCgiVarValueByName (VarName: AnsiString): AnsiString;
begin
  try
    Result:= CgiVarByName [VarName].Value

  except
    on e: ENameNotFound do
      Result:= '';

  end;

end;

function TCgiVariableCollection.GetText: AnsiString;
var
  i: Integer;

begin
  Result:= '';
  for i:= 0 to Count- 1 do
    Result:= Result+ CgiVar [i].ToString+ ';';

end;

procedure TCgiVariableCollection.LoadFromString (const Str: AnsiString);
var
  VarName, VarValue: AnsiString;
  PCh, PEndChar: PChar;

begin
  if Length (Str)= 0 then
    Exit;

  PCh:= @Str [1];
  Dec (PCh);
  PEndChar:= @Str [Length (Str)];

  while PCh<> PEndChar do
  begin
    VarName:= '';
    VarValue:= '';

    while PCh<> PEndChar do
    begin
      Inc (PCh);
      if PCh^= '=' then
        Break
      else if PCh^= '\' then
      begin
        VarName:= VarName+ PCh^;
        Inc (PCh);
        VarName:= VarName+ PCh^;

      end
      else
        VarName:= VarName+ PCh^;


    end;

    while PCh<> PEndChar do
    begin
      Inc (PCh);

      if PCh^= '&' then
        Break
      else if PCh^= '\' then
      begin
        VarValue:= VarValue+ PCh^;
        Inc (PCh);
        VarValue:= VarValue+ PCh^;

      end
      else
        VarValue:= VarValue+ PCh^;

    end;

    URLDecode (VarName);
    URLDecode (VarValue);
    Self.AddNameValue (TCgiVar.Create (VarName, VarValue));

  end;

end;

end.

