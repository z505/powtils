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
    function ToString: String;

  end;

  {
    This class loads and hold the cgi variables information.
  }

  { TCgiVariableCollection }

  TCgiVariableCollection= class (TNameValueCollection)
  private
    function GetCgiVar (Index: Integer): TCgiVar;
    function GetCgiVarByName (VarName: String): TCgiVar;
    function GetCgiVarValueByName (VarName: String): String;
    function GetText: String;

  public
    property CgiVar [Index: Integer]: TCgiVar read GetCgiVar;
    property CgiVarByName [VarName: String]: TCgiVar read GetCgiVarByName;
    property CgiVarValueByName [VarName: String]: String read GetCgiVarValueByName;

    property Text: String read GetText;

    procedure LoadFromString (const Str: String);

  end;



implementation
uses
  WebStringUnit;

{ TCgiVar }

function TCgiVar.ToString: String;
begin
  Result:= FName+ ':'+ Value;

end;

{ TCgiVariableCollection }

function TCgiVariableCollection.GetCgiVar (Index: Integer): TCgiVar;
begin
  Result:= NameValue [Index] as TCgiVar;

end;

function TCgiVariableCollection.GetCgiVarByName (VarName: String): TCgiVar;
begin
  Result:= NameValueByName [VarName] as TCgiVar;

end;

function TCgiVariableCollection.GetCgiVarValueByName (VarName: String): String;
begin
  Result:= CgiVarByName [VarName].Value;

end;

function TCgiVariableCollection.GetText: String;
var
  i: Integer;

begin
  Result:= '';
  for i:= 0 to Count- 1 do
    Result:= Result+ CgiVar [i].ToString+ ';';

end;

procedure TCgiVariableCollection.LoadFromString (const Str: String);
var
  VarName, VarValue: String;
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

