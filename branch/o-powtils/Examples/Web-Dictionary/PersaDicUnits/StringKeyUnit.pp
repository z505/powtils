unit StringKeyUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DictionaryTreeUnit;

type
  { TStringKey }

  TStringKey= class (TAbstractKey)
  private
    FSize: Integer;

  protected
    function GetMaxValue: Integer; override;
    function GetMinValue: Integer; override;
    function GetLength: Integer; override;
    function GetValueAt (Index: Integer): Integer; override;

    FKey: String;

  public

    constructor Create (const Key: String= '');
    procedure SetKey (const Key: String);

  end;

  { TStringKeyWithWildChar }

  TStringKeyWithWildChar= class (TStringKey)
  private
    function GetIsWildChar(Index: Integer): Boolean;
  public
    property IsWildChar [Index: Integer]: Boolean read GetIsWildChar;
  end;

implementation

{ TStringKey }

function TStringKey.GetMaxValue: Integer;
begin
  Result:= 255;

end;

function TStringKey.GetMinValue: Integer;
begin
  Result:= 0;

end;

function TStringKey.GetLength: Integer;
begin
  Result:= FSize;// Length (FKey);

end;

function TStringKey.GetValueAt (Index: Integer): Integer;
begin
  Result:= Ord (FKey [Index+ 1]);

end;

constructor TStringKey.Create (const Key: String);
begin
  inherited Create;

  FKey:= Key;
  FSize:= System.Length (Key);

end;

procedure TStringKey.SetKey (const Key: String);
begin
  FKey:= Key;
  FSize:= System.Length (Key);

end;

{ TStringKeyWithWildChar }

function TStringKeyWithWildChar.GetIsWildChar (Index: Integer): Boolean;
const
  QuestionMark: Integer= Ord ('?');

begin
  Result:= ValueAt [Index]= QuestionMark;

end;

end.

