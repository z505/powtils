unit SEResultParserUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TEntryConfig }

  TEntryConfig= class (TObject)
  private
    NeedEncoding: Boolean;
    Start: TStringList;
    Fin: TStringList;
    
  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;
    
  end;
  
  TResultConfig= class (TObject)

  end;
  
  { TSEParser }

  TSEParser= class (TObject)
  private
    ResultCountConfig: TEntryConfig;
    LinkConfig: TEntryConfig;
    TitleConfig: TEntryConfig;
    SummaryConfig: TEntryConfig;
    
  public
    constructor Create (AnStream: TStream);
    destructor Destroy; override;
  
  end;
  
implementation
uses
  StreamUnit;
  
{ TEntryConfig }

constructor TEntryConfig.Create (AnStream: TStream);
var
  Str: String;
  n: Integer;
  
begin
  inherited Create;

  Str:= UpperCase (ReadLineFromStream (AnStream));
  NeedEncoding:= Str= 'TRUE';

  n:= StrToInt (ReadLineFromStream (AnStream));
  Start:= TStringList.Create;

  while 0< n do
  begin
    Str:= ReadLineFromStream (AnStream);
    Start.Add (Str);
    Dec (n);

  end;

  n:= StrToInt (ReadLineFromStream (AnStream));
  Fin:= TStringList.Create;

  while 0< n do
  begin
    Str:= ReadLineFromStream (AnStream);
    Fin.Add (Str);
    Dec (n);

  end;

end;

destructor TEntryConfig.Destroy;
begin
  Start.Free;
  Fin.Free;

  inherited Destroy;
  
end;

{ TSEParser }

constructor TSEParser.Create (AnStream: TStream);
begin
  inherited Create;
  
  ResultCountConfig:= TEntryConfig.Create (AnStream);
  LinkConfig:= TEntryConfig.Create (AnStream);
  TitleConfig:= TEntryConfig.Create (AnStream);
  SummaryConfig:= TEntryConfig.Create (AnStream);
  
end;

destructor TSEParser.Destroy;
begin
  ResultCountConfig.Free;
  LinkConfig.Free;
  TitleConfig.Free;
  SummaryConfig.Free;
  
  inherited Destroy;
end;

end.

