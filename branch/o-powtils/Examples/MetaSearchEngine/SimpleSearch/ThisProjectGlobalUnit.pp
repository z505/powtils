unit ThisProjectGlobalUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GlobalUnit, ParserUnit, SpellCheckerUnit,
  PersaDictionaryUnit;
  
type

  { TMyGlobalObjectContainers }

  TMyGlobalObjectContainers= class (TGlobalObjectContainer)
  private
    FGParser: TParser;
    FMParser: TParser;
    FSpellChecker: TSpellChecker;
    FYParser: TParser;
    PersaDic: TPersaDic;

  public
    property GParser: TParser read FGParser;
    property MParser: TParser read FMParser;
    property YParser: TParser read FYParser;
    property SpellChecker: TSpellChecker read FSpellChecker;

    constructor Create;
    destructor Destroy; override;

  end;
  
var
  GlobalObjContainer: TMyGlobalObjectContainers;
  
implementation
uses
  FileStringsUnit, DictionaryTreeUnit, StreamUnit;
  
{ TMyGlobalObjectContainers }

constructor TMyGlobalObjectContainers.Create;
var
  AnStream: TFileStream;
  MyStream: TMyTextStream;
  Word: AnsiString;

begin
  inherited Create;

  AnStream:= TFileStream.Create ('GParser.Conf', fmOpenRead);
  FGParser:= TParser.Create (AnStream);
  AnStream.Free;

  AnStream:= TFileStream.Create ('YParser.Conf', fmOpenRead);
  FYParser:= TParser.Create (AnStream);
  AnStream.Free;

  AnStream:= TFileStream.Create ('MParser.Conf', fmOpenRead);
  FMParser:= TParser.Create (AnStream);
  AnStream.Free;

  AnStream:= TFileStream.Create ('SpellCheckerFile.txt', fmOpenRead);
  PersaDic:= TPersaDic.Create;
  PersaDic.LoadFromStream (AnStream);

  FSpellChecker:= TSpellChecker.Create (PersaDic);

  MyStream.Free;
  AnStream.Free;

end;

destructor TMyGlobalObjectContainers.Destroy;
begin
  FMParser.Free;
  FGParser.Free;
  FYParser.Free;
  FSpellChecker.Free;

  inherited Destroy;

end;

initialization
  GlobalObjContainer:= TMyGlobalObjectContainers.Create;
  
finalization
  GlobalObjContainer.Free;
  
end.

