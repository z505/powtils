program ParserProgram;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, {heaptrc, }SEResultParserUnit
  { you can add units after this }, ParserUnit, SearchResultUnit;

var
  Parser: TParser;
  AnStream: TFileStream;
  InputText: TStringList;
  FileName: String;
  Text: String;
  SearchResults: TSearchResultCollection;

begin
  Write ('Configure file:');
//  ReadLn (FileName);
  FileName:= 'MSN.Conf';
  AnStream:= TFileStream.Create (FileName, fmOpenRead);

  Parser:= TParser.Create (AnStream);
  AnStream.Free;

  Write ('File name to be parsed:');
//  ReadLn (FileName);
  FileName:= 'MIranTehranPer.html';
  InputText:= TStringList.Create;
  InputText.LoadFromFile (FileName);

  Text:= InputText.Text;
  SearchResults:= Parser.Parse (@(Text [1]), Length (Text));
  WriteLn (SearchResults.ToString);
  SearchResults.Free;

  Parser.Free;

end.

