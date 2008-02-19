{ CsvUtil demo (creates MS Excel compatible CSV.. but you can specify your own
  custom delimiter to whatever you want by using overloaded functions! 

  Author,
  Lars (L505)
  http://z505.com
}

program CsvDemo; {$ifdef fpc}{$mode objfpc} {$H+}{$endif}

uses
  pwcsvutil in '..\..\..\main\pwcsvutil.pas',
  strwrap1 in '..\..\..\main\strwrap1.pas',
  pwcapstr in '..\..\..\main\pwcapstr.pas';

const
  LF = #10;

// CSV ansistring 
procedure Example1;
var
  newstr: string = '';
const
  FNAME = 'testcsv1.csv';
begin
  CsvAddCell('test', newstr);
  CsvAddCell('testing 123', newstr);
  CsvAddCell('testing "and" 456', newstr);
  CsvStartNewRow(newstr);
  CsvAddCell('tester', newstr);
  CsvStartNewRow(newstr);
  CsvAddCell('testing12345', newstr);
  CsvAddCell('testing345', newstr);
  CsvAddCell('multi line '#10'cell is here', newstr);
  CsvAddCell('multi and another multiline '#10'cell is here', newstr);
  CsvEnd(newstr);
  writeln(newstr);

  strsavefile('testcsv1.csv', newstr);
  writeln('saved file as: ', FNAME);
  writeln('Done example 1');
  writeln('---------------------------------');
end;

// capstring is much more effecient than an ansistring
procedure Example2;
var
  newstr: TCapstr;
const
  FNAME = 'testcsv2.csv';
begin
  capstr.resetbuf(@newstr);
  CsvAddCell('test', newstr);
  CsvAddCell('testing 123', newstr);
  CsvAddCell('testing "and" 456', newstr);
  CsvStartNewRow(newstr);
  CsvAddCell('tester', newstr);
  CsvStartNewRow(newstr);
  CsvAddCell('testing12345', newstr);
  CsvAddCell('testing345', newstr);
  CsvAddCell('multi line '#10'cell is here', newstr);
  CsvAddCell('multi and another multiline '#10'cell is here', newstr);
  CsvEnd(newstr);
  writeln(newstr.data);

  strsavefile('testcsv2.csv', newstr.data);
  writeln('saved file as: ', FNAME);
  writeln('Done example 2');
  writeln('---------------------------------');
end;

const
  LOOP_CNT = 100000; // GOOD SPEED TEST


{ capstring performance test }
procedure Example3;
var
  newstr: TCapstr;
  i: integer;
const
  FNAME = 'testcsv3.csv';
begin
  capstr.resetbuf(@newstr);
  for i:= 1 to LOOP_CNT do 
  begin
    CsvAddCell('test', newstr);
    CsvAddCell('testing 123', newstr);
    CsvStartNewRow(newstr);
    CsvAddCell('tester', newstr);
    CsvStartNewRow(newstr);
    CsvAddCell('testing12345', newstr);
    CsvAddCell('testing345', newstr);
    CsvAddCell('multi line '#10'cell is here', newstr);
    CsvStartNewRow(newstr);
  end;

  CsvEnd(newstr);

  strsavefile(FNAME, newstr.data);
  writeln('saved file as: ', FNAME);
  writeln('Done example 3');
  writeln('---------------------------------');
end;

{ ansistring performance is shitty :-) }
procedure Example4;
var
  newstr: string;
  i: integer;
const
  FNAME = 'testcsv4.csv';
begin
  for i:= 1 to LOOP_CNT do 
  begin
    CsvAddCell('test', newstr);
    CsvAddCell('testing 123', newstr);
    CsvStartNewRow(newstr);
    CsvAddCell('tester', newstr);
    CsvStartNewRow(newstr);
    CsvAddCell('testing12345', newstr);
    CsvAddCell('testing345', newstr);
    CsvAddCell('multi line '#10'cell is here', newstr);
    CsvStartNewRow(newstr);
  end;

  CsvEnd(newstr);
  
  strsavefile(FNAME, newstr);
  writeln('saved file as: ', FNAME);
  writeln('Done example 4');
  writeln('---------------------------------');
end;

// custom delimiter/encloser ability
procedure Example0;
var
  csv: TCsvData = (buf: ''; delim: ';', encloser: '"');
const
  FNAME = 'testcsv0.csv';
begin
  CsvAddCell('test', csv);
  CsvAddCell('testing 123', csv);
  CsvAddCell('testing "and" 456', csv);
  CsvStartNewRow(csv);
  CsvAddCell('tester', csv);
  CsvStartNewRow(csv);
  CsvAddCell('testing12345', csv);
  CsvAddCell('testing345', csv);
  CsvAddCell('multi line '#10'cell is here', csv);
  CsvAddCell('multi and another multiline '#10'cell is here', csv);
  CsvEnd(csv);
  writeln(csv.buf);
  StrSaveFile('testcsv2.csv', csv.buf);
  writeln('saved file as: ', FNAME);
  writeln('Done example 2');
  writeln('---------------------------------');
end;

begin
  writeln('Demo');
  writeln('---------------------------------');

  writeln('Example0... PLEASE WAIT...');
  Example0;
  writeln('Example1... PLEASE WAIT...');
  Example1;
  writeln('Example2...... PLEASE WAIT...');
  Example2;
  writeln('Example3...... PLEASE WAIT... (capstring: FAST)');
  Example3;
  writeln('Example4...... PLEASE WAIT... (ansistring: SHIT HITS FAN)');
  Example4;

  writeln('(Done program. hit enter)');
  writeln('>>> Please check output CSV files for data, such as testcsv1.csv');
  readln;
end.
