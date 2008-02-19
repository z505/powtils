{ Runs through and demos SDSDirect functions
  L505                                         }
  
program project1;
{$i defs.inc}

uses
  sdsd;

// write all SDS errors to STDOUT
procedure CustomErrLog(s: string);
begin
  writeln(s);
end;

var
  Cols: TSdsCols;
  Row1: TSDSFields;
  Row2: TSDSFields;
  Row3: TSDSFields;
  AnyRow: TSDSFields;
  delres: integer;
  delbool: boolean;

const
 { database name }
  DB1 = 'test.sds';
 { database columns }
  cFIRSTNAME = 0;
  cLASTNAME = 1;
  cSTREETNAME = 2;
  cCITY = 3;
  cCOUNTRY = 4;
 { database columns with id }
  ciID = 0;
  ciFIRSTNAME = 1;
  ciLASTNAME = 2;
  ciSTREETNAME = 3;
  ciCITY = 4;
  ciCOUNTRY = 5;

var
  SdsRes: TSDSResult;
  i: integer;
  tmpstr: string;
  rowres: TSDSRow;
begin
  // setup error reporting
  sdsd.sdserror:= {$ifdef fpc}@{$endif}CustomErrLog;
  setlength(Cols, 5);

  Cols[cFIRSTNAME].name:= 'FIRST NAME';
  Cols[cFIRSTNAME].dtype:= dVARCHAR;

  Cols[cLASTNAME].name:= 'LAST NAME';
  Cols[cLASTNAME].dtype:= dVARCHAR;

  Cols[cSTREETNAME].name:= 'STREET NAME';
  Cols[cSTREETNAME].dtype:= dVARCHAR;

  Cols[cCITY].name:= 'CITY';
  Cols[cCITY].dtype:= dVARCHAR;

  Cols[cCOUNTRY].name:= 'COUNTRY';
  Cols[cCOUNTRY].dtype:= dVARCHAR;

  setlength(row1, 5);
  row1[cFIRSTNAME]:= 'John';
  row1[cLASTNAME]:= 'Black';
  row1[cSTREETNAME]:= 'Mavil Drive';
  row1[cCITY]:= 'Victoria';
  row1[cCOUNTRY]:= 'Canada';

  setlength(row2, 5);
  row2[cFIRSTNAME]:= 'Joe';
  row2[cLASTNAME]:= 'Domingus';
  row2[cSTREETNAME]:= 'Chaud Street';
  row2[cCITY]:= 'Montreal';
  row2[cCOUNTRY]:= 'Canada';

  setlength(row3, 5);
  row3[cFIRSTNAME]:= 'Stacy';
  row3[cLASTNAME]:= 'Peterson';
  row3[cSTREETNAME]:= 'N/A';
  row3[cCITY]:= 'N/A';
  row3[cCOUNTRY]:= 'Canada';

  CreateTable(DB1, Cols);

  sdsd.InsertRow(DB1, row1);
  sdsd.InsertRow(DB1, row2);
  sdsd.InsertRow(DB1, row3);

  // delete row containing CITY with a value of N/A
  delres:= sdsd.deleterow(DB1, 'CITY', 'N/A');
  writeln('Total found and deleted with CITY named N/A: ', delres);

  // delete row 1
  delbool:= sdsd.deleterow(DB1, 1);
  if delbool then writeln('Row ', 1, ' deleted') else writeln('Row 1 not available');

  // try to delete an impossible row
  delbool:= sdsd.deleterow(DB1, 40000);
  if not delbool then writeln('Note: access to row 40000 attempted, it does not exist!');

  sdsd.InsertRow(DB1, row1); // insert again
  sdsd.InsertRow(DB1, row3); // insert again
  writeln('LAST ID ', GetLastID(DB1));

  writeln;
  writeln('The first names of humans living in Canada are:');
  SdsRes:= SelectAll(DB1, 'COUNTRY', 'Canada');
  if sdsres = nil then writeln('SDS result is nil!')
  else
  begin
    for i := 0 to length(sdsres) - 1 do
      writeln(SDSres[i][ciFIRSTNAME]);
    writeln;
    writeln('The surnames of humans living in Canada are:');
    for i := 0 to length(sdsres) - 1 do
      writeln(SDSres[i][ciLASTNAME]);
  end;
    
  // you can reuse the same pascal structure
  setlength(AnyRow, 5);
  AnyRow[cFIRSTNAME]:= 'Mandy';
  AnyRow[cLASTNAME]:= 'Travo';
  AnyRow[cSTREETNAME]:= 'Main Street';
  AnyRow[cCITY]:= 'New York';
  AnyRow[cCOUNTRY]:= 'USA';
  sdsd.InsertRow(DB1, AnyRow);
  // ...
  AnyRow[cFIRSTNAME]:= 'Farori';
  AnyRow[cLASTNAME]:= 'Vascali';
  AnyRow[cSTREETNAME]:= 'Itilia R1';
  AnyRow[cCITY]:= 'Rome';
  AnyRow[cCOUNTRY]:= 'Italy';
  sdsd.InsertRow(DB1, AnyRow);
  
  writeln;
  writeln('The cities of humans living in USA are:');
  SdsRes:= SelectAll(DB1, 'COUNTRY', 'USA');
  if sdsres = nil then writeln('SDS result is nil!')
  else
  begin
    for i := 0 to length(sdsres) - 1 do
      writeln(SDSres[i][ciCITY]);
  end;

  // check for non-existing row
  writeln;
  write('Are there any people named SMOKEY in the database? ');
  SdsRes:= SelectAll(DB1, 'FIRST NAME', 'SMOKEY');
  if sdsres = nil then writeln('SMOKEY not in database!');

  // add another Joe to the database in addition to Joe Domingus
  AnyRow[cFIRSTNAME]:= 'Joe';
  AnyRow[cLASTNAME]:= 'Splaton';
  AnyRow[cSTREETNAME]:= 'RR345 Highway 1';
  AnyRow[cCITY]:= 'Toronto';
  AnyRow[cCOUNTRY]:= 'Canada';
  sdsd.InsertRow(DB1, AnyRow);
  // check for Joe's in database
  writeln;
  write('How many people named Joe in the database? ');
  SdsRes:= SelectAll(DB1, 'FIRST NAME', 'Joe');
  if sdsres = nil then writeln('None!') else writeln(length(sdsres)) ;

  writeln;
  i:= UpdateField(DB1, 'FIRST NAME', 'Joe', 'FIRST NAME', 'Joseph');
  writeln('Updated fields with Joe to Joseph: ', i);

  i:= UpdateField(DB1, 'FIRST NAME', 'Joseph', 'COUNTRY', 'Germany');
  writeln('Updated fields with Joseph to the country Germany: ', i);

  i:= UpdateFieldByValue(DB1, 'COUNTRY', 'Germany', 'Canada');
  writeln('Updated fields containing Germany to contain Canada: ', i);

  writeln;
  // Racist database entry: make all Canadians not available and not on file
  AnyRow[cFIRSTNAME]:= 'N/A - NOT ON FILE';
  AnyRow[cLASTNAME]:= 'N/A - NOT ON FILE';
  AnyRow[cSTREETNAME]:= 'N/A - NOT ON FILE';
  AnyRow[cCITY]:= 'N/A - NOT ON FILE';
  AnyRow[cCOUNTRY]:= 'N/A - NOT ON FILE';
  i:= UpdateRow(DB1, 'COUNTRY', 'Canada', tsdsrow(AnyRow)); // all Canadians BANNED
  writeln('Canadians no longer in our database: ', i);
  
  writeln;
  // find someone's name who lives in new york
  tmpstr:= SelectField('FIRST NAME', DB1, 'CITY', 'New York');
  writeln(tmpstr + ' lives in New York');

  writeln;
  rowres:= SelectRow(DB1, 'FIRST NAME', 'Bruce');
  if rowres = nil then writeln('Bruce not found.');

  writeln;
  rowres:= SelectRow(DB1, 'FIRST NAME', 'Mandy');
  if rowres = nil then writeln('Mandy not found.')
  else
  begin
    writeln('Data for Mandy row:');
    for i:= 0 to length(rowres) - 1 do
      writeln('  ', rowres[i]);
  end;
  
  writeln;
  i:= CountRows(DB1);
  writeln('Total rows in table: ', i);

  writeln;
  i:= GetLastId(DB1);
  writeln('Last insert id: ', i);

  writeln;
  rowres:= GetColumnNames(DB1);
  if rowres = nil then writeln('Column names not found.')
  else
  begin
    writeln('Column names:');
    for i:= 0 to length(rowres) - 1 do
      writeln('  ', rowres[i]);
  end;
  
  i:= NumFields(DB1);
  writeln('Number of fields: ', i);
  
  writeln;
  SdsRes:= SelectWhole(DB1);
  if sdsres = nil then writeln('SDS SelectWhole result is nil!')
  else
  begin
    writeln('The first names in the database are:');
    for i := 0 to length(sdsres) - 1 do
      writeln(SDSres[i][ciFIRSTNAME]);
    writeln;
    writeln('The surnames in the database are:');
    for i := 0 to length(sdsres) - 1 do
      writeln(SDSres[i][ciLASTNAME]);
    writeln;
    writeln('The cities in the database are:');
    for i := 0 to length(sdsres) - 1 do
      writeln(SDSres[i][ciCITY]);

  end;


  readln;
end.

