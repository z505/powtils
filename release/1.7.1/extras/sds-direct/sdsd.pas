{ SDS DIRECT

  Similar to some ideas in Simple Data Storage but no SQL scripts required

  This implementation of a text database is called "Simple Data Storage Direct"

  The focus is to have direct access to the storage system with no need for
  obfusticated and embedded SQL code in a program.

  Any programming language can access the Simple Storage Direct database,
  usually  using a SSD dll dynamically, or by using static code if a static
  implentation or object file  has been written for the language.

  Note:
    #SDS_CHAR_CR# = CARRIAGE RETURN escaped
    #SDS_CHAR_LF# = LINE FEED escaped

  Authors:
   Lars (L505),

  Original PSP 1.3 SDS Author:
   Vladimir Sibirov (Trustmaster)

}


unit sdsd;

{$i defs.inc}

interface

uses
{$ifndef fpc}
  windows,
{$endif}
//  pwsubstr,
  fileshare,
  sysutils;

type
  // Table row
  TSDSRow = array of string;
  PSDSRow = ^TSDSRow;

  // Table field
  TSDSField = string;

  // Table fields
  TSDSFields = array of TSDSField;
  PSDSFields = ^TSDSFields;

  // Table fields
  TSDSRows = array of TSDSRow;
  PSDSRows = ^TSDSRows;

  // Field types
  TSDSFieldList = array of longword;
  PSDSFieldList = ^TSDSFieldList;

  TSDSFieldValues = array of string;
  PSDSFieldValues = ^TSDSFieldValues;

  // Type for query result in memory
  TSDSResult = array of TSDSRow;
  PSDSResult = ^TSDSResult;


  // SDS table header
  TSDSHeader = record
    intro: string;
    fields,
    rows,
    last_id: longint;
  end;
  PSDSHeader = ^TSDSHeader;

  // Column data
  TSDSCol = record
    name: string; // Column name
    dtype: byte; // Data type:
    // 1 = VARCHAR, 2 = INT, 3 = REAL, 4 = DATE, 5 = TIME, 6 = DATETIME.
  end;

  // Table column data
  TSDSCols = array of TSDSCol;
  PSDSCols = ^TSDSCols;
(*
type
  TQueueInfo = record
    dbtable: text;
    dbfilename: string;
    tmpdb: text;
    dbheader: TSdsHeader;
    dbcols: TSDSCols;
    dbRowsBuf: TSDSRows;
    i: integer; // counter
    chunksize: longword; // number of rows in chunk, but 0 at end of queue
  end;
*)
type
  TQueueData = record
    cols: TSDSCols;
    tmpfile: text;
    dbfile: text;
    dbhdr: TSDSHeader;
    dbfilename: string;
  end;

// SDS data types
const
   dVARCHAR = 1;
   dINT = 2;
   dREAL = 3;
   dDATE = 4;
   dTIME = 5;
   dDATETIME = 6;

// dummy proc does nothing by default, i.e. error logging is off
procedure NilSDSErrorLog(s: string);

// user definable error procedure, logs errors anywhere developer wishes
//(TODO: implement more SDSerror messages)
var
  SDSError: procedure(s: string) = {$ifdef fpc}@{$endif}NilSDSErrorLog;


{------------------------------------------------------------------------------}
{  PUBLIC FUNCTION DECLARATIONS                                                }
{------------------------------------------------------------------------------}

// function CreateTable(FileName: string; fields: longint; cols: PSDSRow): boolean; //old
function CreateTableP(const FileName: string; cols: PSDSCols): boolean;
function CreateTable(const FileName: string; cols: TSDSCols): boolean;

function InsertRow(const FileName: string;
  fields: PSDSFields): longword; overload;
function InsertRow(const FileName: string;
  fields: TSDSFields): longword; overload;

function BeginQueuedInsert(FileName: string): TQueueData;
procedure QueueRow(var qd: TQueueData; Fields: TSDSFields);
procedure EndQueuedInsert(var qd: TQueueData);

(*
function QueueInsertRow(const QueueInfo: TQueueInfo; fields: TSDSFields;
  QueueCount: longword): longword;
*)

function MatchedRows(FromFile, where, equals: string): longint;
function SelectRow(FromFile, where, equals: string): TSDSRow;
function SelectField(field, FromFile, where, equals: string): string;
function SelectAll(FromFile, where, equals: string): TSDSResult;
function SelectWhole(FromFile: string): TSDSResult;
function CountResultRows(res: PSDSResult): longint;
function FetchRow(res: PSDSResult; id: longint): TSDSRow;

function UpdateRow(FileName, where, equals: string;
  row: PSDSRow): longint; overload;
function UpdateRow(FileName, where, equals: string;
  row: TSDSRow): longint; overload;


function UpdateFieldByValue(FileName, where, equals, setval: string): longint;
function UpdateField(FileName, where, equals, setfield, toval: string): longint;
function DeleteRow(FromFile, where, equals: string): longint; overload;
function DeleteRow(FromFile: string; RowIdx: longint): boolean; overload;

function NumFields(FromFile: string): longint;
function CountRows(FromFile: string): longint;
function GetLastID(FromFile: string): longint;
function CountFields(FromFile: string): longint;
function GetColumnNames(FromFile: string): TSDSRow;
function DeleteTable(FileName: string): boolean;

// Extra functions
function Encode(str: string): string;
function Decode(str: string): string;

// END PUBLIC FUNCTION DECLARATIONS
{------------------------------------------------------------------------------}

implementation



{ error dummy }
procedure NilSDSErrorLog(s: string);
begin
end;

{------------------------------------------------------------------------------}
{  PRIVATE TYPES                                                               }
{------------------------------------------------------------------------------}



// END OF PRIVATE TYPES
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{  PRIVATE FUNCTIONS                                                           }
{------------------------------------------------------------------------------}

// Replaces CR, LF with special boundaries
function Encode(str: string): string;
begin
  result:= str;
  if pos(#13, result) > 0 then
    result := StringReplace(result, #13, '#SDS_CHAR_CR#', [rfReplaceAll]);
  if pos(#10, result) > 0 then
    result := StringReplace(result, #10, '#SDS_CHAR_LF#', [rfReplaceAll]);
end;

// Replaces SDS-boundaries with CR and LF
function Decode(str: string): string;
begin
  result:= str;
  if pos('#SDS_CHAR_CR#', result) > 0 then
    result := StringReplace(result, '#SDS_CHAR_CR#', #13, [rfReplaceAll]);
  if pos('#SDS_CHAR_LF#', result) > 0 then
    result := StringReplace(result, '#SDS_CHAR_LF#', #10, [rfReplaceAll]);
end;

// Reads the row from the table stream into the array
function ReadRow(var fh: text; fields: longint): TSDSRow;
var
  cnt: longint;
begin
  SetLength(result, fields);
  for cnt := 0 to (fields - 1) do
  begin
    readln(fh, result[cnt]);
    result[cnt] := Decode(result[cnt]);
  end;
end;

(*
{ writes fields in row to file }
procedure WriteRow(var fh: text; row: PSDSRow);
{$J +}
const
 i: longword = 0;  // DEBUG COUNTER
{$J -}
var
  cnt: longint;  // field count
  //DEBUG
//  fh2: text;
begin
//DEBUG
//  inc(i);
//  assign(fh2, 'TESTING' + inttostr(i) + '.sds.tmp');
//  rewrite(fh2);
  for cnt := 0 to (length(row^) - 1) do
  begin                                          // DEBUG: TEXT OKAY
    writeln(fh, Encode(row^[cnt])); // replace linefeeds/carriage returns and write fields
//    sdserror('WRITEROW: ' + row^[cnt] + ' '#13#10);
//    writeln(Encode(row^[cnt]));
  end;
//  close(fh2);
end;

*)

procedure WriteRow(var fh: text; row: TSDSRow);
var
  cnt: longint;  // field count
begin
  { replace CR/LF and write each field in row }
  for cnt:= low(row) to high(row) do
  begin                                          // DEBUG: TEXT OKAY
     writeln(fh, Encode(row[cnt]));
  end;
end;

(*
{ writes several rows at once into file }
procedure WriteRows(var fh: text; rows: PSDSRows);
var
  fcnt: longint; // field count
  rcnt: longint; // row count
  bufsize: array [0..65536] of byte; // 64 KB, several rows are usually big data chunks
begin
  for rcnt := 0 to (length(rows^) - 1) do
  begin
    for fcnt := 0 to (length(rows^[rcnt]) - 1) do
    begin
      writeln(fh, Encode(rows^[rcnt][fcnt])); // replace linefeeds/carriage returns and write fields
    end;
  end;
end;
*)

{ Reads table header from open file  }
function ReadHeader(var fh: text): TSDSHeader;
begin
  readln(fh, result.intro);
  readln(fh, result.fields);
  readln(fh, result.last_id);
  readln(fh, result.rows);
  readln(fh);
end;

(*
{ Writes the table header into open file }
procedure WriteHeader(var fh: text; hdr: PSDSHeader);
begin
  writeln(fh, hdr^.intro);
  writeln(fh, hdr^.fields);
  writeln(fh, hdr^.last_id);
  writeln(fh, hdr^.rows);
  writeln(fh);
  flush(fh);
end;
*)

{ Writes the table header into open file }
procedure WriteHeader(var fh: text; hdr: TSDSHeader);
begin
  writeln(fh, hdr.intro);
  writeln(fh, hdr.fields);
  writeln(fh, hdr.last_id);
  writeln(fh, hdr.rows);
  writeln(fh);
  flush(fh);
end;


// Reads table column data
function ReadCols(var fh: text; fields: longword): TSDSCols;
var
  i: longword;
begin
  // read all fields including ID
  for i := 0 to fields - 1 do
  begin
    SetLength(result, i + 1);
    readln(fh, result[i].name);
    readln(fh, result[i].dtype);
  end;
  readln(fh);
end;
(*
// Writes table column data
procedure WriteCols(var fh: text; cols: PSDSCols);
var
  i: longword;
begin
  // write column names
  for i:= 0 to length(cols^) - 1 do
  begin
    writeln(fh, cols^[i].name);
    writeln(fh, cols^[i].dtype);
  end;
  writeln(fh);
  flush(fh);
end;
*)
// Writes table column data
procedure WriteCols(var fh: text; cols: TSDSCols);
var
  i: longword;
begin
  // write column names
  for i:= 0 to length(cols) - 1 do
  begin
    writeln(fh, cols[i].name);
    writeln(fh, cols[i].dtype);
  end;
  writeln(fh);
  flush(fh);
end;

const
 SDS_INTRO_LINE = '[-- Simple Data Storage File --]';

// Creates a new table
function CreateTableP(const FileName: string; cols: PSDSCols): boolean;
var
  fh: text;
  hdr: TSDSHeader;
  tmpcols: TSDSCols;
  tmpcolslen: longint;
  i: longint;
begin
  FileMarkWrite(FileName);
  assign(fh, FileName);
  rewrite(fh);
  hdr.intro := SDS_INTRO_LINE;
  tmpcolslen:= length(cols^) + 1;
  hdr.fields := tmpcolslen; // + 1 for ID field
  hdr.last_id := 0;
  hdr.rows := 0;
  WriteHeader(fh, hdr);
  setlength(tmpcols, tmpcolslen);
  // prepend ID column
  tmpcols[0].name:= 'id';
  tmpcols[0].dtype:= dINT;
  // now main columns
  for i:= 1 to length(cols^) do
  begin
    tmpcols[i].name:= cols^[i-1].name;
    tmpcols[i].dtype:= cols^[i-1].dtype;
  end;
  WriteCols(fh, tmpcols);
  close(fh);
  FileUnmarkWrite(FileName);
  result := true;
end;

(*
function CreateTable(const FileName: string; cols: TSDSCols): boolean; overload;
begin
  result:= CreateTableP(FileName, PSDSCols(@cols));
end;
*)

function CreateTable(const FileName: string; cols: TSDSCols): boolean;
var
  fh: text;
  hdr: TSDSHeader;
  tmpcols: TSDSCols;
  tmpcolslen: longint;
  i: longint;
begin
  FileMarkWrite(FileName);
  assign(fh, FileName);
  rewrite(fh);
  hdr.intro := SDS_INTRO_LINE;
  tmpcolslen:= length(cols) + 1;
  hdr.fields := tmpcolslen; // + 1 for ID field
  hdr.last_id := 0;
  hdr.rows := 0;
  WriteHeader(fh, hdr);
  setlength(tmpcols, tmpcolslen);
  // prepend ID column
  tmpcols[0].name:= 'id';
  tmpcols[0].dtype:= dINT;
  // now main columns
  for i:= 1 to length(cols) do
  begin
    tmpcols[i].name:= cols[i-1].name;
    tmpcols[i].dtype:= cols[i-1].dtype;
  end;
  WriteCols(fh, tmpcols);
  close(fh);
  FileUnmarkWrite(FileName);
  result := true;
end;

// Returns number of fields in the table
function NumFields(FromFile: string): longint;
var
  fh: text;
  hdr: TSDSHeader;
  fk: word;
begin
  result := 0;
  if not FileExists_plain(FromFile) then
  begin
    SDSerror('CreateTable: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FromFile, fk);
  result := hdr.fields;
end;

// Gets last ID in the table
function GetLastID(FromFile: string): longint;
var
  fh: text;
  hdr: TSDSHeader;
  fk: word;
begin
  result := 0;
  if not FileExists_plain(FromFile) then
  begin
    SDSerror('GetLastID: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FromFile, fk);
  result := hdr.last_id;
end;

// Returns total number of rows in the table
function CountRows(FromFile: string): longint;
var
  fh: text;
  hdr: TSDSHeader;
  fk: word;
begin
  result := 0;
  if not FileExists_plain(FromFile) then
  begin
    SDSerror('CountRows: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FromFile, fk);
  result := hdr.rows;
end;

// Returns total number of cells (fields) in the table
function CountFields(FromFile: string): longint;
var
  fh: text;
  hdr: TSDSHeader;
  fk: word;
begin
  result := 0;
  if not FileExists_plain(FromFile) then exit;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FromFile, fk);
  result := hdr.fields * hdr.rows;
end;

// Fetches column names in the table
function GetColumnNames(FromFile: string): TSDSRow;
var
  fh: text;
  hdr: TSDSHeader;
  fk: word;
  cols: TSDSCols;
  i: integer;
begin
  result:=  nil;
  if not FileExists_plain(FromFile) then
  begin
    SDSerror('GetColumnNames: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr:= ReadHeader(fh);
  SetLength(result, hdr.fields);
  cols:= ReadCols(fh, hdr.fields);
  // convert col names to row
  for i:= 0 to length(cols) - 1 do
    result[i]:= cols[i].name;
  close(fh);
  FileUnmarkRead(FromFile, fk);
end;

// Inserts a new row into a table, returns 1 if successful, 0 otherwise
function InsertRow(const FileName: string; Fields: PSDSFields): longword;
var
  fh, fhn: text;
  buff: string;
  newid: string; // latest ID after insertion
  hdr: TSDSHeader;
  cols: TSDSCols;
  tmprow: TSDSRow;
  i: longword;
begin
  // Initializing, locking
  result := 0;
  if not FileExists_plain(FileName) then
  begin
    sdserror('InsertRow: file does not exist: ' + FileName);
    exit;
  end;
  FileMarkWrite(FileName);
  // Copying/modifying header
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  hdr := ReadHeader(fh);
  // update last ID
  if hdr.rows <> 0 then inc(hdr.last_id);
  inc(hdr.rows);
  WriteHeader(fhn, hdr);
  // Copying column data
  cols := ReadCols(fh, hdr.fields);
  WriteCols(fhn, cols);
  // Copying table contents
  while not eof(fh) do
  begin
    readln(fh, buff);
    writeln(fhn, buff);
  end;
  // Converting fields and values into a single row
  SetLength(tmprow, hdr.fields);
  for i:= 0 to length(Fields^) - 1 do
  begin
    // time right now converted to string
    if uppercase(Fields^[i]) = 'NOW' then
    begin
      case cols[i].dtype of
          4: tmprow[i+1] := FormatDateTime('yyyy-mm-dd', now);
          5: tmprow[i+1] := FormatDateTime('hh:nn:ss', now);
          6: tmprow[i+1] := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
      else
        tmprow[i+1] := Fields^[i];
      end;
    end else
      tmprow[i+1] := Fields^[i]; // i + 1 row [0] filled in with ID below
  end;
  // update tmp row ID to reflect latest ID, which was incremented already
  newid:= inttostr(hdr.last_id);
  if tmprow[0] = '' then tmprow[0] := newid;
  // Insert new data now
  WriteRow(fhn, tmprow);
  // Final steps
  inc(result);
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  FileUnmarkWrite(FileName);
end;

function InsertRow(const FileName: string; Fields: TSDSFields): longword;
begin
  result:= InsertRow(filename, PSDSFields(fields));
end;

{ Must be called before QueueRow. }
function BeginQueuedInsert(FileName: string): TQueueData;
  { open file, settextbuf, write a false header since we don't know total
    insert count or last ID yet  }
var
  buff: string;
begin

  if not FileExists_plain(FileName) then
  begin
    sdserror('InsertRow: file does not exist: ' + FileName);
    exit;
  end;
  FileMarkWrite(FileName);

  // Copying/modifying header
  assign(result.dbfile, FileName);
  assign(result.tmpfile, FileName + '.tmp');
  reset(result.dbfile);
  rewrite(result.tmpfile);

// ERROR
//  settextbuf(result.tmpfile, chunksize);

  result.dbhdr := ReadHeader(result.dbfile);

  //ERROR
  // write false header, header must be modified after queue done! lastid and rowcount are known only then
  WriteHeader(result.tmpfile, result.dbhdr);

  // Copying column data
  result.cols := ReadCols(result.dbfile, result.dbhdr.fields);
  WriteCols(result.tmpfile, result.cols); // Copy cols to tmpfile, they stay unmodified

  { Copy existing table rows to tmpfile, they stay unmodified }
  while not eof(result.dbfile) do
  begin
    readln(result.dbfile, buff);
    writeln(result.tmpfile, buff);
  end;
  result.dbfilename:= filename;
end;

{ Queue single row for insertion, once chunksize is reached row or part of row
  will be flushed automatically through writeln settextbuf buffer, occuring in
  the WriteRow procedure }
procedure QueueRow(var qd: TQueueData; Fields: TSDSFields);
var
  tmprow: TSDSRow;
  i: integer;
begin
  // update last ID
  if qd.dbhdr.rows <> 0 then inc(qd.dbhdr.last_id);
  inc(qd.dbhdr.rows);
  // Converting fields and values into a single row
  SetLength(tmprow, qd.dbhdr.fields);
  for i:= 0 to length(Fields) - 1 do
  begin
//    sdserror('DEBUG: QL: ' + inttostr(i) + Fields[i] + ' '#13#10);
    // time right now converted to string
    if uppercase(Fields[i]) = 'NOW' then
    begin
      case qd.cols[i].dtype of
          4: tmprow[i+1] := FormatDateTime('yyyy-mm-dd', now);
          5: tmprow[i+1] := FormatDateTime('hh:nn:ss', now);
          6: tmprow[i+1] := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
      else
        tmprow[i+1] := Fields[i];
      end;
    end else
    begin
      tmprow[i+1] := Fields[i]; // i + 1 row [0] filled in with ID below
//      sdserror(tmprow[i+1] {+ '  ' + Fields[i]});
    end;
  end;
  // update tmp row ID to reflect latest ID, which was incremented already
  tmprow[0] := inttostr(qd.dbhdr.last_id);
  // Inserting new data
  WriteRow(qd.tmpfile, tmprow);
end;

procedure EndQueuedInsert(var qd: TQueueData);
begin
  { close file, open file, rewrite header with known lastid and rowcount, close
    file }

  close(qd.dbfile);
  close(qd.tmpfile);

  // new tmp db table will take place of old db file
//  erase(qd.dbfile);

(*
  // now rewrite header
  assign(qd.tmpfile, qd.dbFileName + '.tmp');  // note: maybe could be optimized with SeekBOF instead of closing and opening ?
  rewrite(qd.tmpfile);
  // write new header
  WriteHeader(qd.tmpfile, @qd.dbhdr);

  // Copying column data
  WriteCols(qd.tmpfile, @qd.cols); // Copy cols to tmpfile, they stay unmodified
*)
  // done with tmp file, save to working db filename
//  rename(qd.tmpfile, qd.dbFileName);

  FileUnmarkWrite(qd.dbFileName);


end;

(*

{ Must be called before a QueueInsertRow is used. Result (TQueueInfo) is then
  sent to QueueRowInsert procedure for each row that is inserted, and
  once more on EndQueueInsert}
function BeginQueueInsert(FileName: string; chunksize: longword): TQueueInfo;
var
  fh, fhn: text;
//  fk: word;
  cols: TSDSCols;

begin
  { Must be positive value (valid chunk), cannot be -1 (end of queue) }
  if (chunksize < 1) then
  begin
    sdserror('ERR: chunksize invalid (1)');
  end;

  result.dbfilename:= filename;
  result.chunksize:= chunksize;
  // lock table
  FileMarkWrite(result.dbfilename);
  assign(result.dbtable, FileName);
  assign(result.tmpdb, FileName + '.tmp');
  reset(result.dbtable);
  rewrite(result.tmpdb);
  // read table header
  result.QueueInfo.dbdeader := ReadHeader(result.dbtable);

end;

{ Must be called after a QueueInsertRow. Ensures flushing of any leftover rows }
procedure EndQueueInsert(var QueueInfo: TQueueInfo);
var
  dummyFields: TSDSFields;
  DummyQueueInfo: TQueueInfo;
begin
  // write table header to new tmp database
  WriteHeader(QueueInfo.tmpdb, @QueueInfo.dbheader);
  // read col data from db
  cols := ReadCols(QueueInfo.dbtable, QueueInfo.dbheader.fields);
  // copy col data to tmp database
  WriteCols(result.tmpdb, @QueueInfo.dbcols);

  QueueInfo.chunksize = 0; // signal end of queue
  QueueInsertRow(DummyQueueInfo, dummyFields);
  close(result.dbtable);
  close(result.tmpdb);
  erase(result.dbtable);
  rename(result.tmpdb, FileName);
  FileUnmarkWrite(result.dbfilename);
end;

{ optimized insert, queues several consecutive inserts before finally writing
  all of them in large chunks. ChunkSize is the number of rows per chunk }
function QueueInsertRow(var QueueInfo: TQueueInfo; fields: TSDSFields): longword;
begin

  inc(QueueInfo.i); // counter

  if QueueInfo.dbheader.rows <> 0 then
    QueueInfo.dbheader.last_id:= QueueInfo.dbheader.last_id + QueueInfo.chunksize
  else // if 0 then we must -1 for 0 based number
    QueueInfo.dbheader.last_id:= QueueInfo.chunksize - 1;
  QueueInfo.dbheader.rows:= QueueInfo.dbheader.rows + QueueInfo.chunksize;

  { Signal end of queue }
  if QueueInfo.chunksize = 0 then
  begin
    setlength(QueueInfo.dbRowsBuf, QueueInfo.i); // nulls out any extra fields if QueueCount is larger than the amount buffered before EndUpdate call
    QueueInfo.dbheader.rows:= QueueInfo.dbheader.rows - (QueueInfo.dbheader.rows - QueueInfo.i); // null out extra rows
    { flush leftover rows to database }

    QueueInfo.i:= 0; // reset counter
    exit;

  { Not end of queue }
  if QueueInfo.chunksize > 0 then
  begin
    if QueueInfo.i = 1 then // setlength on first insert queue only
      setlength(QueueInfo.dbRowsBuf, QueueInfo.chunksize);
  end;


  if QueueInfo.i = QueueInfo.chunksize then
  begin
    { flush rows to database }
    setlength(QueueInfo.dbRowsBuf, 0); // clear rows buffer
    QueueInfo.i:= 0; // reset counter
  end;
end;
*)

// Returns number of rows in the table with field 'where' matching 'equals'
function MatchedRows(FromFile, where, equals: string): longint;
var
  fh: text;
  field, cnt: longint;
  row: TSDSRow;
  hdr: TSDSHeader;
  fk: word;
  cols: TSDSCols;
begin
  result := 0;
  field:= 0;
  if not FileExists_plain(FromFile) then
  begin
    sdserror('MatchedRows: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  for cnt := 0 to (hdr.fields - 1) do
    if cols[cnt].name = where then field := cnt;
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if row[field] = equals then
      inc(result);
  end;
  close(fh);
  FileUnmarkRead(FromFile, fk);
end;

// Selects a row by field value
function SelectRow(FromFile, where, equals: string): TSDSRow;
var
  fh: text;
  field, cnt: longint;
  row: TSDSRow;
  cols: TSDSCols;
  hdr: TSDSHeader;
  fk: word;
begin
  result:= nil;
  field:= 0;
  if not FileExists_plain(FromFile) then
  begin
    sdserror('SelectRow: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(result, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  for cnt := 0 to (hdr.fields - 1) do
    if cols[cnt].name = where then field:= cnt;
  SetLength(row, hdr.fields);
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if row[field] = equals then
    begin
      result:= row;
      break;
    end;
  end;
  close(fh);
  FileUnmarkRead(FromFile, fk);
end;

// Custom field selection
function SelectField(field, FromFile, where, equals: string): string;
var
  fh: text;
  cnt, col, whr: longint;
  row: TSDSRow;
  cols: Tsdscols;
  hdr: TSDSHeader;
  fk: word;
begin
  result:= '';
  whr:= 0;
  col:= 0;
  if not FileExists_plain(FromFile) then
  begin
    sdserror('SelectField: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr:= ReadHeader(fh);
  // column data
  cols:= Readcols(fh, hdr.fields);
  for cnt:= 0 to (hdr.fields - 1) do
  begin
    if cols[cnt].name = field then
    begin
      col:= cnt;
//      writeln('debug: col: ', col);
    end;
    if cols[cnt].name = where then
    begin
      whr:= cnt;
//      writeln('debug: whr: ', whr);
    end;
  end;
  // row data
  SetLength(row, hdr.fields);
  while not eof(fh) do
  begin
    row:= ReadRow(fh, hdr.fields);
    if row[whr] = equals then
    begin
      result:= row[col];
      break;
    end;
  end;
  close(fh);
  FileUnmarkRead(FromFile, fk);
end;

// Updates whole row by its field value
function UpdateRow(FileName, where, equals: string; row: PSDSRow): longint; overload;
var
  fh, fhn: text;
  cnt, field: longint;
  tmprow: TSDSRow;
  tmpcols: TSdsCols;
  hdr: TSDSHeader;
  idrow:TSDSRow; // row with ID prepended
  i: integer;
begin
  result:= 0;
  field:= 0;
  if not FileExists_plain(FileName) then
  begin
    sdserror('UpdateRow: file does not exist');
    exit;
  end;
  FileMarkWrite(FileName);
  result := 0;
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  hdr := ReadHeader(fh);
  WriteHeader(fhn, hdr);
  SetLength(tmpcols, hdr.fields);
  // column data
  tmpcols := ReadCols(fh, hdr.fields);
  for cnt := 0 to (hdr.fields - 1) do
    if tmpcols[cnt].name = where then field := cnt;
  Writecols(fhn, tmpcols);
  // row data
  SetLength(tmprow, hdr.fields);
  SetLength(idrow, hdr.fields);

  while not eof(fh) do
  begin
    tmprow := ReadRow(fh, hdr.fields);
    if tmprow[field] = equals then
    begin
      // incoming row^ has no id, move it into a array prepeneding the ID in [0]
      for i:= 0 to length(row^)-1 do
      begin
        idrow[i+1]:= row^[i];
      end;
      idrow[0] := tmprow[0]; // prepend ID to old id from sds file
      WriteRow(fhn, idrow);
      inc(result);
    end
    else WriteRow(fhn, tmprow);
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  FileUnMarkWrite(FileName);
end;

function UpdateRow(FileName, where, equals: string; row: TSDSRow): longint; overload;
begin
  result:= UpdateRow(FileName, where, equals, PSDSRow(@row));
end;

// Updates a field by its value
function UpdateFieldByValue(FileName, where, equals, setval: string): longint;
var
  fh, fhn: text;
  cnt, field: longint;
  row: TSDSRow;
  cols: TSDSCols;
  hdr: TSDSHeader;
begin
  result:= 0;
  field:= 0;
  if not FileExists_plain(FileName) then
  begin
    sdserror('UpdateFieldByValue: file does not exist');
    exit;
  end;
  FileMarkWrite(FileName);
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  hdr := ReadHeader(fh);
  WriteHeader(fhn, hdr);
  // column data
  cols := ReadCols(fh, hdr.fields);
  for cnt := 0 to (hdr.fields - 1) do
    if cols[cnt].name = where then field := cnt;
  WriteCols(fhn, cols);
  // row data
  SetLength(row, hdr.fields);
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if row[field] = equals then
    begin
      row[field] := setval;
      inc(result);
    end;
    WriteRow(fhn, row);
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  FileUnMarkWrite(FileName);
end;

// Custom field update
function UpdateField(FileName, where, equals, setfield, ToVal: string): longint;
var
  fh, fhn: text;
  cnt, field, another: longint;
  row: TSDSRow;
  cols: TSDSCols;
  hdr: TSDSHeader;
begin
  result := 0;
  another:= 0;
  field:= 0;
  if not FileExists_plain(FileName) then
  begin
    sdserror('UpdateField: file does not exist');
    exit;
  end;
  FileMarkWrite(FileName);
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  hdr:= ReadHeader(fh);
  WriteHeader(fhn, hdr);
  cols:= ReadCols(fh, hdr.fields);
  for cnt:= 0 to (hdr.fields - 1) do
  begin
    if cols[cnt].name = where then another:= cnt;
    if cols[cnt].name = setfield then field:= cnt;
  end;
  WriteCols(fhn, Cols);
  SetLength(row, hdr.fields);
  while not eof(fh) do
  begin
    row:= ReadRow(fh, hdr.fields);
    if row[another] = equals then
    begin
      row[field]:= toval;
      inc(result);
    end;
    WriteRow(fhn, row);
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  FileUnMarkWrite(FileName);
end;

// Deletes a row by its field value
function DeleteRow(FromFile, where, equals: string): longint;
var
  fh, fhn: text;
  cnt,
  field,
  matches: longint;
  row: TSDSRow;
  hdr: TSDSHeader;
  cols: TSDSCols;
begin
  result:= 0;
  field:= 0;
  if not FileExists_plain(FromFile) then
  begin
    SdsError('DeleteRow: file does not exist.');
    exit;
  end;
  matches := MatchedRows(FromFile, where, equals);
  if matches = 0 then
  begin
    SdsError('DeleteRow: no matches found.');
    exit;
  end;

  FileMarkWrite(FromFile);
  assign(fh, FromFile);
  assign(fhn, FromFile + '.tmp');
  reset(fh);
  rewrite(fhn);
  hdr:= ReadHeader(fh);
  SetLength(row, hdr.fields);
  // matches we delete must be accounted for in header row count
  hdr.rows:= hdr.rows - matches;
  // read column data
  cols:= ReadCols(fh, hdr.fields);
  for cnt:= 0 to (hdr.fields - 1) do
    if cols[cnt].name = where then field := cnt;
  WriteHeader(fhn, hdr);
  WriteCols(fhn, cols);
  while not eof(fh) do
  begin
    row:= ReadRow(fh, hdr.fields);
    //writeln('DEBUG: DELETEROW: ROW: ', row[field]);
    if row[field] = equals then
      inc(result)
    else
      WriteRow(fhn, row);
  end;

  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FromFile);
  FileUnmarkWrite(FromFile);
end;

// deletes row by it's row number index, returns false if row not found
function DeleteRow(FromFile: string; RowIdx: longint): boolean;
var
  fh, fhn: text;
  readcnt:  longint;
  row: TSDSRow;
  hdr: TSDSHeader;
  cols: TSDSCols;
begin
  result := false;
  readcnt:= 0;
  if not FileExists_plain(FromFile) then
  begin
    sdserror('DeleteRow: file does not exist');
    exit;
  end;
  FileMarkWrite(FromFile);
  assign(fh, FromFile);
  reset(fh);
  hdr:= ReadHeader(fh);
  // can't delete non existing row
  if hdr.rows < RowIdx then
  begin
    result:= false;
    close(fh);
    FileUnmarkWrite(FromFile);
    exit;
  end;
  SetLength(row, hdr.fields);
  // deleting one row, accounted for
  hdr.rows:= hdr.rows - 1;
  assign(fhn, FromFile + '.tmp');
  rewrite(fhn);
  WriteHeader(fhn, hdr);
  // read column data
  cols:= ReadCols(fh, hdr.fields);
  WriteCols(fhn, cols);
  while not eof(fh) do
  begin
    row:= ReadRow(fh, hdr.fields);
    inc(readcnt);
    // row found or not
    if readcnt <> rowIdx then WriteRow(fhn, row) else result:= true;
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FromFile);
  FileUnmarkWrite(FromFile);
end;

{ Custom multirow select, note: result is cleared first, returns nil if nothing
  found }
function SelectAll(FromFile, where, equals: string): TSDSResult;
var
  fh: text;
  cnt, rows, field: longint;
  row: TSDSRow;
  hdr: TSDSHeader;
  fk: word;
  cols: TSdsCols;
begin
  result:= nil;
  field:= 0;
  if not FileExists_plain(FromFile) then
  begin
    sdserror('SelectAll: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  //SetLength(result, 1);
  assign(fh, FromFile);
  reset(fh);
  hdr := ReadHeader(fh);
  // read column data
  cols:= ReadCols(fh, hdr.fields);
  for cnt:= 0 to (hdr.fields - 1) do
    if cols[cnt].name = where then field := cnt;
  SetLength(row, hdr.fields);
  rows := 0;
  while not eof(fh) do
  begin
    row:= ReadRow(fh, hdr.fields);
    if row[field] = equals then
    begin
      SetLength(result, length(result) + 1);
      result[rows]:= row;
//      writeln('DEBUG: SELECTALL: ',  row[field]);
      inc(rows);
    end;
  end;
  close(fh);
  FileUnmarkRead(FromFile, fk);
end;

// Selects whole table into memory
function SelectWhole(FromFile: string): TSDSResult;
var
  fh: text;
  cnt: longint;
  row: TSDSRow;
  hdr: TSDSHeader;
  fk: word;
begin
  result:= nil;
  if not FileExists_plain(FromFile) then
  begin
    sdserror('SelectWhole: file does not exist');
    exit;
  end;
  FileMarkRead(FromFile, fk);
  assign(fh, FromFile);
  reset(fh);
  hdr:= ReadHeader(fh);
  SetLength(row, hdr.fields);
  SetLength(result, hdr.rows);
  // skip past column info
  readcols(fh, hdr.fields);
  // read field info
  for cnt:= 0 to (hdr.fields - 1) do
  begin
    row:= ReadRow(fh, hdr.fields);
    result[cnt]:= row;
  end;
  close(fh);
  FileUnmarkRead(FromFile, fk);
end;

// Returns number of rows in result
function CountResultRows(res: PSDSResult): longint;
begin
  result:= length(res^) - 1;
end;

// Returns a row from result
function FetchRow(res: PSDSResult; id: longint): TSDSRow;
begin
  if id <= CountResultRows(res) then result:= res^[id];
end;

// Deletes table
function DeleteTable(FileName: string): boolean;
var
 fh: text;
begin
  result:= false;
  if not FileExists_plain(FileName) then
  begin
    sdserror('DeleteTable: file does not exist');
    exit;
  end;
  FileMarkWrite(FileName);
  assign(fh, FileName);
  erase(fh);
  FileUnmarkWrite(FileName);
  result := true;
end;  

end.
