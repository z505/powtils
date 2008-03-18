{*******************************************************************************

                        Simple Data Storage Unit (SDS)

********************************************************************************

--------------------------------------------------------------------------------
 Main SDS Unit
--------------------------------------------------------------------------------
  A bit more comfortable than plain text way to store data based on
  electronic tables and SQL.
  USE /ReadMe/UnitNotes/ FOR DEVELOPER NOTES - DOCUMENT EACH MAJOR CHANGE!

  Authors/Credits: Trustmaster (Vladimir Sibirov), L505 (Lars)

********************************************************************************}


unit pwsds;

{$DEFINE EXTRA_SECURE}

{$I defines1.inc}

// Define it if REGEXP operator is needed (not yet supported)
//{$DEFINE ENABLE_REGEXP}

interface
uses
  pwsubstr, // Advanced string operations
  pwfileshare, // File access in shared mode
  pwfileutil,
  {$IFDEF ENABLE_REGEXP}pwregexp_tregexpr,{$ENDIF}
  pwtypes,
  sysutils; // DATETIME operations

type
  SDS_Result = Pointer;
  SDS_Array = Pointer;
  SDS_ColumnInfo = Pointer;

{------------------------------------------------------------------------------}
{      PUBLIC PROCEDURE/FUNCTION DECLARATIONS                                  }
{------------------------------------------------------------------------------}

// Public functions
// Read the documentation for the information about these functions

                         
procedure ColumnFree(cp: SDS_ColumnInfo);
function ColumnIndex(cp: SDS_ColumnInfo; const name: string): longint;
function ColumnInfo(const FileName: string): SDS_ColumnInfo;
function ColumnName(cp: SDS_ColumnInfo; index: longint): string;
function ColumnType(cp: SDS_ColumnInfo; index: longint): byte;
function Escape(const str: string): string;
function ExportCSV(const FileName: string): string;
function ExportCSV_custom(const filename, encloser, delimiter: string): string;
function ExportSDS(const FileName: string): string;
function ExportSQL(const FileName, SQLTable: string): string;
function ExportXML(const FileName, TableTag, RowTag: string): string;
function FetchColumn(ap: SDS_Array; index: longint): string;
function FetchColumn_float(ap: SDS_Array; index: longint): double;
function FetchColumn_int(ap: SDS_Array; index: longint): longint;
function FetchField(ap: SDS_Array; const name: string): string;
function FetchField_float(ap: SDS_Array; const name: string): double;
function FetchField_int(ap: SDS_Array; const name: string): longint;
function FetchRow(rp: SDS_Result): SDS_Array;
procedure FreeResult(rp: SDS_Result);
procedure FreeRow(ap: SDS_Array);
function ImportSQL(const dump: string): longint;
function LastID(const FileName: string): longint;
function NumFields(const FileName: string): longint;
function NumRows(const FileName: string): longint;
function ResultCmd(rp: SDS_Result): string;
function ResultEOF(rp: SDS_Result): boolean;
function ResultError(rp: SDS_Result): string;
function ResultFields(rp: SDS_Result): longint;
function ResultPointer(rp: SDS_Result): longint;
procedure ResultRewind(rp: SDS_Result);
function ResultRows(rp: SDS_Result): longint;
procedure ResultSeek(rp: SDS_Result; index: longint);
function ResultTime(rp: SDS_Result): double;
function TotalFields(const FileName: string): longint;
function Query(const InputQuery: string): SDS_Result;


implementation
uses pwdebugplugin;

var debugt: longint;

{$ifdef DBUG_ON}
procedure debugln(s: astr);
begin
  pwdebugplugin.debugln(debugt, s);
end;
{$endif}
{------------------------------------------------------------------------------}
{      PRIVATE TYPE DECLARATIONS                                               }
{------------------------------------------------------------------------------}

type 
    // Type of table row
    SDS_Row = array of string;
  
    // Pointer to SDS_Row
    SDS_PRow = ^SDS_Row;

    // SDS table header    
    SDS_Header = record
      intro: string;
      fields, rows, last_id: longint;
    end;

    // Pointer to SDS_Header    
    SDS_PHeader = ^SDS_Header;
                                        
    // Column data
    SDS_Col = record
      name: string; // Column name                                        
      data_type: byte; // Data type:
      // 1 = VARCHAR, 2 = INT, 3 = REAL, 4 = DATE, 5 = TIME, 6 = DATETIME.
    end;

    // Table column data
    SDS_Cols = array of SDS_Col;

    // Pointer to SDS_Cols
    SDS_PCols = ^SDS_Cols;

    // Comparsion record
    SDS_Comp = record
      field: longint; // Field number
      opr, value: string; // Operator and value for comparsion
      logic: byte; // Boolean logic:
      // AND = 1, OR = 2, XOR = 3, AND NOT = 4, OR NOT = 5, XOR NOT = 6.
    end;

    // SDS condition type
    SDS_Condition = array of SDS_Comp;

    // Pointer to SDS_Condition
    SDS_PCondition = ^SDS_Condition;

    // List of fields
    SDS_FieldList = array of longint;

    // Pointer to SDS_FieldList
    SDS_PFieldList = ^SDS_FieldList;

    // UPDATE value pair
    SDS_SetItem = record
      field: longint;
      value: string;
    end;

    SDS_Set = array of SDS_SetItem;

    // Pointer to SDS_Set
    SDS_PSet = ^SDS_Set;

    // Item of rowset order
    SDS_OrdItem = record
      key: longint; // Key field
      dir: boolean; // Direction FALSE = ascending, TRUE = descending
    end;
    
    // Row order array
    SDS_Order = array of SDS_OrdItem;

    // Pointer to SDS_Order
    SDS_POrder = ^SDS_Order;

    // Internal row representation    
    SDS_Block = record
      row: SDS_PRow; // Row data pointer
      cols: SDS_PCols; // Column data pointer
    end;

    // Pointer to SDS_Block
    SDS_PBlock = ^SDS_Block;

    // Rowset type
    SDS_RowSet = record
      rs: array of SDS_PRow; // Rowset pointers
      cs: SDS_Cols; // Colset data
      rows: longint; // Number of rows
      point: longint; // Current row pointer
      fields: longint; // Row length
      cmd: string; // SQL command type
      error: string; // Last syntax or I/O error
      time: double; // Seconds passed during query performance
    end;

    // Pointer to SDS_RowSet
    SDS_PRowSet = ^SDS_RowSet;


{------------------------------------------------------------------------------}
{      PROCEDURES/FUNCTIONS                                                    }
{------------------------------------------------------------------------------}

// Only if REGEXP is enabled (it is recommended not to used it if not needed)
{$IFDEF ENABLE_REGEXP}
{--[ RegExpCheck ]------------------------------------------------------------}
// Checks if str matches RegExp pattern

function SDSRegexp(const pattern, str: string): boolean;
var
  rh: TRegExpr;
  lim_rpos: longint;
  pat_lim: string;
begin
  // Init
  result := false;
  // Determine pattern boundaries
  pat_lim := copy(pattern, 1, 1);
  lim_rpos := substrrpos(pattern, pat_lim);
  // Building
  rh := TRegExpr.Create;
  // Split pattern and modifiers
  rh.Expression := copy(pattern, 2, lim_rpos - 2);
  rh.ModifierStr := copy(pattern, lim_rpos + 1, longint(length(pattern)) - lim_rpos);
  // Execution
  result := rh.Exec(str);
  // Finalization
  rh.Free;
end;

{------------------------------------------------------------------------------}
{$ENDIF}


// Replaces CR, LF with special boundaries
function Encode(const str: string): string;
begin
  result := str;
  if pos(#13, result) > 0 then
    result := substrreplace(result, #13, '#SDS_CHAR_CR#');
  if pos(#10, result) > 0 then
    result := substrreplace(result, #10, '#SDS_CHAR_LF#');
end;


// Replaces SDS-boundaries with CR and LF
function Decode(const str: string): string;
begin
  result := str;
  if pos('#SDS_CHAR_CR#', result) > 0 then
    result := substrreplace(result, '#SDS_CHAR_CR#', #13);
  if pos('#SDS_CHAR_LF#', result) > 0 then
    result := substrreplace(result, '#SDS_CHAR_LF#', #10);
end;


// Converts dangerous characters into SDS-macros
function Escape(const str: string): string;
begin
  result := str;
  if pos('''', result) > 0 then
    result := substrreplace(result, '''', '#SDS_CHAR_TK#');
  if pos('"', result) > 0 then
    result := substrreplace(result, '"', '#SDS_CHAR_QT#');
end;


// MySQL-style escape
function EscapeSQL(const str: string): string;
begin
  result := str;
  if pos('''', result) > 0 then
    result := substrreplace(result, '''', '\''');
  if pos('"', result) > 0 then
    result := substrreplace(result, '"', '\"');
end;


// Unescapes the string
function Unescape(const str: string): string;
begin
  result := str;
  if pos('#SDS_CHAR_TK#', result) > 0 then
    result := substrreplace(result, '#SDS_CHAR_TK#', '''');
  if pos('#SDS_CHAR_QT#', result) > 0 then
    result := substrreplace(result, '#SDS_CHAR_QT#', '"');
end;


// Compares 2 SQL DATE fields (YYYY-MM-DD)
function DateCompare(date1, date2: string): shortint;
var
  n1, n2: word;
  dummy: integer;
begin
  // Comparing years
  val(copy(date1, 1, 4), n1, dummy);
  val(copy(date2, 1, 4), n2, dummy);
  if n1 < n2 then
  begin
    result:= -1;
    exit;
  end;
  if n1 > n2 then
  begin
    result:= 1;
    exit;
  end;
  // Comparing months
  val(copy(date1, 6, 2), n1, dummy);
  val(copy(date2, 6, 2), n2, dummy);
  if n1 < n2 then
  begin
    result:= -1;
    exit;
  end;
  if n1 > n2 then
  begin
    result:= 1;
    exit;
  end;
  // Comparing days
  val(copy(date1, 9, 2), n1, dummy);
  val(copy(date2, 9, 2), n2, dummy);
  if n1 < n2 then
  begin
    result:= -1;
    exit;
  end;
  if n1 > n2 then
  begin
    result:= 1;
    exit;
  end;
  // If still equal
  result := 0;
end;


// Compares 2 SQL TIME fields (HH:MM:SS)
function TimeCompare(date1, date2: string): shortint;
var n1, n2: word;
    dummy: integer;
begin
  {$ifdef DBUG_ON}debugln('TimeCompare begin');{$endif}
  // Comparing hours
  val(copy(date1, 1, 2), n1, dummy);
  val(copy(date2, 1, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1; exit; end;
  // Comparing minutes
  val(copy(date1, 4, 2), n1, dummy);
  val(copy(date2, 4, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;

  // Comparing seconds
  val(copy(date1, 7, 2), n1, dummy);
  val(copy(date2, 7, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;
  // If still equal
  result := 0;
  {$ifdef DBUG_ON}debugln('TimeCompare end');{$endif}
end;


// Compares 2 SQL DATETIME fields (YYYY-MM-DD HH:MM:SS)
function DateTimeCompare(date1, date2: string): shortint;
var n1, n2: word;
    dummy: integer;
begin
  {$ifdef DBUG_ON}debugln('DateTimeCompare begin');{$endif}
  // Compare years
  val(copy(date1, 1, 4), n1, dummy);
  val(copy(date2, 1, 4), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;
  // Compare months
  val(copy(date1, 6, 2), n1, dummy);
  val(copy(date2, 6, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;
  // Compare days
  val(copy(date1, 9, 2), n1, dummy);
  val(copy(date2, 9, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;
  // Compare hours
  val(copy(date1, 12, 2), n1, dummy);
  val(copy(date2, 12, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;
  // Compare minutes
  val(copy(date1, 15, 2), n1, dummy);
  val(copy(date2, 15, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1;  exit; end;
  // Compare seconds
  val(copy(date1, 18, 2), n1, dummy);
  val(copy(date2, 18, 2), n2, dummy);
  if n1 < n2 then begin result:= -1; exit; end;
  if n1 > n2 then begin result:= 1; exit;
  end;
  // If still equal
  result:= 0;
  {$ifdef DBUG_ON}debugln('DateTimeCompare end');{$endif}
end;

// Returns difference between two timestamps in seconds
function StampSpan(const stamp1, stamp2: TDateTime): double;
begin
  result:= abs(stamp2 - stamp1) * 86400;
end;

// Inserts a new row by copying
procedure RowsetAdd(var res: SDS_PRowSet; row: SDS_PRow);
var i: longint;
begin
  inc(res^.rows);
  SetLength(res^.rs, res^.rows);
  new(res^.rs[res^.rows - 1]);
  SetLength(res^.rs[res^.rows - 1]^, res^.fields);
  for i:= 0 to res^.fields - 1 do res^.rs[res^.rows - 1]^[i]:= row^[i];
end;

// Sets a custom row by copying
procedure RowsetSet(var res: SDS_PRowSet; index: longint; row: SDS_PRow);
var i: longint;
begin
  if index < 0 then exit;
  for i := 0 to res^.fields - 1 do res^.rs[index]^[i]:= row^[i];
end;

// Reverses the rowset
procedure RowsetReverse(var rs: array of SDS_PRow);
var i, j: longint;
    temp: SDS_PRow;
begin
  i := 0;
  if length(rs) < 1 then exit;
  j := length(rs) - 1;
  repeat
    temp := rs[i];
    rs[i] := rs[j];
    rs[j] := temp;
    inc(i);
    dec(j);
  until i >= j;
end;

// Ascending rowset sorting by key field
procedure ShellSort(var rowset: array of SDS_PRow; key: longint; cs: SDS_PCols);
  type
    SedjInc = array of longint;

  function pow(num, deg: longint): longint;
  var
    i: longint;
  begin
    result := 1;
    for i := 1 to deg do result := result * num;
  end;
  
  // Sedjewick increment formula
  function increment(var incr: SedjInc; size: longint): longint;
  var
    s: longint;
  begin
    s := -1;
    repeat
      inc(s);
      SetLength(incr, s + 1);
      if (s mod 2) > 0 then
        incr[s] := 8*pow(2, s) - 6*pow(2, (s + 1) div 2) + 1
      else
        incr[s] := 9*pow(2, s) - 9*pow(2, s div 2) + 1;
    until 3*incr[s] >= size;
    result := s;
  end;

  var
    incr,
    i,
    j,
    s,
    int1, int2: longint;
    size: longint;
    seq: SedjInc;
    temp: SDS_PRow;
    flt1, flt2: double;
    dummy: integer;
begin
  size := length(rowset);
  if size < 1 then exit;
  s := increment(seq, size);
  while (s >= 0) do
  begin
    incr := seq[s];
    dec(s);
    for i := incr to size - 1 do
    begin
      temp := rowset[i];
      j := i - incr;
      // Need type-specific comparison
      if cs^[key].data_type = 1 then
      begin
        while (j >= 0) and (pwsubstr.strcomp(rowset[j]^[key], temp^[key]) > 0) do
        begin
            rowset[j + incr] := rowset[j];
            j := j - incr;
        end;
      end else if cs^[key].data_type = 2 then
      begin
        while (j >= 0) do
        begin
          val(rowset[j]^[key], int1, dummy);
          val(temp^[key], int2, dummy);
          if int1 <= int2 then break;
          rowset[j + incr] := rowset[j];
          j := j - incr;
        end;
      end else if cs^[key].data_type = 3 then
      begin
        while (j >= 0) do
        begin
          val(rowset[j]^[key], flt1, dummy);
          val(temp^[key], flt2, dummy);
          if flt1 <= flt2 then break;
          rowset[j + incr] := rowset[j];
          j := j - incr;
        end;
      end else if cs^[key].data_type = 4 then
      begin
        while (j >= 0) and (DateCompare(rowset[j]^[key], temp^[key]) > 0) do
        begin
          rowset[j + incr] := rowset[j];
          j := j - incr;
        end;
      end else if cs^[key].data_type = 5 then
      begin
        while (j >= 0) and (TimeCompare(rowset[j]^[key], temp^[key]) > 0) do
        begin
          rowset[j + incr] := rowset[j];
          j := j - incr;
        end;
      end else if cs^[key].data_type = 6 then
      begin
        while (j >= 0) and (DatetimeCompare(rowset[j]^[key], temp^[key]) > 0) do
        begin
          rowset[j + incr] := rowset[j];
          j := j - incr;
        end;
      end;
      rowset[j + incr] := temp;
    end;
  end;
end;

// Ascending and descending sort by several keys
procedure RowsetSort(res: SDS_PRowSet; order: SDS_POrder);
var 
  temp: array of SDS_PRow;
  i, len, p, j, lt: longint;
  flag: boolean;
begin
  len := length(order^);
  if len < 1 then exit;
  if res^.rows < 2 then exit;
  if order^[0].key = 0 then exit;
  // First time we sort by order[0].key
  ShellSort(res^.rs, order^[0].key, @res^.cs);
  if order^[0].dir then RowsetReverse(res^.rs);
  if len < 2 then exit;
  // Then we sort equal elements
  for i := 1 to len - 1 do
  begin
    p := 0;
    flag := false;
    SetLength(temp, 0);
    while p < res^.rows do
    begin
      if flag then
      begin
        SetLength(temp, length(temp) + 1);
        lt := length(temp) - 1;
        temp[lt] := res^.rs[p];
        if (((p + 1) < res^.rows) and (res^.rs[p]^[order^[i - 1].key] <> res^.rs[p + 1]^[order^[i - 1].key])) 
          or (p = (res^.rows - 1)) then
        begin
          ShellSort(temp, order^[i].key, @res^.cs);
          if order^[i].dir then RowsetReverse(temp);
          for j := 0 to lt do res^.rs[p - j] := temp[lt - j];
          flag := false;
        end;
        inc(p);
      end else
      begin
        if ((p + 1) < res^.rows) 
          and (res^.rs[p]^[order^[i - 1].key] = res^.rs[p + 1]^[order^[i - 1].key]) then
        begin
          SetLength(temp, 1);
          temp[0] := res^.rs[p];
          flag := true;
        end;
        inc(p);
      end;
    end;
  end;
end;

// Reads the row from the table stream into the array
function ReadRow(var fh: text; fields: longint): SDS_Row;
var cnt: longint;
begin
  if fields < 1 then exit;
  SetLength(result, fields);
  for cnt := 0 to (fields - 1) do
  begin
    readln(fh, result[cnt]);
    result[cnt] := Decode(result[cnt]);
  end;
end;

// Implements the array in table stream
procedure WriteRow(var fh: text; row: SDS_PRow);
var 
  cnt: longint;
begin
  if length(row^) < 1 then exit;
  for cnt := 0 to (length(row^) - 1) do
  begin
    writeln(fh, Unescape(Encode(row^[cnt])));// escape  needed in SQL DB access
  end;
end;

 
// Reads the table header from open file stream
function ReadHeader(var fh: text): SDS_Header;
begin
  {$ifdef DBUG_ON}debugln('ReadHeader begin');{$endif}
  readln(fh, result.intro);
  readln(fh, result.fields);
  readln(fh, result.last_id);
  readln(fh, result.rows);
  readln(fh);
  {$ifdef DBUG_ON}debugln('ReadHeader end');{$endif}
end;

// Writes the table header into the open file stream
procedure WriteHeader(var fh: text; hdr: SDS_PHeader);
begin
  {$ifdef DBUG_ON}debugln('WriteHeader begin');{$endif}
  writeln(fh, hdr^.intro);
  writeln(fh, hdr^.fields);
  writeln(fh, hdr^.last_id);
  writeln(fh, hdr^.rows);
  writeln(fh);
  {$ifdef DBUG_ON}debugln('WriteHeader end');{$endif}
end;

// Reads the table column data
function ReadCols(var fh: text; fields: longint): SDS_Cols;
var i: longint;
begin
  {$ifdef DBUG_ON}debugln('ReadCols begin');{$endif}
  if fields < 1 then exit;
  for i := 0 to fields - 1 do
  begin
    SetLength(result, i + 1);
    readln(fh, result[i].name);
    readln(fh, result[i].data_type);
  end;
  readln(fh);
  {$ifdef DBUG_ON}debugln('ReadCols end');{$endif}
end;

// Writes table column data
procedure WriteCols(var fh: text; cols: SDS_PCols);
var i: longint;
begin
  {$ifdef DBUG_ON}debugln('WriteCols begin');{$endif}
  if length(cols^) < 1 then exit;
  for i := 0 to length(cols^) - 1 do
  begin
    writeln(fh, cols^[i].name);
    writeln(fh, cols^[i].data_type);
  end;
  writeln(fh);
  {$ifdef DBUG_ON}debugln('WriteCols end');{$endif}
end;

// Retrives header and column data standalone
procedure ReadInfo(const FileName: string; var hdr: SDS_Header; var cols: SDS_Cols);
var fh: text;
    fk: word;
begin
  {$ifdef DBUG_ON}debugln('ReadInfo begin');{$endif}
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  cols := ReadCols(fh, hdr.fields);
  close(fh);
  FileUnmarkRead(FileName, fk);
  {$ifdef DBUG_ON}debugln('ReadInfo end');{$endif}
end;

// Checks if row matches the condition
function MatchCondition(var cond: SDS_PCondition; cols: SDS_PCols; row: SDS_PRow): boolean;
var 
  i: longint;
  sub: boolean;
  n, nc: longint;
  f, fc: double;
  t: byte; // 1 = str, 2 = int, 3 = float
  tmp: string;
  dummy: integer;
begin
  {$ifdef DBUG_ON}debugln('MatchCondition begin');{$endif}
  result := true;
  if length(cond^) = 0 then exit;

  // Checking each subcondition for matching
  for i := 0 to length(cond^) - 1 do
  begin
    sub := false;
    // Determine column type
    t := cols^[cond^[i].field].data_type;
    if t = 2 then
    begin
      val(row^[cond^[i].field], n, dummy);
      val(cond^[i].value, nc, dummy);
    end;
    if t = 3 then
    begin
      val(row^[cond^[i].field], f, dummy);
      val(cond^[i].value, fc, dummy);
    end;
    // Supporting NOW in conditions
    if uppercase(cond^[i].value) = 'NOW' then
    case t of
        4: cond^[i].value := FormatDateTime('yyyy-mm-dd', now);
        5: cond^[i].value := FormatDateTime('hh:nn:ss', now);
        6: cond^[i].value := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
    end;
    // Depends on operator type
    tmp := uppercase(cond^[i].opr);
    if cond^[i].opr = '=' then
    begin
      // Equal
      case t of
          1: if row^[cond^[i].field] = cond^[i].value then sub := true;
          2: if n = nc then sub := true;
          3: if f = fc then sub := true;
          4: if DateCompare(row^[cond^[i].field], cond^[i].value) = 0 then sub := true;
          5: if TimeCompare(row^[cond^[i].field], cond^[i].value) = 0 then sub := true;
          6: if DatetimeCompare(row^[cond^[i].field], cond^[i].value) = 0 then sub := true;
      end;
    end;
    if cond^[i].opr = '<>' then
    begin
      // Not equal
      case t of
          1: if row^[cond^[i].field] <> cond^[i].value then sub := true;
          2: if n <> nc then sub := true;
          3: if f <> fc then sub := true;
          4: if DateCompare(row^[cond^[i].field], cond^[i].value) <> 0 then sub := true;
          5: if TimeCompare(row^[cond^[i].field], cond^[i].value) <> 0 then sub := true;
          6: if DatetimeCompare(row^[cond^[i].field], cond^[i].value) <> 0 then sub := true;
      end;
    end;
    if cond^[i].opr = '>' then
    begin
      // Greater than
      case t of
          1: if pwsubstr.strcomp(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
          2: if n > nc then sub := true;
          3: if f > fc then sub := true;
          4: if DateCompare(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
          5: if TimeCompare(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
          6: if DatetimeCompare(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
      end;
    end;
    if cond^[i].opr = '<' then
    begin
      // Less than
      case t of
          1: if pwsubstr.strcomp(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
          2: if n < nc then sub := true;
          3: if f < fc then sub := true;
          4: if DateCompare(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
          5: if TimeCompare(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
          6: if DatetimeCompare(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
      end;
    end;
    if cond^[i].opr = '>=' then
    begin
      // Greater than or equal
      case t of
          1: if pwsubstr.strcomp(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
          2: if n >= nc then sub := true;
          3: if f >= fc then sub := true;
          4: if DateCompare(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
          5: if TimeCompare(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
          6: if DatetimeCompare(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
      end;
    end;
    if cond^[i].opr = '<=' then
    begin
      // Less than or equal
      case t of
          1: if pwsubstr.strcomp(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
          2: if n <= nc then sub := true;
          3: if f <= fc then sub := true;
          4: if DateCompare(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
          5: if TimeCompare(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
          6: if DatetimeCompare(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
      end;
    end;
    {$IFDEF ENABLE_REGEXP}
    if tmp = 'REGEXP' then
    begin
      // Matches regular expression
      if sds_regexp(cond^[i].value, row^[cond^[i].field]) then sub := true;
    end;
    {$ENDIF}
    if tmp = 'CASE_EQ' then
    begin
      // Equal (case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) = 0 then sub := true;
    end;
    if tmp = 'CASE_NOT_EQ' then
    begin
      // Not equal (case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) <> 0 then sub := true;
    end;
    if tmp = 'CASE_LT' then
    begin
      // Less than (case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
    end;
    if tmp = 'CASE_GT' then
    begin
      // Greater than (case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
    end;
    if tmp = 'CASE_LT_OR_EQ' then
    begin
      // Less than or equal (case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
    end;
    if tmp = 'CASE_GT_OR_EQ' then
    begin
      // Greater than or equal (case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
    end;
    if tmp = 'NAT_LT' then
    begin
      // Less than (natural algoritm)
      if strncomp(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
    end;
    if tmp = 'NAT_GT' then
    begin
      // Greater than (natural algoritm)
      if strncomp(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
    end;
    if tmp = 'NAT_LT_OR_EQ' then
    begin
      // Less than or equal (natural algoritm)
      if strncomp(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
    end;
    if tmp = 'NAT_GT_OR_EQ' then
    begin
      // Greater than or equal (natural algoritm)
      if strncomp(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
    end;
    if tmp = 'NAT_CASE_LT' then
    begin
      // Less than (natural algoritm, case insensitive)
      if strincomp(row^[cond^[i].field], cond^[i].value) < 0 then sub := true;
    end;
    if tmp = 'NAT_CASE_GT' then
    begin
      // Greater than (natural algoritm, case insensitive)
      if strincomp(row^[cond^[i].field], cond^[i].value) > 0 then sub := true;
    end;
    if tmp = 'NAT_CASE_LT_OR_EQ' then
    begin
      // Less than or equal (natural algoritm, case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) <= 0 then sub := true;
    end;
    if tmp = 'NAT_CASE_GT_OR_EQ' then
    begin
      // Greater than or equal (natural algoritm, case insensitive)
      if pwsubstr.stricomp(row^[cond^[i].field], cond^[i].value) >= 0 then sub := true;
    end;
    case cond^[i].logic of
        1: result := result and sub;
        2: result := result or sub;
        3: result := result xor sub;
        4: result := result and (not sub);
        5: result := result or (not sub);
        6: result := result xor (not sub);
    end;
  end;
  {$ifdef DBUG_ON}debugln('MatchConditin end');{$endif}
end;

// Creates a new table
function CreateTable(const FileName: string; cols: SDS_PCols): boolean;
var
  fh: text;
  hdr: SDS_Header;
begin
  {$ifdef DBUG_ON}debugln('CreateTable begin');{$endif}
  FileMarkWrite(FileName);
  assign(fh, FileName);
  rewrite(fh);
  hdr.intro := '[-- Simple Data Storage File --]';
  hdr.fields := length(cols^);
  hdr.last_id := 0;
  hdr.rows := 0;
  WriteHeader(fh, @hdr);
  WriteCols(fh, cols);
  close(fh);
  FileUnmarkWrite(FileName);
  result := true;
  {$ifdef DBUG_ON}debugln('CreateTable end');{$endif}
end;

// Drops a table
function DropTable(const FileName: string): boolean;
var fh: text;
begin
  {$ifdef DBUG_ON}debugln('DropTable begin');{$endif}
  result := false;
  if not FileExists(FileName) then
  begin
    result:= false;
    exit;
  end;
  FileMarkWrite(FileName);
  assign(fh, FileName);
  erase(fh);
  FileUnmarkWrite(FileName);
  result := true;
  {$ifdef DBUG_ON}debugln('DropTable end');{$endif}
end;

// Returns number of fields in the table
function NumFields(const FileName: string): longint;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  {$ifdef DBUG_ON}debugln('NumFields begin');{$endif}
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FileName, fk);
  result := hdr.fields;
  {$ifdef DBUG_ON}debugln('NumFields end');{$endif}
end;

// Gets last ID in the table
function LastId(const FileName: string): longint;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  {$ifdef DBUG_ON}debugln('LastId begin');{$endif}
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FileName, fk);
  result := hdr.last_id;
  {$ifdef DBUG_ON}debugln('LastId end');{$endif}
end;

// Returns total number of rows in the table
function NumRows(const FileName: string): longint;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FileName, fk);
  result := hdr.rows;
end;

// Returns total number of cells in the table
function TotalFields(const FileName: string): longint;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  close(fh);
  FileUnmarkRead(FileName, fk);
  result := hdr.fields * hdr.rows;
end;

// Reads table column info
function ColumnInfo(const FileName: string): SDS_ColumnInfo;
var 
  fh: text;
  hdr: SDS_Header;
  cols: SDS_PCols;
  fk: word;
begin
  if not FileExists(FileName) then exit;
  new(cols);
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  cols^ := ReadCols(fh, hdr.fields);
  close(fh);
  FileUnmarkRead(FileName, fk);
  result := SDS_ColumnInfo(cols);
end;

// Returns name of the column specified by index
function ColumnName(cp: SDS_ColumnInfo; index: longint): string;
var cols: SDS_PCols;
begin
  if index < 0 then exit;
  cols := SDS_PCols(cp);
  result := cols^[index].name;
end;


// Returns type of the column specified by index
//   1 = TEXT/VARCHAR
//   2 = INT/INTEGER
//   3 = REAL/DOUBLE
//   4 = DATE
//   5 = TIME
//   6 = DATETIME
function ColumnType(cp: SDS_ColumnInfo; index: longint): byte;
var cols: SDS_PCols;
begin
  if index < 0 then exit;
  cols := SDS_PCols(cp);
  result := cols^[index].data_type;
end;

// Returns index of the column specified by name
function ColumnIndex(cp: SDS_ColumnInfo; const name: string): longint;
var i: longint;
    cols: SDS_PCols;
begin
  cols := SDS_PCols(cp);
  result := 0;
  if length(cols^) > 0 then
  for i := 0 to length(cols^) - 1 do if cols^[i].name = name then
  begin
    result:= i;
    exit;
  end;
end;

// Frees memory occupied by SDS columns info
procedure ColumnFree(cp: SDS_ColumnInfo);
var 
  cols: SDS_PCols;
begin
  cols := SDS_PCols(cp);
  dispose(cols);
end;

// Inserts a new row into a table
function InsertRow(const FileName: string; fields: SDS_PFieldList; values: SDS_PRow): longint;
var          
  fh, fhn: text;

  procedure Fini;
  begin
    close(fh);
    close(fhn);
    erase(fh);
    rename(fhn, FileName);
    FileUnmarkWrite(FileName);
  end;
var
  buff: string;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  i: longint;
begin
  // Initializing, locking
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkWrite(FileName);
  // Copying/modifying header
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  hdr := ReadHeader(fh);
  if hdr.rows <> 0 then inc(hdr.last_id);
  inc(hdr.rows);
  WriteHeader(fhn, @hdr);
  // Copying column data
  cols := ReadCols(fh, hdr.fields);
  WriteCols(fhn, @cols);
  // Copying table contents
  while not eof(fh) do
  begin
    readln(fh, buff);
    writeln(fhn, buff);
  end;                                                           
  str(hdr.last_id, buff);
  // Converting fields and values into a single row
  SetLength(row, hdr.fields);
  if length(fields^) < 1 then 
  begin 
    Fini;
    exit;
  end;
  for i := 0 to length(fields^) - 1 do
  begin
    // Conversion with function support
    if uppercase(values^[i]) = 'NOW' then
    begin
      // Now function
      case cols[fields^[i]].data_type of
          4: row[fields^[i]] := FormatDateTime('yyyy-mm-dd', now);
          5: row[fields^[i]] := FormatDateTime('hh:nn:ss', now);
          6: row[fields^[i]] := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
          else row[fields^[i]] := values^[i];
      end;
    end else
      row[fields^[i]] := values^[i];
  end;
  if row[0] = '' then
    row[0] := buff;
  // Inserting new data
  WriteRow(fhn, @row);
  // Final steps
  inc(result);
  Fini;
end;                                                    

// SQL SELECT
procedure Select(const filename: string; fields: SDS_PFieldList; cond: SDS_PCondition; order: SDS_POrder; limit: SDS_PRow; var res: SDS_PRowSet);
var 
  fh: text;
  i, cnt, loffs, lcnt: longint;
  hdr: SDS_Header;
  cols, colt: SDS_Cols;
  row, tmp: SDS_Row;
  fk: word;
  use_limit: boolean;
  dummy: integer;
begin
  // Preparing LIMIT
  if length(limit^) = 1 then
  begin
    use_limit := true;
    loffs := 0;
    val(limit^[0], lcnt, dummy);
  end
  else if length(limit^) = 2 then
  begin
    use_limit := true;
    val(limit^[0], loffs, dummy);
    val(limit^[1], lcnt, dummy);
  end
  else use_limit := false;
  cnt := 0;
  // Opening
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  // Reading header and column data
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  SetLength(tmp, length(fields^));
  SetLength(colt, length(fields^));
  res^.fields := length(fields^);
  cols := ReadCols(fh, hdr.fields);
  for i := 0 to res^.fields - 1 do colt[i] := cols[fields^[i]];
  // Performing select
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then
    begin
      inc(cnt);
      if use_limit and (cnt > (loffs + lcnt)) then break;
      if use_limit and (cnt <= loffs) then continue;
      for i := 0 to res^.fields - 1 do tmp[i] := row[fields^[i]];
      RowsetAdd(res, @tmp);
    end;
  end;
  // Replacing result coldata
  res^.cs := colt;
  // Sorting
  if length(order^) > 0 then RowsetSort(res, order);
  // Done
  close(fh);
  FileUnmarkRead(FileName, fk);
end;

// SELECT * FROM
procedure SelectAll(const FileName: string; cond: SDS_PCondition; order: SDS_POrder; limit: SDS_PRow; var res: SDS_PRowSet);
var 
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  fk: word;
  cnt, loffs, lcnt: longint;
  use_limit: boolean;
  dummy: integer;
begin
  // Preparing LIMIT
  if length(limit^) = 1 then begin
    use_limit := true;
    loffs := 0;
    val(limit^[0], lcnt, dummy);
  end else
    if length(limit^) = 2 then begin
      use_limit := true;
      val(limit^[0], loffs, dummy);
      val(limit^[1], lcnt, dummy);
    end
  else
    use_limit := false;
  cnt := 0;
  // Opening
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  // Reading header and column data
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  res^.fields := hdr.fields;
  cols := ReadCols(fh, hdr.fields);
  // Performing select
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then
    begin
      inc(cnt);
      if use_limit and (cnt > (loffs + lcnt)) then break;
      if use_limit and (cnt <= loffs) then continue;
      RowsetAdd(res, @row);
    end;
  end;
  // Sorting
  if length(order^) > 0 then RowsetSort(res, order);
  // Done
  close(fh);
  FileUnmarkRead(FileName, fk);
end;

// Counts number of rows matched the SELECT
procedure SelectCount(const FileName: string; cond: SDS_PCondition; limit: SDS_PRow; var res: SDS_PRowSet);
var
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  fk: word;
  cnt, loffs, lcnt: longint;
  use_limit: boolean;
  dummy: integer;
begin
  // Preparing LIMIT
  if length(limit^) = 1 then
  begin
    use_limit := true;
    loffs := 0;
    val(limit^[0], lcnt, dummy);
  end else
    if length(limit^) = 2 then
    begin
      use_limit := true;
      val(limit^[0], loffs, dummy);
      val(limit^[1], lcnt, dummy);
    end else
      use_limit := false;
  cnt := 0;
  // Opening
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  // Reading header and column data
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  res^.fields := hdr.fields;
  cols := ReadCols(fh, hdr.fields);
  // Searching
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then
    begin
      inc(cnt);
      if use_limit and (cnt > (loffs + lcnt)) then break;
      if use_limit and (cnt <= loffs) then continue;
      inc(res^.rows);
    end;
  end;
  // Done
  close(fh);
  FileUnmarkRead(FileName, fk);
end;

// Selects a row with maximal value of the key field
procedure SelectMax(const FileName: string; fields: SDS_PFieldList; cond: SDS_PCondition; limit: SDS_PRow; var res: SDS_PRowSet);
var
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  st: string;
  nt, nr: longint;
  ft, fr: double;
  first: boolean;
  fk: word;
  cnt, loffs, lcnt: longint;
  use_limit: boolean;
  dummy:  integer;
begin
  // Preparing LIMIT
  if length(limit^) = 1 then
  begin
    use_limit := true;
    loffs := 0;
    val(limit^[0], lcnt, dummy);
  end else
    if length(limit^) = 2 then
    begin
      use_limit := true;
      val(limit^[0], loffs, dummy);
      val(limit^[1], lcnt, dummy);
    end else
      use_limit := false;
  cnt := 0;
  // Opening
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  // Reading header and column data
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  res^.fields := hdr.fields;
  cols := ReadCols(fh, hdr.fields);
  // Performing select MAX()
  first := false;
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then
    begin
      inc(cnt);
      if use_limit and (cnt > (loffs + lcnt)) then break;
      if use_limit and (cnt <= loffs) then continue;
      if not first then
      begin
        first := true;
        case cols[fields^[0]].data_type of
            1: st := row[fields^[0]];
            2: val(row[fields^[0]], nt, dummy);
            3: val(row[fields^[0]], ft, dummy);
            4: st := row[fields^[0]];
            5: st := row[fields^[0]];
            6: st := row[fields^[0]];
        end;
        RowsetAdd(res, @row);
      end else
      begin
        case cols[fields^[0]].data_type of
            1: if pwsubstr.strcomp(row[fields^[0]], st) <= 0 then 
                 continue else st := row[fields^[0]];
            2: begin
                val(row[fields^[0]], nr, dummy);
                if nr <= nt then continue else nt := nr;
            end;
            3: begin
                val(row[fields^[0]], fr, dummy);
                if fr <= ft then continue else ft := fr;
            end;
            4: if DateCompare(row[fields^[0]], st) <= 0 then continue else st := row[fields^[0]];
            5: if TimeCompare(row[fields^[0]], st) <= 0 then 
                 continue else st := row[fields^[0]];
            6: if DatetimeCompare(row[fields^[0]], st) <= 0 then 
                 continue else st := row[fields^[0]];
        end;
        RowsetSet(res, 0, @row);
      end;
    end;
  end;
  // Done
  close(fh);
  FileUnmarkRead(FileName, fk);
end;

// Selects a row with minimal value of the key field
procedure SelectMin(const FileName: string; fields: SDS_PFieldList; cond: SDS_PCondition; limit: SDS_PRow; var res: SDS_PRowSet);
var
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  st: string;
  nt, nr: longint;
  ft, fr: double;
  first: boolean;
  fk: word;
  cnt, loffs, lcnt: longint;
  use_limit: boolean;
  dummy: integer;
begin
  // Preparing LIMIT
  if length(limit^) = 1 then
  begin
    use_limit := true;
    loffs := 0;
    val(limit^[0], lcnt, dummy);
  end
  else if length(limit^) = 2 then
  begin
    use_limit := true;
    val(limit^[0], loffs, dummy);
    val(limit^[1], lcnt, dummy);
  end
  else use_limit := false;
  cnt := 0;
  // Opening
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  // Reading header and column data
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  res^.fields := hdr.fields;
  cols := ReadCols(fh, hdr.fields);
  // Performing select MIN()
  first := false;
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then
    begin
      inc(cnt);
      if use_limit and (cnt > (loffs + lcnt)) then break;
      if use_limit and (cnt <= loffs) then continue;
      if not first then
      begin
        first := true;
        case cols[fields^[0]].data_type of
            1: st := row[fields^[0]];
            2: val(row[fields^[0]], nt, dummy);
            3: val(row[fields^[0]], ft, dummy);
            4: st := row[fields^[0]];
            5: st := row[fields^[0]];
            6: st := row[fields^[0]];
        end;
        RowsetAdd(res, @row);
      end else
      begin
        case cols[fields^[0]].data_type of
            1: if pwsubstr.strcomp(row[fields^[0]], st) >= 0 then 
                 continue else st := row[fields^[0]];
            2: begin
                val(row[fields^[0]], nr, dummy);
                if nr >= nt then continue else nt := nr;
               end;
            3: begin
                val(row[fields^[0]], fr, dummy);
                if fr >= ft then continue else ft := fr;
               end;
            4: if DateCompare(row[fields^[0]], st) >= 0 then 
                 continue else st := row[fields^[0]];
            5: if TimeCompare(row[fields^[0]], st) >= 0 then 
                 continue else st := row[fields^[0]];
            6: if DatetimeCompare(row[fields^[0]], st) >= 0 then 
                 continue else st := row[fields^[0]];
        end;
        RowsetSet(res, 0, @row);
      end;
    end;
  end;
  // Done
  close(fh);
  FileUnmarkRead(FileName, fk);
end;

// SQL update
function Update(const FileName: string; updset: SDS_PSet; cond: SDS_PCondition): longint;
var fh, fhn: text;
    i: longint;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
begin
  // Initializing
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkWrite(FileName);
  // Opening
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  // Copying header and column data
  hdr := ReadHeader(fh);
  WriteHeader(fhn, @hdr);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  WriteCols(fhn, @cols);
  // Row-by-row checking and updating
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then
    begin
      // Changing row data
      for i := 0 to length(updset^) - 1 do
      begin
        // AUTO_INCREMENT PRIMARY KEY can not be modified
        if updset^[i].field <> 0 then
        begin
          // Conversion with function support
          if uppercase(updset^[i].value) = 'NOW' then
          begin
            // NOW function
            case cols[updset^[i].field].data_type of
                4: row[updset^[i].field] := FormatDateTime('yyyy-mm-dd', now);
                5: row[updset^[i].field] := FormatDateTime('hh:nn:ss', now);
                6: row[updset^[i].field] := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                else row[updset^[i].field] := updset^[i].value;
            end;
          end
          else row[updset^[i].field] := updset^[i].value;
        end;
      end;
      inc(result);
    end;
    WriteRow(fhn, @row);
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  FileUnmarkWrite(FileName);
end;

// Deletes a row by its field value
function DeleteRow(const FileName: string; cond: SDS_PCondition): longint;
var
  fh, fhn: text;
  row: SDS_Row;
  hdr: SDS_Header;
  cols: SDS_Cols;
  buff: string;
begin
  // Initialization, checks, locking
  result := 0;
  if not FileExists(FileName) then exit;
  FileMarkWrite(FileName);
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');
  reset(fh);
  rewrite(fhn);
  // Header data
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  // We'll have to decrease hdr.rows by result after the DELETE itself is performed
  WriteHeader(fhn, @hdr);
  // Column data
  cols := ReadCols(fh, hdr.fields);
  WriteCols(fhn, @cols);
  while not eof(fh) do
  begin
    row := ReadRow(fh, hdr.fields);
    if MatchCondition(cond, @cols, @row) then inc(result)
    else WriteRow(fhn, @row);
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  // Modyfing just the header
  assign(fh, FileName);
  assign(fhn, FileName + '.tmp');                         
  reset(fh);
  rewrite(fhn);
  hdr := ReadHeader(fh);
  hdr.rows := hdr.rows - result;
  WriteHeader(fhn, @hdr);
  cols := ReadCols(fh, hdr.fields);
  WriteCols(fhn, @cols);
  // Copying the rest of the file
  while not eof(fh) do
  begin
    readln(fh, buff);
    writeln(fhn, buff);
  end;
  close(fh);
  close(fhn);
  erase(fh);
  rename(fhn, FileName);
  // Done
  FileUnmarkWrite(FileName);
end;

// Exports Comma Separated Values from SDS table
function ExportCsv(const FileName: string): string;
var
  fh: text;
  i, j: longint;
  row: SDS_Row;
  hdr: SDS_Header;
  cols: SDS_Cols;
  fk: word;
begin
  {$ifdef DBUG_ON}debugln('ExportCsv begin');{$endif}
  result:= '';
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  for i := 0 to hdr.rows - 1 do
  begin
    row := ReadRow(fh, hdr.fields);
    for j := 0 to (hdr.fields - 1) do
    begin
      if pos('"', row[j]) > 0 then row[j] := substrreplace(row[j], '"', '""');
      if (cols[j].data_type = 1) or (cols[j].data_type = 4) 
         or (cols[j].data_type = 5) or (cols[j].data_type = 6) 
      then begin
        if j = 0 then 
          result := result + '"' + row[j] + '"'
        else result := result + ',"' + row[j] + '"';
      end else
      begin
        if j = 0 then result := result + row[j]
        else result := result + ',' + row[j];
      end;
    end;
    result := result + #13 + #10;
  end;
  close(fh);
  FileUnmarkRead(FileName, fk);
  {$ifdef DBUG_ON}debugln('ExportCsv end');{$endif}
end;


// Exports Comma Separated Values from SDS table with custom delimiter/enclosing
function ExportCSV_custom(const filename, encloser, delimiter: string): string;
var
  fh: text;
  i, j: longint;
  row: SDS_Row;
  hdr: SDS_Header;
  cols: SDS_Cols;
  fk: word;
begin
  {$ifdef DBUG_ON}debugln('ExportCsv_custom begin');{$endif}
  result:= '';
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  for i := 0 to hdr.rows - 1 do
  begin
    row := ReadRow(fh, hdr.fields);
    for j := 0 to (hdr.fields - 1) do
    begin
      if pos(encloser, row[j]) > 0 then 
        row[j] := substrreplace(row[j], encloser, encloser+encloser);
      if (cols[j].data_type = 1) or (cols[j].data_type = 4) 
         or (cols[j].data_type = 5) or (cols[j].data_type = 6) 
      then begin
        if j = 0 then 
         result := result + encloser + row[j] + encloser
        else result := result + delimiter + encloser + row[j] + encloser;
      end else
      begin
        if j = 0 then result := result + row[j]
        else result := result + delimiter + row[j];
      end;
    end;
    result := result + #13 + #10;
  end;
  close(fh);
  FileUnmarkRead(FileName, fk);
  {$ifdef DBUG_ON}debugln('ExportCsvCustom end');{$endif}
end;

// Exports SDS table `from` to SDS/SQL table dump script.
function ExportSDS(const FileName: string): string;
var
  fh: text;
  i, j: longint;
  row: SDS_Row;
  hdr: SDS_Header;
  cols: SDS_Cols;
  buff: string;
  fk: word;
begin
  {$ifdef DBUG_ON}debugln('ExportSds begin');{$endif}
  result:= '';
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  result := '/**' + #13 + #10 + '* SDS Table SQL Dump for file:' + #13 + #10 + '* ' + FileName + #13 + #10 + '*/' + #13 + #10;
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  // First CREATE TABLE query
  result := result + 'CREATE TABLE `' + FileName + '` (`' + cols[0].name + '` INT';
  for j := 1 to (hdr.fields - 1) do
  begin
    case cols[j].data_type of
        1: buff := 'TEXT';
        2: buff := 'INT';
        3: buff := 'DOUBLE';
        4: buff := 'DATE';
        5: buff := 'TIME';
        6: buff := 'DATETIME';
    end;
    result := result + ', `' + cols[j].name + '` ' + buff;
  end;
  result := result + ');' + #13 + #10;
  // Then INSERT ones
  for i := 0 to (hdr.rows - 1) do
  begin
    result := result + 'INSERT INTO `' + FileName + '` (';
    for j := 0 to (hdr.fields - 1) do
    begin
      if j = 0 then result := result + '`' + cols[j].name + '`'
      else result := result + ', `' + cols[j].name + '`';
    end;
    result := result + ') VALUES (';
    row := ReadRow(fh, hdr.fields);
    result := result + row[0];
    // Do RegExp here again to determine whether to use quotes or not ->
    for j := 1 to (hdr.fields - 1) do
    begin
      if (cols[j].data_type = 1) or (cols[j].data_type = 4) or
        (cols[j].data_type = 5) or (cols[j].data_type = 6) then
          result := result + ', ''' + Escape(row[j]) + ''''
      else result := result + ', ' + row[j];
    end;
    result := result + ');' + #13 + #10;
  end;
  close(fh);
  FileUnmarkRead(FileName, fk);
  {$ifdef DBUG_ON}debugln('ExportSds end');{$endif}
end;

// Exports SDS table `from` to SQL table dump script for table `SQLTable`.
function ExportSQL(const filename, SQLTable: string): string;
var fh: text;
    i, j: longint;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
    buff: string;
    fk: word;
begin
  {$ifdef DBUG_ON}debugln('ExportSql begin');{$endif}
  result:='';
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  result := '/**' + #13 + #10 + '* SDS Table SQL Dump for file:' + #13 + #10 + '* ' + FileName + #13 + #10 + '*/' + #13 + #10;
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  // First CREATE TABLE query
  result := result + 'CREATE TABLE `' + SQLTable + '` (`' + cols[0].name + '` INT UNSIGNED PRIMARY KEY AUTO_INCREMENT';
  for j := 1 to (hdr.fields - 1) do
  begin
    case cols[j].data_type of
      1: buff := 'TEXT';
      2: buff := 'INT';
      3: buff := 'DOUBLE';
      4: buff := 'DATE';
      5: buff := 'TIME';
      6: buff := 'DATETIME';
    end;
    result := result + ', `' + cols[j].name + '` ' + buff;
  end;
  result := result + ');' + #13 + #10;
  // Then INSERT ones
  for i := 0 to (hdr.rows - 1) do
  begin
    result := result + 'INSERT INTO `' + SQLTable + '` (';
    for j := 0 to (hdr.fields - 1) do
    begin
      if j = 0 then result := result + '`' + cols[j].name + '`'
      else result := result + ', `' + cols[j].name + '`';
    end;
    result := result + ') VALUES (';
    row := ReadRow(fh, hdr.fields);
    result := result + row[0];
    // Do RegExp here again to determine whether to use quotes or not ->
    for j := 1 to (hdr.fields - 1) do
    begin
      if (cols[j].data_type = 1) or (cols[j].data_type = 4) or
        (cols[j].data_type = 5) or (cols[j].data_type = 6) 
      then
        result := result + ', ''' + EscapeSQL(row[j]) + ''''
      else result := result + ', ' + row[j];
    end;
      result := result + ');' + #13 + #10;
  end;
  close(fh);
  FileUnmarkRead(FileName, fk);
  {$ifdef DBUG_ON}debugln('ExportSql end');{$endif}
end;

// Exports SDS table `from` to XML data. Root element tag can be set with
// `TableTag`, each row tag can be set with `RowTag`.
function ExportXML(const FileName, TableTag, RowTag: string): string;
var
  fh: text;
  i, j: longint;
  row: SDS_Row;
  hdr: SDS_Header;
  cols: SDS_Cols;
  fk: word;
begin
  result:= '';
  if not FileExists(FileName) then exit;
  FileMarkRead(FileName, fk);
  result := '<' + TableTag + '>' + #13 + #10;
  assign(fh, FileName);
  reset(fh);
  hdr := ReadHeader(fh);
  SetLength(row, hdr.fields);
  cols := ReadCols(fh, hdr.fields);
  for i := 0 to (hdr.rows - 1) do
  begin
    result := result + '<' + RowTag + '>' + #13 + #10;
    row := ReadRow(fh, hdr.fields);
    for j := 0 to (hdr.fields - 1) do
    begin
      if pos('<', row[j]) > 0 then row[j] := substrreplace(row[j], '<', '&lt;');
      if pos('>', row[j]) > 0 then row[j] := substrreplace(row[j], '>', '&gt;');
      result := result + '<' + cols[j].name + '>' + row[j] + '</' + cols[j].name + '>' + #13 + #10;
    end;
    result := result + '</' + RowTag + '>' + #13 + #10;
  end;
  close(fh);
  FileUnmarkRead(FileName, fk);
  result := result + '</' + TableTag + '>';
end;


// Performs SDS/SQL query
function Query(const InputQuery: string): SDS_Result;
var
  i, j, len: longint;
  lex, buff: string;
  quot: char;
  flag: boolean;
  bflag: byte;
  hdr: SDS_Header;
  cols: SDS_Cols;
  fields: SDS_FieldList;
  values: SDS_Row;
  updset: SDS_Set;
  cond: SDS_Condition;
  resp: SDS_PRowSet;
  order: SDS_Order;
  stamp: TDateTime;


    // Table name precompiler.
    function sds_compile_name: string;
    begin
      // Reading table name
      lex := '';
      if (InputQuery[i] = '''') or (InputQuery[i] = '"') or (InputQuery[i] = '`') then
      begin
        // Quoted filename
        quot := InputQuery[i];
        inc(i);
        while (i <= len) and (InputQuery[i] <> quot) do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)] := InputQuery[i];
          inc(i);
        end;
        inc(i);
      end
      else
      begin
        // Reading till whitespace or brace
        while (i <= len) and (InputQuery[i] <> ' ') and (InputQuery[i] <> '(') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)] := InputQuery[i];
          inc(i);
        end;
      end;
      // Making the result contain table name
      result := lex;
    end;


    // Create fieldset precompiler. Example
    // 'id INT, cash FLOAT, `posts` INT, name VARCHAR' =>
    // [0].name = 'id', [0].data_type = 2; [1].name = 'cash', [1].data_type = 3,
    // [2].name = 'posts', [2].data_type = 3, [3].name = 'name', [3].data_type = 1.
    function sds_compile_types: SDS_Cols;
    begin
      SetLength(result, 0);
      // Parsing
      while (i < len) and (InputQuery[i] <> ')') do
      begin
        // New result element
        SetLength(result, length(result) + 1);
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        if InputQuery[i] = '`' then
        begin
          // Quoted field name. Getting string inside of backticks
          inc(i);
          lex := '';
          while (i <= len) and (InputQuery[i] <> '`') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space or colon
          lex := '';
          while (i <= len) and (InputQuery[i] <> ' ') and (InputQuery[i] <> ',') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
        end;
        result[length(result) - 1].name := lex;
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Getting field data type until space or colon or brace
        lex := '';
        while (i <= len) and (InputQuery[i] <> ' ')
                 and (InputQuery[i] <> ',') and (InputQuery[i] <> ')') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)] := InputQuery[i];
          inc(i);
        end;
        // Converting to upper case
        lex := uppercase(lex);
        if lex = 'DATETIME' then result[length(result) - 1].data_type := 6;
        if lex = 'TIME' then result[length(result) - 1].data_type := 5;
        if lex = 'DATE' then result[length(result) - 1].data_type := 4;
        if (lex = 'REAL') or (lex = 'DOUBLE') then
          result[length(result) - 1].data_type := 3;
        if (lex = 'INT') or (lex = 'INTEGER') then
          result[length(result) - 1].data_type := 2;
        if (lex = 'VARCHAR') or (lex = 'TEXT') then
          result[length(result) - 1].data_type := 1;
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Skipping everything till the colon or brace as we don't support anything else
        while (i <= len) and (InputQuery[i] <> ',') and (InputQuery[i] <> ')') do inc(i);
        // Skipping colon
        if (i <= len) and (InputQuery[i] = ',') then inc(i);
      end;
    end;


   {L505: DEBUG NOTE, errors when there are two text fields parsed, i.e.:

            Query('INSERT INTO `sds/topics.sds` (desc, aid, name, posts) VALUES (testdesc, 2, testname, 0)');
          
          Causes an error when it shouldn't:

            SQL syntax error: ) expected after field list

    }
    // Compiles field list into array of indexes. Example:
    // 'id, posts, login' => [0] = 0, [1] = 4, [2] = 1. 
    function sds_compile_fields(cols: SDS_PCols): SDS_FieldList;
    var j: longint;
    begin
      SetLength(result, 0);
      // Parsing while
      while (i < len) and (InputQuery[i] <> ')') do
      begin
        // Ingoring spaces                  
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Checking for FROM in SELECT
        if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'FROM') then
          break;
        // New result element
        SetLength(result, length(result) + 1);
        // Checking for ASC and DESC after ORDER BY in SELECT
        if (((i + 3) < len) and (uppercase(copy(InputQuery, i, 3)) = 'ASC'))
            or (((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'DESC')) then
          break;
          
        if InputQuery[i] = '`' then
        begin
          // Getting string inside of backticks
          inc(i);
          lex := '';
          while (i <= len) and (InputQuery[i] <> '`') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
          inc(i);
        end else
        begin
          // Getting string till space or comma or brace
          lex := '';
          while (i <= len) and (InputQuery[i] <> ' ') and (InputQuery[i] <> ',')
            and (InputQuery[i] <> ')') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
        end;
        // Ignoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Skipping comma
        if (i <= len) and (InputQuery[i] = ',') then inc(i);
        // Ignoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        for j := 0 to length(cols^) - 1 do 
          if cols^[j].name = lex then
            result[length(result) - 1] := j;
      end;
    end;

    // Compiles value list into row. Example:
    // '10, 5.1, "a string"' => [0] = '10', [1] = '5.1', [2] = 'a string'.
    function sds_compile_values: SDS_Row;
    begin
      SetLength(result, 0);
      while (i <= len) and (InputQuery[i] <> ')') do
      begin
        // New array element
        SetLength(result, length(result) + 1);
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        if (InputQuery[i] = '"') or (InputQuery[i] = '''') then
        begin
          // Getting string inside of quotes
          quot := InputQuery[i];
          inc(i);
          lex := '';
          while (i <= len) and (InputQuery[i] <> quot) do
          begin
            if (InputQuery[i] = '\') and (InputQuery[i + 1] = quot) then
            begin
              SetLength(lex, length(lex) + 1);
              lex[length(lex)] := quot;
              i := i + 2;
              continue;
            end;
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // Getting column till space or colon
          lex := '';
          while (i <= len) and (InputQuery[i] <> ' ')
                  and (InputQuery[i] <> ',') and (InputQuery[i] <> ')') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
        end;
        // Ignoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Skipping colon
        if (i <= len) and (InputQuery[i] = ',') then inc(i);
        // Ignoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        result[length(result) - 1] := lex;
      end;
    end;


    // Compiles string SDS/SQL condition into internal SDS representation
    // Example condition: '`id` > 10 AND `login` = "John" OR `posts` <= 5'
    function sds_compile_condition(cols: SDS_PCols): SDS_Condition;
    var
      idx, j: longint;
    begin
      idx := 0;
      while i < len do
      begin
        lex := '';
        // Spaces are ignored
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Checking for ORDER BY in SELECT
        if ((i + 8) < len) and (uppercase(copy(InputQuery, i, 8)) = 'ORDER BY') then break;
        // Initializing new element
        SetLength(result, idx + 1);
        result[idx].field := 0;
        result[idx].opr := '=';
        result[idx].logic := 1;
        // Checking for AND
        if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'AND ') then
        begin
          result[idx].logic := 1;
          i := i + 4;
        end;
        // Checking for OR
        if ((i + 3) < len) and (uppercase(copy(InputQuery, i, 3)) = 'OR ') then
        begin
          result[idx].logic := 2;
          i:= i + 3;
        end;
        // Checking for XOR
        if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'XOR ') then
        begin
          result[idx].logic:= 3;
          i:= i + 4;
        end;
        // Ignoring other spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Checking for NOT
        if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'NOT ') then
        begin
          result[idx].logic := result[idx].logic + 3;
          i:= i + 4;
          // Ignoring other spaces
          while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        end;
        // Getting first lexem
        if InputQuery[i] = '`' then
        begin
          // Quoted field name, get it!
          inc(i);
          while (i <= len) and (InputQuery[i] <> '`') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)]:= InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // Getting whole lexem till ' ' or EOLN
          while (i <= len) and (InputQuery[i] <> ' ') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)]:= InputQuery[i];
            inc(i);
          end;
        end;
        // Determine field index
        for j:= 0 to length(cols^) - 1 do
        if cols^[j].name = lex then
        begin
          result[idx].field:= j;
          break;
        end;
        // Ignoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Getting the operator
        lex := '';
        while (i <= len) and (InputQuery[i] <> ' ') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)]:= InputQuery[i];
          inc(i);
        end;
        result[idx].opr:= lex;
        // Ignoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Getting the value
        lex := '';
        if (InputQuery[i] = '"') or (InputQuery[i] = '''') then
        begin
          quot:= InputQuery[i];
          inc(i);
          // Quoted string value
          while (i <= len) and (InputQuery[i] <> quot) do
          begin
            if (InputQuery[i] = '\') and (InputQuery[i + 1] = quot) then
            begin
              SetLength(lex, length(lex) + 1);
              lex[length(lex)]:= quot;
              i := i + 2;
              continue;
            end;
            SetLength(lex, length(lex) + 1);
            lex[length(lex)]:= InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // Unquoted value
          while (i <= len) and (InputQuery[i] <> ' ')  do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)]:= InputQuery[i];
            inc(i);
          end;
        end;
        result[idx].value:= lex;
        // Ingnoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Increasing element index
        inc(idx);
      end;
    end;

    // UPDATE SET compiler. Example:
    // 'id = 15, `name` = "John"' => [0].field = 0, [0].value = '15'; [1].field = 3,
    // [1].value = 'John'.
    function sds_compile_set(cols: SDS_PCols): SDS_Set;
    var j: longint;
    begin
      SetLength(result, 0);
      // Parsing
      while i < len do
      begin
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Checking for WHERE (limit)
        if ((i + 5) < len) and (uppercase(copy(InputQuery, i, 5)) = 'WHERE') then break;
        // New result element
        SetLength(result, length(result) + 1);
        if InputQuery[i] = '`' then
        begin
          // Quoted field name. Getting string inside of backticks
          inc(i);
          lex := '';
          while (i <= len) and (InputQuery[i] <> '`') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space.
          lex := '';
          while (i <= len) and (InputQuery[i] <> ' ') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
        end;
        for j := 0 to length(cols^) - 1 do if cols^[j].name = lex then
        begin
          result[length(result) - 1].field := j;
          break;
        end;
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Then '=' should follow, skipping it or throw an error (TODO)
        if InputQuery[i] <> '=' then resp^.error := 'SQL syntax error: = expected';
        inc(i);
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Getting value depending on field type
        if (InputQuery[i] = '''') or (InputQuery[i] = '`') or (InputQuery[i] = '"') then
        begin
          // VARCHAR, DATE, TIME, DATETIME value (quoted)
          // Getting string inside of quotes
          quot := InputQuery[i];
          inc(i);
          lex := '';
          while (i <= len) and (InputQuery[i] <> quot) do
          begin
            if (InputQuery[i] = '\') and (InputQuery[i + 1] = quot) then
            begin
              SetLength(lex, length(lex) + 1);
              lex[length(lex)]:= quot;
              i := i + 2;
              continue;
            end;
            SetLength(lex, length(lex) + 1);
            lex[length(lex)]:= InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // INT/FLOAT value (plain)
          // Getting column name till space or colon
          lex := '';
          while (i <= len) and (InputQuery[i] <> ' ') and (InputQuery[i] <> ',') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
        end;
        result[length(result) - 1].value := lex;
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Skipping colon
        if (i <= len) and (InputQuery[i] = ',') then inc(i);
      end;
    end;

    // Compiles row order data. Example:
    // 'posts DESC, name ASC' => [0].key = 4, [0].dir = true; [1].key = 1, [1].dir = false.
    function sds_compile_order(cols: SDS_PCols): SDS_Order;
    var j: longint;
    begin
      SetLength(result, 0);
      // Parsing
      while i < len do
      begin
        // Skipping whitespace
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Checking for LIMIT
        if ((i + 5) < len) and (uppercase(copy(InputQuery, i, 5)) = 'LIMIT') then break;
        // New result element
        SetLength(result, length(result) + 1);
        // Getting field name
        if InputQuery[i] = '`' then
        begin
          // Quoted field name. Getting string inside of backticks
          inc(i);
          lex:= '';
          while (i <= len) and (InputQuery[i] <> '`') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)] := InputQuery[i];
            inc(i);
          end;
          inc(i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space.
          lex:= '';
          while (i <= len) and (InputQuery[i] <> ' ') do
          begin
            SetLength(lex, length(lex) + 1);
            lex[length(lex)]:= InputQuery[i];
            inc(i);
          end;
        end;
        // Detecting key value
        for j := 0 to length(cols^) - 1 do if cols^[j].name = lex then
        begin
          result[length(result) - 1].key:= j;
          break;
        end;
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Default order
        result[length(result) - 1].dir:= false;
        // Is there any ASC or DESC?
        if ((i + 3) < len) and (uppercase(copy(InputQuery, i, 3)) = 'ASC') then
        begin
          result[length(result) - 1].dir:= false;
          i := i + 3;
        end
        else if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'DESC') then
        begin
          result[length(result) - 1].dir:= true;
          i := i + 4;
        end;
        // Ingoring spaces
        while (i <= len) and (InputQuery[i] = ' ') do inc(i);
        // Skipping colon
        if (i <= len) and (InputQuery[i] = ',') then inc(i);
      end;
    end;

begin
  {$ifdef DBUG_ON}debugln('Query begin');{$endif}
  // Turning the timer on
  stamp := Now;
  // Result init
  new(resp);
  resp^.rows:= 0;
  resp^.fields:= 0;
  resp^.point:= 0;
  resp^.time:= 0.0;
  // Determine SQL query type
  i := 1;
  len := length(InputQuery);
  lex := '';
  // Reading string till ' '
  while (i < len) and (InputQuery[i] <> ' ') do
  begin
    SetLength(lex, length(lex) + 1);
    lex[length(lex)] := InputQuery[i];
    inc(i);
  end;
  resp^.cmd := uppercase(lex);
  // Skipping whitespace
  while (i < len) and (InputQuery[i] = ' ') do inc(i);
  if resp^.cmd = 'CREATE' then
  begin
    // SQL CREATE
    // Next word should be 'TABLE' as only CREATE TABLE syntax is supported
    lex := '';
    while (i < len) and (InputQuery[i] <> ' ') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)] := InputQuery[i];
      inc(i);
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    if uppercase(lex) <> 'TABLE' then
    begin
      resp^.error := 'SQL syntax error: TABLE expected after CREATE';
      result:= SDS_Result(resp);
      exit;
    end;
    buff := sds_compile_name;
    // Skipping whitespace and looking for brace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    if InputQuery[i] <> '(' then
    begin
      resp^.error := 'SQL syntax error: ( expected after TABLE';
      result:= SDS_Result(resp);
      exit;
    end;
    inc(i);
    // Reading column name/type defination into cols
    cols := sds_compile_types;
    resp^.cs := cols;
    // Performing query
    flag := CreateTable(buff, @cols);
    if flag then resp^.rows := 1 else resp^.rows := 0;
  end;
  if resp^.cmd = 'DROP' then
  begin
    // SQL DROP
    // Next word should be 'TABLE' as only DROP TABLE syntax is supported
    lex := '';
    while (i < len) and (InputQuery[i] <> ' ') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)] := InputQuery[i];
      inc(i);
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    if uppercase(lex) <> 'TABLE' then
    begin
      resp^.error := 'SQL syntax error: TABLE expected after DROP';
      result:= SDS_Result(resp);
      exit;
    end;
    buff := sds_compile_name;
    // Performing query
    // Checking if exists
    if not FileExists(buff) then
    begin
      resp^.error := 'File does not exist: ' + buff;
      result:= SDS_Result(resp);
      exit;
    end;
    flag := DropTable(buff);
    if flag then resp^.rows := 1 else resp^.rows := 0;
  end;
  if resp^.cmd = 'INSERT' then
  begin
      // SQL INSERT
      // Next word should be 'INTO' as only INSERT INTO syntax is supported
      lex := '';
      while (i < len) and (InputQuery[i] <> ' ') do
      begin
        SetLength(lex, length(lex) + 1);
        lex[length(lex)] := InputQuery[i];
        inc(i);
      end;
      // Skipping whitespace
      while (i < len) and (InputQuery[i] = ' ') do inc(i);
      if uppercase(lex) <> 'INTO' then begin
        resp^.error := 'SQL syntax error: INTO expected after INSERT';
        result:= SDS_Result(resp);
        exit;
      end;
      buff := sds_compile_name;
      // Checking if exists
      if not FileExists(buff) then begin
        resp^.error := 'File does not exist: ' + buff;
        result:= SDS_Result(resp);
        exit;
      end;
      // Reading table data
      ReadInfo(buff, hdr, cols);
      // Skipping whitespace and looking for brace
      while (i < len) and (InputQuery[i] = ' ') do inc(i);
      if InputQuery[i] <> '(' then begin
        resp^.error := 'SQL syntax error: ( expected after ' + buff;
        result:= SDS_Result(resp);
        exit;
      end;
      inc(i);
      // Getting field list
      fields := sds_compile_fields(@cols);
      // Closing brace
      if InputQuery[i] <> ')' then begin
        resp^.error := 'SQL syntax error: ) expected after field list';
        result:= SDS_Result(resp);
        exit;
      end;
      inc(i);
      // Skipping whitespace
      while (i < len) and (InputQuery[i] = ' ') do inc(i);
      // Next word should be 'VALUES'
      lex := '';
      while (i < len) and (InputQuery[i] <> ' ') do
      begin
        SetLength(lex, length(lex) + 1);
        lex[length(lex)] := InputQuery[i];
        inc(i);
      end;
      if uppercase(lex) <> 'VALUES' then
      begin
        resp^.error := 'SQL syntax error: VALUES expected after the field list';
        result:= SDS_Result(resp);
        exit;
      end;
      // Skipping whitespace and looking for brace
      while (i < len) and (InputQuery[i] = ' ') do inc(i);
      if InputQuery[i] <> '(' then
      begin
        resp^.error := 'SQL syntax error: ( expected after VALUES';
        result:= SDS_Result(resp);
        exit;
      end;
      inc(i);
      // Getting values
      values := sds_compile_values;
      // Closing brace
      if InputQuery[i] <> ')' then
      begin
        resp^.error := 'SQL syntax error: ) expected after value list';
        result:= SDS_Result(resp);
        exit;
      end;
      inc(i);
      // Performing query
      resp^.rows := InsertRow(buff, @fields, @values);
  end;
  if resp^.cmd = 'DELETE' then
  begin
    // SQL DELETE
    // Next word should be 'FROM' as only DELETE FROM syntax is supported
    lex := '';
    while (i < len) and (InputQuery[i] <> ' ') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)] := InputQuery[i];
      inc(i);
    end;
    if uppercase(lex) <> 'FROM' then
    begin
      resp^.error := 'SQL syntax error: FROM expected after DELETE';
      result:= SDS_Result(resp);
      exit;
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Getting name
    buff := sds_compile_name;
    // Checking if exists
    if not FileExists(buff) then begin
      resp^.error := 'File does not exist: ' + buff;
      result:= SDS_Result(resp);
      exit;
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Next word should be 'WHERE'
    lex:= '';
    while (i < len) and (InputQuery[i] <> ' ') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)]:= InputQuery[i];
      inc(i);
    end;
    if uppercase(lex) <> 'WHERE' then
    begin
      resp^.error := 'SQL syntax error: WHERE expected after ' + buff;
      result:= SDS_Result(resp);
      exit;
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Retriving table info
    ReadInfo(buff, hdr, cols);
    // Reading and compiling search condition
    cond := sds_compile_condition(@cols);
    // Performing query
    resp^.rows := DeleteRow(buff, @cond);
  end;
  if resp^.cmd = 'UPDATE' then
  begin
    // SQL UPDATE
    lex := '';
    // Reading table name
    buff := sds_compile_name;
    // Checking if exists
    if not FileExists(buff) then begin
      resp^.error := 'File does not exist: ' + buff;
      result:= SDS_Result(resp);
      exit;
    end;
    // Fetching info
    ReadInfo(buff, hdr, cols);
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Next word should be 'SET'
    lex := '';
    while (i < len) and (InputQuery[i] <> ' ') do begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)] := InputQuery[i];
      inc(i);
    end;
    if uppercase(lex) <> 'SET' then begin
      resp^.error := 'SQL syntax error: SET expected after ' + buff;
      result:= SDS_Result(resp);
      exit;
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Parsing SET data
    updset := sds_compile_set(@cols);
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Next word should be 'WHERE'
    lex := '';
    while (i < len) and (InputQuery[i] <> ' ') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)] := InputQuery[i];
      inc(i);
    end;
    if uppercase(lex) <> 'WHERE' then
    begin
      resp^.error := 'SQL syntax error: WHERE expected after the UPDATE name = value data';
      result:= SDS_Result(resp);
      exit;
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Reading condition
    cond := sds_compile_condition(@cols);
    // Performing query
    resp^.rows := Update(buff, @updset, @cond);
  end;
  if resp^.cmd = 'SELECT' then
  begin
    // SQL SELECT
    // SELECT, SELECT COUNT(*) or SELECT ALL
    bflag := 0;
    if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'MIN(') then
    begin
      bflag := 5;
      i := i + 4;
    end;
    if ((i + 4) < len) and (uppercase(copy(InputQuery, i, 4)) = 'MAX(') then
    begin
      bflag := 4;
      i := i + 4;
    end;
    if ((i + 8) < len) and (uppercase(copy(InputQuery, i, 8)) = 'COUNT(*)') then
    begin
      bflag := 3;
      i := i + 8;
    end;
    if ((i + 3) < len) and (uppercase(copy(InputQuery, i, 3)) = 'ALL') then
    begin
      bflag := 2;
      i := i + 3
    end;
    if (i < len) and (InputQuery[i] = '*') then
    begin
      bflag := 2;
      inc(i);
    end;
    if bflag = 0 then bflag := 1;
    if (bflag = 1) or (bflag = 4) or (bflag = 5) then
    begin
      // Getting field list
      // We need table name before we can extract fields
      j := i;
      while (j < len) and (lex <> 'FROM') do
      begin
        lex := '';
        // Ignoring whitespace
        while (j < len) and (InputQuery[j] = ' ') do inc(j);
        // Reading data till ' '
        while (j < len) and (InputQuery[j] <> ' ') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)] := InputQuery[j];
          inc(j);
        end;
      end;
      while (j < len) and (InputQuery[j] = ' ') do inc(j);
      lex := '';
      if (InputQuery[j] = '''') or (InputQuery[j] = '"') or (InputQuery[j] = '`') then
      begin
        // Quoted filename
        quot := InputQuery[j];
        inc(j);
        while (j <= len) and (InputQuery[j] <> quot) do
        begin
          SetLength(lex, length(lex) + 1);                               
          lex[length(lex)] := InputQuery[j];
          inc(j);
        end;
        inc(j);
      end
      else
      begin
        // Reading till whitespace
        while (j <= len) and (InputQuery[j] <> ' ') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)]:= InputQuery[j];
          inc(j);
        end;
      end;
      buff:= lex;
      // Checking if exists
      if not FileExists(buff) then
      begin
        resp^.error:= 'File does not exist: ' + buff;
        result:= SDS_Result(resp);
        exit;
      end;
      // Fetching table info
      ReadInfo(buff, hdr, cols);
      resp^.fields:= hdr.fields;
      resp^.cs:= cols;
    end;
    if bflag = 1 then
    begin
      // Reading field list
      fields:= sds_compile_fields(@cols);
    end;
    if (bflag = 4) or (bflag = 5) then
    begin
      // Getting name of the key field for MIN/MAX()
      if InputQuery[i] = '`' then
      begin
        // Quoted field name. Getting string inside of backticks
        inc(i);
        lex:= '';
        while (i <= len) and (InputQuery[i] <> '`') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)]:= InputQuery[i];
          inc(i);
        end;
        inc(i);
      end
      else
      begin
        // Unquoted field name. Getting column name till space.
        lex := '';
        while (i <= len) and (InputQuery[i] <> ' ') and (InputQuery[i] <> ')') do
        begin
          SetLength(lex, length(lex) + 1);
          lex[length(lex)] := InputQuery[i];
          inc(i);
        end;
      end;
      // Detecting key value
      for j := 0 to length(cols) - 1 do if cols[j].name = lex then
      begin
        SetLength(fields, 1);
        fields[0] := j;
        break;
      end;
      if InputQuery[i] <> ')' then
      begin
        resp^.error := 'SQL syntax error: ) expected after ' + lex;
        result:= SDS_Result(resp);
        exit;
      end;
      inc(i);
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Next word should be 'FROM'
    lex := '';
    while (i < len) and (InputQuery[i] <> ' ') do
    begin
      SetLength(lex, length(lex) + 1);
      lex[length(lex)] := InputQuery[i];
      inc(i);
    end;
    if uppercase(lex) <> 'FROM' then
    begin
      resp^.error := 'SQL syntax error: FROM expected after the field list';
      result:= SDS_Result(resp);
      exit;
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Getting table name
    buff := sds_compile_name;
    // Checking if exists
    if not FileExists(buff) then
    begin
      resp^.error := 'File does not exist: ' + buff;
      result:= SDS_Result(resp);
      exit;
    end;
    ReadInfo(buff, hdr, cols);
    resp^.cs := cols;
    resp^.fields := hdr.fields;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Next word should be 'WHERE' if it is not whole table select
    if i < len then
    begin
      lex := '';
      while (i < len) and (InputQuery[i] <> ' ') do
      begin
        SetLength(lex, length(lex) + 1);
        lex[length(lex)] := InputQuery[i];
        inc(i);
      end;
      if uppercase(lex) <> 'WHERE' then
      begin
        resp^.error := 'SQL syntax error: WHERE expected after ' + buff;
        result:= SDS_Result(resp);
        exit;
      end;
      // Skipping whitespace
      while (i < len) and (InputQuery[i] = ' ') do inc(i);
      // Getting search condition
      cond := sds_compile_condition(@cols);
      // Skipping whitespace
      while (i < len) and (InputQuery[i] = ' ') do inc(i);
    end;
    // Is there any ORDER BY?
    if ((i + 8) < len) and (uppercase(copy(InputQuery, i, 8)) = 'ORDER BY') then
    begin
      i := i + 8;
      order := sds_compile_order(@cols);
    end;
    // Skipping whitespace
    while (i < len) and (InputQuery[i] = ' ') do inc(i);
    // Is there any LIMIT?
    if ((i + 5) < len) and (uppercase(copy(InputQuery, i, 5)) = 'LIMIT') then
    begin
      i := i + 5;
      values := sds_compile_values;
    end;
    // Perforiming query
    case bflag of
        1: Select(buff, @fields, @cond, @order, @values, resp);
        2: SelectAll(buff, @cond, @order, @values, resp);
        3: SelectCount(buff, @cond, @values, resp);
        4: SelectMax(buff, @fields, @cond, @values, resp);
        5: SelectMin(buff, @fields, @cond, @values, resp);
    end;
  end;
  // Getting query time
  resp^.time := StampSpan(stamp, Now);
  // Converting returned value
  result := SDS_Result(resp);
  {$ifdef DBUG_ON}debugln('Query end');{$endif}
end;

// Returns type of the command which caused the result
function ResultCmd(rp: SDS_Result): string;
var res: SDS_PRowSet;
begin
  {$ifdef DBUG_ON}debugln('ResultCmd begin');{$endif}
  res := SDS_PRowSet(rp);
  result := res^.cmd;
  {$ifdef DBUG_ON}debugln('ResultCmd end');{$endif}
end;

// True if the result pointer is at the end of rowset
function ResultEof(rp: SDS_Result): boolean;
var res: SDS_PRowSet;
begin
  {$ifdef DBUG_ON}debugln('ResultEof begin');{$endif}
  res := SDS_PRowSet(rp);
  if res^.point >= res^.rows then result := true else result := false;
  {$ifdef DBUG_ON}debugln('ResultEof end');{$endif}
end;

// Returns last error message if error was caused by the query
function ResultError(rp: SDS_Result): string;
var 
  res: SDS_PRowSet;
begin
  res := SDS_PRowSet(rp);
  result := res^.error;
end;

// Returns number of field per row in the result
function ResultFields(rp: SDS_Result): longint;
var 
  res: SDS_PRowSet;
begin
  res := SDS_PRowSet(rp);
  result := res^.fields;
end;

// Returns current row pointer position in the result
function ResultPointer(rp: SDS_Result): longint;
var res: SDS_PRowSet;
begin
  res := SDS_PRowSet(rp);
  result := res^.point;
end;

// Rewinds the result pointer
procedure ResultRewind(rp: SDS_Result);
var res: SDS_PRowSet;
begin
  res := SDS_PRowSet(rp);
  res^.point := 0;
end;

// Returns number of rows in the result
function ResultRows(rp: SDS_Result): longint;
var res: SDS_PRowSet;
begin
  res := SDS_PRowSet(rp);
  result := res^.rows;
end;

// Seeks the row pointer position in the result
procedure ResultSeek(rp: SDS_Result; index: longint);
var res: SDS_PRowSet;
begin
  if index < 0 then exit;
  res := SDS_PRowSet(rp);
  res^.point := index;
end;

// Returns number of seconds passed during query performance
function ResultTime(rp: SDS_Result): double;
var res: SDS_PRowSet;
begin
  res := SDS_PRowSet(rp);
  result := res^.time;
end;

// Fetches row as SDS_Block object
function FetchRow(rp: SDS_Result): SDS_Array;
var res: SDS_PRowSet;
    row: SDS_PBlock;
begin
  {$ifdef DBUG_ON}debugln('FetchRow begin');{$endif}
  res := SDS_PRowSet(rp);
  result:= nil;
  if res^.point = res^.rows then exit;
  new(row);
  row^.row := res^.rs[res^.point];
  row^.cols := @(res^.cs);
  inc(res^.point);
  result := SDS_Array(row);
  {$ifdef DBUG_ON}debugln('FetchRow end');{$endif}
end;

// Frees the result
procedure FreeResult(rp: SDS_Result);
var res: SDS_PRowSet;
    i: longint;
begin
  {$ifdef DBUG_ON}debugln('FreeResult begin');{$endif}
  if rp = nil then exit;
  res := SDS_PRowSet(rp);
  if (res^.rows > 0) and (res^.cmd = 'SELECT') and (length(res^.rs) > 0) then
  begin
    for i := 0 to length(res^.rs) - 1 do dispose(res^.rs[i]);
    SetLength(res^.rs, 0);
  end;
  dispose(res);
  {$ifdef DBUG_ON}debugln('FreeResult end');{$endif}
end;

// Fetches column specified by index as string
function FetchColumn(ap: SDS_Array; index: longint): string;
var row: SDS_PBlock;
begin
  {$ifdef DBUG_ON}debugln('FetchColumn begin');{$endif}
  result:= '';
  if index < 0 then exit;
  row := SDS_PBlock(ap);
  if index >= length(row^.row^) then exit;
  result:= row^.row^[index];
  {$ifdef DBUG_ON}debugln('FetchColumn end');{$endif}
end;

// Fetches column specified by index as longint
function FetchColumn_int(ap: SDS_Array; index: longint): longint;
var row: SDS_PBlock;
    dummy: integer;
begin
  if index < 0 then exit;
  row := SDS_PBlock(ap);
  val(row^.row^[index], result, dummy);
end;

// Fetches column specified by index as double
function FetchColumn_float(ap: SDS_Array; index: longint): double;
var row: SDS_PBlock;
    dummy: integer;
begin
  if index < 0 then exit;
  row := SDS_PBlock(ap);
  val(row^.row^[index], result, dummy);
end;

// Fetches field specified by name as string
function FetchField(ap: SDS_Array; const name: string): string;
var row: SDS_PBlock;
    i: longint;
begin
  {$ifdef DBUG_ON}debugln('FetchField begin');{$endif}
  row := SDS_PBlock(ap);
  result := '';
  if length(row^.cols^) > 0 then
  for i := 0 to length(row^.cols^) - 1 do
    if row^.cols^[i].name = name then begin result:= row^.row^[i]; exit; end;
  {$ifdef DBUG_ON}debugln('FetchField end');{$endif}
end;

// Fetches field specified by name as longint
function FetchField_int(ap: SDS_Array; const name: string): longint;
var dummy: integer;
begin
  val(FetchField(ap, name), result, dummy);
end;

// Fetches field specified by name as double
function FetchField_float(ap: SDS_Array; const name: string): double;
var dummy: integer;
begin
  val(FetchField(ap, name), result, dummy);
end;

// Frees memory occupied by row
procedure FreeRow(ap: SDS_Array);
var row: SDS_PBlock;
begin
  if ap = nil then exit;
  row := SDS_PBlock(ap);
  dispose(row);
end;

// Imports SDS2/SQL dump and returns number of queries successfully run
function ImportSQL(const dump: string): longint;
var res: SDS_Result;
    sql: string;
    i, len: longint;
begin
  // Initialization
  result := 0;
  i := 1;
  len := length(dump);
  while i < len do
  begin
    sql := '';
    // Skipping spaces, tabs and new lines
    while (i < len) and ((dump[i] = ' ') or (dump[i] = #13)
             or (dump[i] = #10) or (dump[i] = #9)) do
      inc(i);
    // Skip the comment
    if ((i + 1) < len) and (dump[i] = '/') and (dump[i + 1] = '*') then
    begin
      i := i + 2;
      repeat
          inc(i);
      until (dump[i] = '*') and (dump[i + 1] = '/');
      i := i + 2;
    end;
    // Skipping spaces, tabs and new lines
    while (i < len) and ((dump[i] = ' ') or (dump[i] = #13) or (dump[i] = #10)
      or (dump[i] = #9)) do inc(i);
    // Getting SQL command till the semicolon
    if dump[i] = '/' then continue;
    while i < len do
    begin
      if (dump[i] = ';') and (((i + 1) = len) or ((dump[i + 1] = #13) and
        (dump[i + 2] = #10))) then
      begin
        i := i + 3;
        break;
      end;
      SetLength(sql, length(sql) + 1);
      sql[length(sql)] := dump[i];
      inc(i);
    end;
    res := Query(sql);
    if ResultError(res) = '' then
      inc(result)
    else
      writeln(ResultError(res));
    FreeResult(res);
    // Skipping spaces, tabs and new lines
    while (i < len) and ((dump[i] = ' ') or (dump[i] = #13) or (dump[i] = #10)
      or (dump[i] = #9)) do inc(i);
  end;
end;

initialization
{$ifdef PWUDEBUG}
  pwdebugplugin.DebugInit(debugt, 'pwsds.debug.log');  
{$endif}
finalization
{$ifdef PWUDEBUG}
  pwdebugplugin.DebugFini(debugt);  
{$endif}
end.
