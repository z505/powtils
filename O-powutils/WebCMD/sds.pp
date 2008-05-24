{$DEFINE EXTRA_SECURE}
{$IFDEF FPC}{$H+}{$MODE OBJFPC}
    {$IFDEF EXTRA_SECURE}
     {$R+}{$Q+}{$CHECKPOINTER ON}
    {$ENDIF}
{$ENDIF}
unit sds;
{
********************************************************************************
                   -== Simple Data Storage Unit (SDS) ==-
********************************************************************************
  A bit more comfortable than plain text way to store data based on
  electronic tables and SQL.
********************************************************************************
  See the Pascal Server Pages Documentation for more information.
********************************************************************************
    Written by Vladimir Sibirov a.k.a. Trustmaster
    http://www.psp.furtopia.org
    mailto:psp@furtopia.org
********************************************************************************
  Copyright (c) 2003-2005 by Pascal Server Pages development team.
  See the Pascal Server Pages License for more information.
********************************************************************************

  [SDS 2.0.0 - 20/OCT/2005 - L505]:

    - minor changes for PWU/PSP 1.5.X
  
  [SDS 2.0.0 - 22.08.05 - Trustmaster]:

    - added LIMIT in SELECT statements.

  [SDS 2.0.0 - 18.08.05 - Trustmaster]:

    - added REGEXP operator support, ENABLE_REGEXP compiler defination
      enables support of this operator but I recommend not to use it without
      need because it adds about 150kb to the executable size.

  [SDS 2.0.0 - 15.08.05 - Trustmaster]:

    - changed file sharing stuff;

    - added import support into INSERT, added sds_import_sql function;

    - added sds_escape and SQL-ijection protection.

  [SDS 2.0.0 - 14.08.05 - Trustmaster]:

    - added DATE, TIME and DATETIME types support;

    - NOW macro is available for DATE, TIME and DATETIME in INSERT, UPDATE and

    - conditions;

    - added SELECT MAX() and SELECT MIN() syntax support;

    - added query time calculation (is it a FPC bug, but most of the queries
      take 0.000000000000 seconds for execution).

  [SDS 2.0.0 - 13.08.05 - Trustmaster]:

    - finally got SDS2 compiled, debugged and working. What are the changes?
      Actually, absolutely new unit!

  [SDS 1.4.0 - 4.06.05 - Trustmaster]:

    - PSP2-compliancy revision.

  [SDS 1.4.0 - 27.05.05 - Trustmaster]:

    - SDS_ExportCSV, SDS_ExportSQL and SDS_ExportXML routines added.

  [SDS 1.3.0 - Trustmaster]:

    - changed file format, rowcount is now defined in file header to optimise
      SDS_NumOfRows() calls;

    - using sds_safeline instead of httpenc to make it work a little faster;                                                             *

    - using dynamic arrays in rows and results;

    - code optimisations;

    - unified table structure: ID is accessible as field[0];

    - frequently called routines are stored in dedicated functions to avoid bugs
      in program logic (there was such a problem).

  [SDS 1.2.2 - Trustmaster]:

    - some code optimisations: speed, size and memory usage.

  [SDS 1.2.1 - Trustmaster]:

    - all integer variables are defined as LongInts (to support large tables).                                                   *

  [SDS 1.2.0 - Trustmaster]:

    - changed file format, last ID is registered in file header for every new
      ID to be unique.

  [SDS 1.1.0 - Trustmaster]:

    - memory optimisation;

    - data is HTTP-encoded to prevent from internal errors;                                                                       *

    - fixed some bugs;

    - added new functions.

********************************************************************************
 }

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

// Define it if REGEXP operator is needed (not yet supported)
//{$DEFINE ENABLE_REGEXP}

interface
uses
  subStrings, // Advanced String operations
  FileShareUnit, // File access in shared mode
  {$IFDEF ENABLE_REGEXP}regexp_tregexpr,{$ENDIF}
  sysutils; // DATETIME operations

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type 
  SDS_Result= Pointer;

  SDS_Array= Pointer;

  SDS_ColumnInfo= Pointer;

  // Boolean Logics AND= 1, OR= 2, XOR= 3, AND NOT= 4, OR NOT= 5, XOR NOT= 6.
  TBooleanLogic= (blStart, blAnd, blOr, blXor, blAndNot, blOrNot, blXorNot, blNone);

  // Column Data Types
  TColumnDataType= (cdtStart, cdtVarchar, cdtInt, cdtReal, cdtDate, cdtTime,
           cdtDateTime, cdtNone);

// Wrapper class for SDS functions

  { TSDSWrapper }

  TSDSWrapper= class (TObject)
  private
  public
    constructor Create;
    procedure Free;
    
    procedure Column_Free (cp: SDS_ColumnInfo);
    function Column_Index (cp: SDS_ColumnInfo; const Name: String): LongWord;
    function Column_Info (const from: String): SDS_ColumnInfo;
    function Column_Name (cp: SDS_ColumnInfo; Index: LongWord): String;
    function Column_Type (cp: SDS_ColumnInfo; Index: LongWord): TColumnDataType;
    function Escape (const Str: String): String;
    function Export_Csv (const From: String): String;
    function Export_SDS (const From: String): String;
    function Export_SQL (const From, SQLTable: String): String;
    function Export_XML (const From, TableTag, RowTag: String): String;
    function Fetch_Column (ap: SDS_Array; Index: LongWord): String;
    function Fetch_Column_Float (ap: SDS_Array; Index: LongWord):  Double;
    function Fetch_Column_Int (ap: SDS_Array; Index: LongWord): LongInt;
    function Fetch_Field (ap: SDS_Array; const Name: String): String;
    function Fetch_Field_float (ap: SDS_Array; const Name: String):  Double;
    function Fetch_Field_int (ap: SDS_Array; const Name: String): LongInt;
    function Fetch_Row (rp: SDS_Result): SDS_Array;
    procedure Free_Result (rp: SDS_Result);
    procedure Free_Row (ap: SDS_Array);
    
    function Import_SQL (const dump: String): LongWord;
    function Last_ID (const from: String): LongWord;
    function Num_Fields (const from: String): LongWord;
    function Num_Rows (const from: String): LongWord;
    function Result_CMD (rp: SDS_Result): String;
    function Result_EOF (rp: SDS_Result): Boolean;
    function Result_Error (rp: SDS_Result): String;
    function Result_Fields (rp: SDS_Result): LongWord;
    function Result_Pointer (rp: SDS_Result): LongWord;
    procedure Result_Rewind (rp: SDS_Result);
    function Result_Rows (rp: SDS_Result): LongWord;
    procedure Result_Seek (rp: SDS_Result; Index: LongWord);
    function Result_Time (rp: SDS_Result):  Double;
    function Total_Fields (const From: String): LongWord;
    function Query (const QueryString: String): SDS_Result;
    
  end;
{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

// Public functions
// Read the documentation for the information about these functions


procedure sds_column_free (cp: SDS_ColumnInfo);

function sds_column_index (cp: SDS_ColumnInfo; const name: String): LongWord;

function sds_column_info (const from: String): SDS_ColumnInfo;

function sds_column_name (cp: SDS_ColumnInfo; index: LongWord): String;

function sds_Column_Type (cp: SDS_ColumnInfo; Index: LongWord): TColumnDataType;

function sds_escape (const str: String): String;

function sds_export_csv (const from: String): String;

function sds_export_sds (const from: String): String;

function sds_export_sql (const from, sql_table: String): String;

function sds_export_xml (const from, table_tag, row_tag: String): String;

function sds_fetch_column (ap: SDS_Array; index: LongWord): String;

function sds_fetch_column_float (ap: SDS_Array; index: LongWord):  Double;

function sds_fetch_column_int (ap: SDS_Array; index: LongWord): LongInt;

function sds_fetch_field (ap: SDS_Array; const name: String): String;

function sds_fetch_field_float (ap: SDS_Array; const name: String):  Double;

function sds_fetch_field_int (ap: SDS_Array; const name: String): LongInt;

function sds_fetch_row (rp: SDS_Result): SDS_Array;

procedure sds_free_result (rp: SDS_Result);

procedure sds_free_row (ap: SDS_Array);

function sds_import_sql (const dump: String): LongWord;

function sds_last_id (const from: String): LongWord;

function sds_num_fields (const from: String): LongWord;

function sds_num_rows (const from: String): LongWord;

function sds_result_cmd (rp: SDS_Result): String;

function sds_result_eof (rp: SDS_Result): Boolean;

function sds_result_error (rp: SDS_Result): String;

function sds_result_fields (rp: SDS_Result): LongWord;

function sds_result_pointer (rp: SDS_Result): LongWord;

procedure sds_result_rewind (rp: SDS_Result);

function sds_result_rows (rp: SDS_Result): LongWord;

procedure sds_result_seek (rp: SDS_Result; index: LongWord);

function sds_result_time (rp: SDS_Result):  Double;

function sds_total_fields (const from: String): LongWord;

function sds_query (const query: String): SDS_Result;


{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{==============================================================================}
{================================   const   ===================================}
{==============================================================================}
const
//Captions for default TColumnDataType
  TColumnDataTypeCaption: array [cdtStart..cdtNone] of array [0..1] of String= (
    ('*', '*'),               //cdtStart a Dummy Value
    ('VARCHAR', 'TEXT'),      //cdtVarChar
    ('INT', 'INTEGER'),       //cdtInt
    ('REAL', 'DOUBLE'),       //cdtREAL
    ('DATE', 'DATE'),         //cdtDate
    ('TIME', 'TIME'),         //cdtTime
    ('DATETIME', 'DATETIME'), //cdtDateTime
    ('NONE', 'NONE')          //cdtNone a Dummy Value
      );

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type 
    // Type of table row
    SDS_Row= array of String;
  
    // Pointer to SDS_Row
    SDS_PRow= ^SDS_Row;

    // SDS table header    
    SDS_Header= record
        intro: String;
        fields, rows, last_id: LongWord;
    end;

    // Pointer to SDS_Header    
    SDS_PHeader= ^SDS_Header;

    // Column data
    SDS_Col= record
        Name: String; // Column name
        DataType: TColumnDataType; // Data type:
        // cdtVarchar= VARCHAR, cdtInt= INT, cdtReal= REAL, cdtDate= DATE, cdtTime= TIME, cdtDateTime= DATETIME.
    end;

    // Table column data
    SDS_Cols= array of SDS_Col;

    // Pointer to SDS_Cols
    SDS_PCols= ^SDS_Cols;

    // Comparsion record
    SDS_Comp= record
        Field: LongWord; // Field number
        Opr, value: String; // Operator and value for comparsion
        Logic: TBooleanLogic; // Boolean logic:
    end;

    // SDS condition type
    SDS_Condition= array of SDS_Comp;

    // Pointer to SDS_Condition
    SDS_PCondition= ^SDS_Condition;

    // List of fields
    SDS_FieldList= array of LongWord;

    // Pointer to SDS_FieldList
    SDS_PFieldList= ^SDS_FieldList;

    // UPDATE value pair
    SDS_SetItem= record
        field: LongWord;
        value: String;
    end;

    SDS_Set= array of SDS_SetItem;

    // Pointer to SDS_Set
    SDS_PSet= ^SDS_Set;

    // Item of rowset order
    SDS_OrdItem= record
        key: LongWord; // Key field
        dir: Boolean; // Direction FALSE= ascending, TRUE= descending
    end;
    
    // Row order array
    SDS_Order= array of SDS_OrdItem;

    // Pointer to SDS_Order
    SDS_POrder= ^SDS_Order;

    // Internal row representation    
    SDS_Block= record
        row: SDS_PRow; // Row data pointer
        cols: SDS_PCols; // Column data pointer
    end;

    // Pointer to SDS_Block
    SDS_PBlock= ^SDS_Block;

    // Rowset type
    SDS_RowSet= record
        rs: array of SDS_PRow; // Rowset pointers
        cs: SDS_Cols; // Colset data
        rows: LongWord; // Number of rows
        point: LongWord; // Current row pointer
        fields: LongWord; // Row length
        cmd: String; // SQL command type
        error: String; // Last syntax or I/O error
        time:  Double; // Seconds passed during query performance
    end;

    // Pointer to SDS_RowSet
    SDS_PRowSet= ^SDS_RowSet;


{==============================================================================}
{========================= Procedures and Functions===========================}
{==============================================================================}

// Only if REGEXP is enabled (it is recommended not to used it if not needed)
{$IFDEF ENABLE_REGEXP}
{--[ regexp_check ]------------------------------------------------------------}
// Checks if str matches RegExp pattern

function sds_regexp (const pattern, str: String): Boolean;
var
  rh: TRegExpr;
  lim_rpos: LongWord;
  pat_lim: String;
begin
  // Init
  Result:= false;
  // Determine pattern boundaries
  pat_lim:= copy (pattern, 1, 1);
  lim_rpos:= substr_rpos (pattern, pat_lim);
  // Building
  rh:= TRegExpr.Create;
  // Split pattern and modifiers
  rh.Expression:= copy (pattern, 2, lim_rpos- 2);
  rh.ModifierStr:= copy (pattern, lim_rpos+ 1, LongWord (length (pattern))- lim_rpos);
  // Execution
  Result:= rh.Exec (str);
  // Finalization
  rh.Free;
end;

{------------------------------------------------------------------------------}
{$ENDIF}


// Replaces CR, LF with special boundaries
function sds_encode (const str: String): String;
begin
  Result:= str;
  if pos (#13, Result) > 0 then
    Result:= substr_replace (Result, #13, '#SDS_CHAR_CR#');
  if pos (#10, Result) > 0 then
    Result:= substr_replace (Result, #10, '#SDS_CHAR_LF#');
end;


// Replaces SDS-boundaries with CR and LF
function sds_decode (const str: String): String;
begin
  Result:= str;
  if pos ('#SDS_CHAR_CR#', Result) > 0 then
    Result:= substr_replace (Result, '#SDS_CHAR_CR#', #13);
  if pos ('#SDS_CHAR_LF#', Result) > 0 then
    Result:= substr_replace (Result, '#SDS_CHAR_LF#', #10);
end;


// Converts dangerous characters into SDS-macros
function sds_escape (const str: String): String;
begin
  Result:= str;
  if pos ('''', Result) > 0 then
    Result:= substr_replace (Result, '''', '#SDS_CHAR_TK#');
  if pos ('"', Result) > 0 then
    Result:= substr_replace (Result, '"', '#SDS_CHAR_QT#');
end;


// MySQL-style escape
function sds_escapesql (const str: String): String;
begin
  Result:= str;
  if pos ('''', Result) > 0 then
    Result:= substr_replace (Result, '''', '\''');
  if pos ('"', Result) > 0 then
    Result:= substr_replace (Result, '"', '\"');
end;


// Unescapes the String
function sds_unescape (const str: String): String;
begin
  Result:= str;
  if pos ('#SDS_CHAR_TK#', Result) > 0 then
    Result:= substr_replace (Result, '#SDS_CHAR_TK#', '''');
  if pos ('#SDS_CHAR_QT#', Result) > 0 then
    Result:= substr_replace (Result, '#SDS_CHAR_QT#', '"');
end;


// Compares 2 SQL DATE fields (YYYY-MM-DD)
function sds_date_comp (date1, date2: String): shortint;
var
  n1, n2: word;
begin
  // Comparing years
  val (copy (date1, 1, 4), n1);
  val (copy (date2, 1, 4), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing months
  val (copy (date1, 6, 2), n1);
  val (copy (date2, 6, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing days
  val (copy (date1, 9, 2), n1);
  val (copy (date2, 9, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // If still equal
  Result:= 0;
end;


// Compares 2 SQL TIME fields (HH:MM:SS)
function sds_time_comp (date1, date2: String): shortint;
var 
  n1, n2: word;
begin
  // Comparing hours
  val (copy (date1, 1, 2), n1);
  val (copy (date2, 1, 2), n2);
  if n1 < n2 then
    exit (-1);
  if n1 > n2 then
    exit (1);
  // Comparing minutes
  val (copy (date1, 4, 2), n1);
  val (copy (date2, 4, 2), n2);
  if n1 < n2 then
    exit (-1);
  if n1 > n2 then
    exit (1);
  // Comparing seconds
  val (copy (date1, 7, 2), n1);
  val (copy (date2, 7, 2), n2);
  if n1 < n2 then
    exit (-1);
  if n1 > n2 then
    exit (1);
  // If still equal
  Result:= 0;
end;


// Compares 2 SQL DATETIME fields (YYYY-MM-DD HH:MM:SS)
function sds_datetime_comp (date1, date2: String): shortint;
var 
  n1, n2: word;
begin
  // Comparing years
  val (copy (date1, 1, 4), n1);
  val (copy (date2, 1, 4), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing months
  val (copy (date1, 6, 2), n1);
  val (copy (date2, 6, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing days
  val (copy (date1, 9, 2), n1);
  val (copy (date2, 9, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing hours
  val (copy (date1, 12, 2), n1);
  val (copy (date2, 12, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing minutes
  val (copy (date1, 15, 2), n1);
  val (copy (date2, 15, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // Comparing seconds
  val (copy (date1, 18, 2), n1);
  val (copy (date2, 18, 2), n2);
  if n1 < n2 then exit (-1);
  if n1 > n2 then exit (1);
  // If still equal
  Result:= 0;
end;


// Returns difference between two timestamps in seconds
function sds_stamp_span (const stamp1, stamp2: TDateTime):  Double;
begin
  Result:= abs (stamp2- stamp1) * 86400;
end;


// Inserts a new row by copying
procedure sds_rowset_add (var res: SDS_PRowSet; row: SDS_PRow);
var 
  i: LongWord;
begin
  Inc (res^.rows);
  SetLength (res^.rs, res^.rows);
  new (res^.rs [res^.rows- 1]);
  SetLength (res^.rs [res^.rows- 1]^, res^.fields);
  for i:= 0 to res^.fields- 1 do res^.rs [res^.rows- 1]^ [i]:= row^ [i];
end;


// Sets a custom row by copying
procedure sds_rowset_set (var res: SDS_PRowSet; index: LongWord; row: SDS_PRow);
var
  i: LongWord;
begin
  for i:= 0 to res^.fields- 1 do
    res^.rs [index]^ [i]:= row^ [i];
end;


// Reverses the rowset
procedure sds_rowset_reverse (var rs: array of SDS_PRow);
var
  i,
  j: LongWord;
  temp: SDS_PRow;
begin
  i:= 0;
  j:= length (rs)- 1;
  repeat
    temp:= rs [i];
    rs [i]:= rs [j];
    rs [j]:= temp;
    Inc (i);
    dec (j);
  until i >= j;
end;


// Ascending rowset sorting by key field
procedure sds_shell_sort (var rowset: array of SDS_PRow; key: LongWord; cs: SDS_PCols);
type 
  SedjInc= array of LongInt;

    function pow (num, deg: LongInt): LongInt;
    var
      i: LongWord;
    begin
      Result:= 1;
      for i:= 1 to deg do Result:= Result * num;
    end;
    
    // Sedjewick increment formula
    function increment (var incr: SedjInc; size: LongInt): LongInt;
    var
      s: LongInt;
    begin
      s:= -1;
      repeat
        Inc (s);
        SetLength (incr, s+ 1);
        if (s mod 2) > 0 then
          incr [s]:= 8*pow (2, s)- 6*pow (2, (s+ 1) div 2)+ 1
        else
          incr [s]:= 9*pow (2, s)- 9*pow (2, s div 2)+ 1;
      until 3*incr [s] >= size;
      Result:= s;
    end;

var
  incr, 
  i, 
  j, 
  s, 
  int1, int2: LongInt;
  size: LongWord;
  seq: SedjInc;
  temp: SDS_PRow;
  flt1, flt2:  Double;
begin
  size:= length (rowset);
  s:= increment (seq, size);
  while (s >= 0) do
  begin
    incr:= seq [s];
    dec (s);
    for i:= incr to size- 1 do
    begin
      temp:= rowset [i];
      j:= i- incr;
      // Need type-specific comparison
      if cs^ [key].DataType= cdtVarchar then
          begin
            while (j >= 0) and (str_comp (rowset [j]^ [key], temp^ [key]) > 0) do
            begin
                rowset [j+ incr]:= rowset [j];
                j:= j- incr;
            end;
          end
         else
           if cs^ [key].DataType= cdtInt then
           begin
             while (j >= 0) do
             begin
               val (rowset [j]^ [key], int1);
               val (temp^ [key], int2);
               if int1 <= int2 then  Break;
               rowset [j+ incr]:= rowset [j];
               j:= j- incr;
            end;
          end else
            if cs^ [key].DataType= cdtReal then
            begin
              while (j >= 0) do
              begin
                val (rowset [j]^ [key], flt1);
                val (temp^ [key], flt2);
                if flt1 <= flt2 then  Break;
                rowset [j+ incr]:= rowset [j];
                j:= j- incr;
              end;
            end else
              if cs^ [key].DataType= cdtDate then
              begin
                while (j >= 0) and (sds_date_comp (rowset [j]^ [key], temp^ [key]) > 0) do
                begin
                  rowset [j+ incr]:= rowset [j];
                  j:= j- incr;
                end;
              end else
                if cs^ [key].DataType= cdtTime then
                begin
                  while (j >= 0) and (sds_time_comp (rowset [j]^ [key], temp^ [key]) > 0) do
                  begin
                    rowset [j+ incr]:= rowset [j];
                    j:= j- incr;
                  end;
                end else
                  if cs^ [key].DataType= cdtDateTime then
                  begin
                    while (j >= 0) and (sds_datetime_comp (rowset [j]^ [key], temp^ [key]) > 0) do
                    begin
                      rowset [j+ incr]:= rowset [j];
                      j:= j- incr;
                    end;
                  end;
      rowset [j+ incr]:= temp;
    end;
  end;
end;


// Ascending and descending sort by several keys
procedure sds_rowset_sort (res: SDS_PRowSet; order: SDS_POrder);
var 
  temp: array of SDS_PRow;
  i, len, p, j, lt: LongWord;
  flag: Boolean;
begin
  len:= length (order^);
  if len= 0 then exit;
  if res^.rows < 2 then exit;
  if order^ [0].key= 0 then exit;
  // First time we sort by order [0].key
  sds_shell_sort (res^.rs, order^ [0].key, @res^.cs);
  if order^ [0].dir then sds_rowset_reverse (res^.rs);
  if len= 1 then exit;
  // Then we sort equal elements
  for i:= 1 to len- 1 do
  begin
    p:= 0;
    flag:= false;
    SetLength (temp, 0);
    while p < res^.rows do
    begin
      if flag then
      begin
        SetLength (temp, length (temp)+ 1);
        lt:= length (temp)- 1;
        temp [lt]:= res^.rs [p];
        if (((p+ 1) < res^.rows) and (res^.rs [p]^ [order^ [i- 1].key] <> res^.rs [p+ 1]^ [order^ [i- 1].key])) or (p= (res^.rows- 1)) then
        begin
          sds_shell_sort (temp, order^ [i].key, @res^.cs);
          if order^ [i].dir then sds_rowset_reverse (temp);
          for j:= 0 to lt do res^.rs [p- j]:= temp [lt- j];
          flag:= false;
        end;
        Inc (p);
      end else
      begin
        if ((p+ 1) < res^.rows) and (res^.rs [p]^ [order^ [i- 1].key]= res^.rs [p+ 1]^ [order^ [i- 1].key]) then
        begin
          SetLength (temp, 1);
          temp [0]:= res^.rs [p];
          flag:= true;
        end;
        Inc (p);
      end;
    end;
  end;
end;


// Reads the row from the table stream into the array
function sds_read_row (var fh: text; fields: LongWord): SDS_Row;
var 
  cnt: LongWord;
begin
  SetLength (Result, fields);
  for cnt:= 0 to (fields- 1) do
  begin
    ReadLn (fh, Result [cnt]);
    Result [cnt]:= sds_decode (Result [cnt]);
  end;
end;


// Implements the array in table stream
procedure sds_write_row (var fh: text; row: SDS_PRow);
var 
  cnt: LongWord;
begin
  for cnt:= 0 to (length (row^)- 1) do
  begin
    writeln (fh, sds_unescape (sds_encode (row^ [cnt])));
  end;
end;

 
// Reads the table header from open file stream
function sds_read_header (var fh: text): SDS_Header;
begin
  ReadLn (fh, Result.intro);
  ReadLn (fh, Result.fields);
  ReadLn (fh, Result.last_id);
  ReadLn (fh, Result.rows);
  ReadLn (fh);
end;

// Writes the table header into the open file stream
procedure sds_write_header (var fh: text; hdr: SDS_PHeader);
begin
  writeln (fh, hdr^.intro);
  writeln (fh, hdr^.fields);
  writeln (fh, hdr^.last_id);
  writeln (fh, hdr^.rows);
  writeln (fh);
end;


// Reads the table column data
function sds_read_cols (var fh: text; fields: LongWord): SDS_Cols;
var 
  IntDataType: Integer;
  i: LongWord;
begin
  for i:= 0 to fields- 1 do
   begin
     SetLength (Result, i+ 1);
     ReadLn (fh, Result [i].name);
     ReadLn (fh, IntDataType);
     Result [i].DataType:= TColumnDataType (IntDataType);
   end;
   ReadLn (fh);
end;


// Writes table column data
procedure sds_write_cols (var fh: text; cols: SDS_PCols);
var 
  i: LongWord;
begin
  for i:= 0 to length (cols^)- 1 do
  begin
    writeln (fh, cols^ [i].name);
    writeln (fh, Integer (cols^ [i].DataType));
  end;
  writeln (fh);
end;


// Retrives header and column data standalone
procedure sds_read_info (const from: String; var hdr: SDS_Header; var cols: SDS_Cols);
var 
  fh: text;
  fk: word;
begin
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  hdr:= sds_read_header (fh);
  cols:= sds_read_cols (fh, hdr.fields);
  close (fh);
  file_unmark_read (from, fk);
end;



// Checks if row matches the condition
function sds_match_condition (var cond: SDS_PCondition; cols: SDS_PCols; row: SDS_PRow): Boolean;
var 
  i: LongWord;
  sub: Boolean;
  n, nc: LongInt;
  f, fc:  Double;
  t: TColumnDataType; // 1= str, 2= int, 3= float
  tmp: String;
begin
  Result:= true;
  if length (cond^)= 0 then exit (true);
  // Checking each subcondition for matching
  for i:= 0 to length (cond^)- 1 do
  begin
    sub:= false;
    // Determine column type
    t:= cols^ [cond^ [i].field].DataType;
    if t= cdtInt then
    begin
      val (row^ [cond^ [i].field], n);
      val (cond^ [i].value, nc);
    end;
    if t= cdtReal then
    begin
      val (row^ [cond^ [i].field], f);
      val (cond^ [i].value, fc);
    end;
    // Supporting NOW in conditions
    if upcase (cond^ [i].value)= 'NOW' then
    case t of
        cdtDate: cond^ [i].value:= FormatDateTime ('yyyy-mm-dd', now);
        cdtTime: cond^ [i].value:= FormatDateTime ('hh:nn:ss', now);
        cdtDateTime: cond^ [i].value:= FormatDateTime ('yyyy-mm-dd hh:nn:ss', now);
    end;
    // Depends on operator type
    tmp:= upcase (cond^ [i].opr);
    if cond^[i].opr= '=' then
    begin
      // Equal
      case t of
          cdtVarchar: if row^ [cond^ [i].field]= cond^ [i].value then sub:= true;
          cdtInt: if n= nc then sub:= true;
          cdtReal: if f= fc then sub:= true;
          cdtDate: if sds_date_comp (row^ [cond^ [i].field], cond^ [i].value)= 0 then sub:= true;
          cdtTime: if sds_time_comp (row^ [cond^ [i].field], cond^ [i].value)= 0 then sub:= true;
          cdtDateTime: if sds_datetime_comp (row^ [cond^ [i].field], cond^ [i].value)= 0 then sub:= true;
      end;
    end;
    if cond^ [i].opr= '<>' then
    begin
      // Not equal
      case t of
          cdtVarchar: if row^ [cond^ [i].field] <> cond^ [i].value then sub:= true;
          cdtInt    : if n <> nc then sub:= true;
          cdtReal   : if f <> fc then sub:= true;
          cdtDate   : if sds_date_comp (row^ [cond^ [i].field], cond^ [i].value) <> 0 then sub:= true;
          cdtTime   : if sds_time_comp (row^ [cond^ [i].field], cond^ [i].value) <> 0 then sub:= true;
          cdtDateTime: if sds_datetime_comp (row^ [cond^ [i].field], cond^ [i].value) <> 0 then sub:= true;
      end;
    end;
    if cond^ [i].opr= '>' then
    begin
      // Greater than
      case t of
          cdtVarchar: if str_comp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
          cdtInt: if n > nc then sub:= true;
          cdtReal: if f > fc then sub:= true;
          cdtDate: if sds_date_comp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
          cdtTime: if sds_time_comp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
          cdtDateTime: if sds_datetime_comp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
      end;
    end;
    if cond^ [i].opr= '<' then
    begin
      // Less than
      case t of
          cdtVarchar: if str_comp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
          cdtInt: if n < nc then sub:= true;
          cdtReal: if f < fc then sub:= true;
          cdtDate: if sds_date_comp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
          cdtTime: if sds_time_comp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
          cdtDateTime: if sds_datetime_comp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
      end;
    end;
    if cond^ [i].opr= '>=' then
    begin
      // Greater than or equal
      case t of
          cdtVarchar: if str_comp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
          cdtInt: if n >= nc then sub:= true;
          cdtReal: if f >= fc then sub:= true;
          cdtDate: if sds_date_comp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
          cdtTime: if sds_time_comp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
          cdtDateTime: if sds_datetime_comp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
      end;
    end;
    if cond^ [i].opr= '<=' then
    begin
      // Less than or equal
      case t of
          cdtVarchar: if str_comp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
          cdtInt: if n <= nc then sub:= true;
          cdtReal: if f <= fc then sub:= true;
          cdtDate: if sds_date_comp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
          cdtTime: if sds_time_comp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
          cdtDateTime: if sds_datetime_comp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
      end;
    end;
    {$IFDEF ENABLE_REGEXP}
    if tmp= 'REGEXP' then
    begin
      // Matches regular expression
      if sds_regexp (cond^ [i].value, row^ [cond^ [i].field]) then sub:= true;
    end;
    {$ENDIF}
    if tmp= 'CASE_EQ' then
    begin
      // Equal (case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value)= 0 then sub:= true;
    end;
    if tmp= 'CASE_NOT_EQ' then
    begin
      // Not equal (case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) <> 0 then sub:= true;
    end;
    if tmp= 'CASE_LT' then
    begin
      // Less than (case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
    end;
    if tmp= 'CASE_GT' then
    begin
      // Greater than (case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
    end;
    if tmp= 'CASE_LT_OR_EQ' then
    begin
      // Less than or equal (case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
    end;
    if tmp= 'CASE_GT_OR_EQ' then
    begin
      // Greater than or equal (case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
    end;
    if tmp= 'NAT_LT' then
    begin
      // Less than (natural algoritm)
      if str_ncomp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
    end;
    if tmp= 'NAT_GT' then
    begin
      // Greater than (natural algoritm)
      if str_ncomp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
    end;
    if tmp= 'NAT_LT_OR_EQ' then
    begin
      // Less than or equal (natural algoritm)
      if str_ncomp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
    end;
    if tmp= 'NAT_GT_OR_EQ' then
    begin
      // Greater than or equal (natural algoritm)
      if str_ncomp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
    end;
    if tmp= 'NAT_CASE_LT' then
    begin
      // Less than (natural algoritm, case insensitive)
      if str_incomp (row^ [cond^ [i].field], cond^ [i].value) < 0 then sub:= true;
    end;
    if tmp= 'NAT_CASE_GT' then
    begin
      // Greater than (natural algoritm, case insensitive)
      if str_incomp (row^ [cond^ [i].field], cond^ [i].value) > 0 then sub:= true;
    end;
    if tmp= 'NAT_CASE_LT_OR_EQ' then
    begin
      // Less than or equal (natural algoritm, case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) <= 0 then sub:= true;
    end;
    if tmp= 'NAT_CASE_GT_OR_EQ' then
    begin
      // Greater than or equal (natural algoritm, case insensitive)
      if str_icomp (row^ [cond^ [i].field], cond^ [i].value) >= 0 then sub:= true;
    end;
    case cond^ [i].Logic of
        blAnd: Result:= Result and sub;
        blOr: Result:= Result or sub;
        blXor: Result:= Result xor sub;
        blAndNot: Result:= Result and (not sub);
        blOrNot: Result:= Result or (not sub);
        blXorNot: Result:= Result xor (not sub);
    end;
  end;
end;


// Creates a new table
function sds_create (const from: String; cols: SDS_PCols): Boolean;
var
  fh: text;
  hdr: SDS_Header;
begin
  file_mark_write (from);
  assign (fh, from);
  rewrite (fh);
  hdr.intro:= ' [-- Simple Data Storage File --]';
  hdr.fields:= length (cols^);
  hdr.last_id:= 0;
  hdr.rows:= 0;
  sds_write_header (fh, @hdr);
  sds_write_cols (fh, cols);
  close (fh);
  file_unmark_write (from);
  Result:= true;
end;


// Drops a table
function sds_drop (const from: String): Boolean;
var
  fh: text;
begin
  Result:= false;
  if not file_exists (from) then exit (false);
  file_mark_write (from);
  assign (fh, from);
  erase (fh);
  file_unmark_write (from);
  Result:= true;
end;


// Returns number of fields in the table
function sds_num_fields (const from: String): LongWord;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  Result:= 0;
  if not file_exists (from) then exit (0);
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  hdr:= sds_read_header (fh);
  close (fh);
  file_unmark_read (from, fk);
  Result:= hdr.fields;
end;


// Gets last ID in the table
function sds_last_id (const from: String): LongWord;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  Result:= 0;
  if not file_exists (from) then exit (0);
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  hdr:= sds_read_header (fh);
  close (fh);
  file_unmark_read (from, fk);
  Result:= hdr.last_id;
end;


// Returns total number of rows in the table
function sds_num_rows (const from: String): LongWord;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  Result:= 0;
  if not file_exists (from) then exit (0);
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  hdr:= sds_read_header (fh);
  close (fh);
  file_unmark_read (from, fk);
  Result:= hdr.rows;
end;


// Returns total number of cells in the table
function sds_total_fields (const from: String): LongWord;
var 
  fh: text;
  hdr: SDS_Header;
  fk: word;
begin
  Result:= 0;
  if not file_exists (from) then exit (0);
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  hdr:= sds_read_header (fh);
  close (fh);
  file_unmark_read (from, fk);
  Result:= hdr.fields * hdr.rows;
end;


// Reads table column info
function sds_column_info (const from: String): SDS_ColumnInfo;
var 
  fh: text;
  hdr: SDS_Header;
  cols: SDS_PCols;
  fk: word;
begin
  if not file_exists (from) then exit;
  new (cols);
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  hdr:= sds_read_header (fh);
  cols^:= sds_read_cols (fh, hdr.fields);
  close (fh);
  file_unmark_read (from, fk);
  Result:= SDS_ColumnInfo (cols);
end;

// Returns name of the column specified by index
function sds_column_name (cp: SDS_ColumnInfo; index: LongWord): String;
var 
  cols: SDS_PCols;
begin
  cols:= SDS_PCols (cp);
  Result:= cols^ [index].name;
end;


// Returns type of the column specified by index
//   1= TEXT/VARCHAR
//   2= INT/INTEGER
//   3= REAL/DOUBLE
//   4= DATE
//   5= TIME
//   6= DATETIME
function sds_Column_Type (cp: SDS_ColumnInfo; Index: LongWord): TColumnDataType;
var 
  cols: SDS_PCols;
begin
  cols:= SDS_PCols (cp);
  Result:= cols^ [index].DataType;
end;


// Returns index of the column specified by name
function sds_column_index (cp: SDS_ColumnInfo; const name: String): LongWord;
var 
  i: LongWord;
  cols: SDS_PCols;
begin
  cols:= SDS_PCols (cp);
  Result:= 0;
  if length (cols^) > 0 then
  for i:= 0 to length (cols^)- 1 do if cols^ [i].name= name then
    exit (i);
end;


// Frees memory occupied by SDS columns info
procedure sds_column_free (cp: SDS_ColumnInfo);
var 
  cols: SDS_PCols;
begin
  cols:= SDS_PCols (cp);
  dispose (cols);
end;


// Inserts a new row into a table
function sds_insert (const from: String; fields: SDS_PFieldList; values: SDS_PRow): LongWord;
var 
  fh, fhn: text;
  buff: String;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  i: LongWord;
begin
  // Initializing, locking
  Result:= 0;
  if not file_exists (from) then exit (0);
  file_mark_write (from);
  // Copying/modifying header
  assign (fh, from);
  assign (fhn, from+ '.tmp');
  reset (fh);
  rewrite (fhn);
  hdr:= sds_read_header (fh);
  if hdr.rows <> 0 then Inc (hdr.last_id);
  Inc (hdr.rows);
  sds_write_header (fhn, @hdr);
  // Copying column data
  cols:= sds_read_cols (fh, hdr.fields);
  sds_write_cols (fhn, @cols);
  // Copying table contents
  while not eof (fh) do
  begin
    ReadLn (fh, buff);
    writeln (fhn, buff);
  end;
  str (hdr.last_id, buff);
  // Converting fields and values into a single row
  SetLength (row, hdr.fields);
  for i:= 0 to length (fields^)- 1 do
  begin
    // Conversion with function support
    if upcase (values^ [i])= 'NOW' then
    begin
      // Now function
      case cols [fields^ [i]].DataType of
          cdtDate: row [fields^ [i]]:= FormatDateTime ('yyyy-mm-dd', now);
          cdtTime: row [fields^ [i]]:= FormatDateTime ('hh:nn:ss', now);
          cdtDateTime: row [fields^ [i]]:= FormatDateTime ('yyyy-mm-dd hh:nn:ss', now);
          else row [fields^ [i]]:= values^ [i];
      end;
    end else
      row [fields^ [i]]:= values^ [i];
  end;
  if row [0]= '' then
    row [0]:= buff;
  // Inserting new data
  sds_write_row (fhn, @row);
  // Final steps
  Inc (Result);
  close (fh);
  close (fhn);
  erase (fh);
  rename (fhn, from);
  file_unmark_write (from);
end;


// SQL SELECT
procedure sds_select (const from: String; fields: SDS_PFieldList; cond: SDS_PCondition; order: SDS_POrder; limit: SDS_PRow; var res: SDS_PRowSet);
var 
  fh: text;
  i, cnt, loffs, lcnt: LongWord;
  hdr: SDS_Header;
  cols, colt: SDS_Cols;
  row, tmp: SDS_Row;
  fk: word;
  use_limit: Boolean;
begin
  // Preparing LIMIT
  if length (limit^)= 1 then
  begin
    use_limit:= true;
    loffs:= 0;
    val (limit^ [0], lcnt);
  end
  else if length (limit^)= 2 then
  begin
    use_limit:= true;
    val (limit^ [0], loffs);
    val (limit^ [1], lcnt);
  end
  else use_limit:= false;
  cnt:= 0;
  // Opening
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  // Reading header and column data
  hdr:= sds_read_header (fh);
  SetLength (row, hdr.fields);
  SetLength (tmp, length (fields^));
  SetLength (colt, length (fields^));
  res^.fields:= length (fields^);
  cols:= sds_read_cols (fh, hdr.fields);
  for i:= 0 to res^.fields- 1 do colt [i]:= cols [fields^ [i]];
  // Performing select
  while not eof (fh) do
  begin
    row:= sds_read_row (fh, hdr.fields);
    if sds_match_condition (cond, @cols, @row) then
    begin
      Inc (cnt);
      if use_limit and (cnt > (loffs+ lcnt)) then  Break;
      if use_limit and (cnt <= loffs) then Continue;
      for i:= 0 to res^.fields- 1 do tmp [i]:= row [fields^ [i]];
      sds_rowset_add (res, @tmp);
    end;
  end;
  // Replacing Result coldata
  res^.cs:= colt;
  // Sorting
  if length (order^) > 0 then sds_rowset_sort (res, order);
  // Done
  close (fh);
  file_unmark_read (from, fk);
end;


// SELECT * FROM
procedure sds_select_all (const from: String; cond: SDS_PCondition; order: SDS_POrder; limit: SDS_PRow; var res: SDS_PRowSet);
var 
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  fk: word;
  cnt, loffs, lcnt: LongWord;
  use_limit: Boolean;
begin
  // Preparing LIMIT
  if length (limit^)= 1 then
  begin
      use_limit:= true;
      loffs:= 0;
      val (limit^ [0], lcnt);
  end else
    if length (limit^)= 2 then
    begin
      use_limit:= true;
      val (limit^ [0], loffs);
      val (limit^ [1], lcnt);
    end
  else
    use_limit:= false;
  cnt:= 0;
  // Opening
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  // Reading header and column data
  hdr:= sds_read_header (fh);
  SetLength (row, hdr.fields);
  res^.fields:= hdr.fields;
  cols:= sds_read_cols (fh, hdr.fields);
  // Performing select
  while not eof (fh) do
  begin
    row:= sds_read_row (fh, hdr.fields);
    if sds_match_condition (cond, @cols, @row) then
    begin
      Inc (cnt);
      if use_limit and (cnt > (loffs+ lcnt)) then
         Break;
      if use_limit and (cnt <= loffs) then
        Continue;
      sds_rowset_add (res, @row);
    end;
  end;
  // Sorting
  if length (order^) > 0 then sds_rowset_sort (res, order);
  // Done
  close (fh);
  file_unmark_read (from, fk);
end;


// Counts number of rows matched the SELECT
procedure sds_select_count (const from: String; cond: SDS_PCondition; limit: SDS_PRow; var res: SDS_PRowSet);
var
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  fk: word;
  cnt, loffs, lcnt: LongWord;
  use_limit: Boolean;
begin
  // Preparing LIMIT
  if length (limit^)= 1 then
  begin
    use_limit:= true;
    loffs:= 0;
    val (limit^ [0], lcnt);
  end else
    if length (limit^)= 2 then
    begin
      use_limit:= true;
      val (limit^ [0], loffs);
      val (limit^ [1], lcnt);
    end else
      use_limit:= false;
  cnt:= 0;
  // Opening
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  // Reading header and column data
  hdr:= sds_read_header (fh);
  SetLength (row, hdr.fields);
  res^.fields:= hdr.fields;
  cols:= sds_read_cols (fh, hdr.fields);
  // Searching
  while not eof (fh) do
  begin
    row:= sds_read_row (fh, hdr.fields);
    if sds_match_condition (cond, @cols, @row) then
    begin
      Inc (cnt);
      if use_limit and (cnt > (loffs+ lcnt)) then  Break;
      if use_limit and (cnt <= loffs) then Continue;
      Inc (res^.rows);
    end;
  end;
  // Done
  close (fh);
  file_unmark_read (from, fk);
end;


// Selects a row with maximal value of the key field
procedure sds_select_max (const from: String; fields: SDS_PFieldList; cond: SDS_PCondition; limit: SDS_PRow; var res: SDS_PRowSet);
var
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  st: String;
  nt, nr: LongInt;
  ft, fr:  Double;
  first: Boolean;
  fk: word;
  cnt, loffs, lcnt: LongWord;
  use_limit: Boolean;
begin
  // Preparing LIMIT
  if length (limit^)= 1 then
  begin
    use_limit:= true;
    loffs:= 0;
    val (limit^ [0], lcnt);
  end else
    if length (limit^)= 2 then
    begin
      use_limit:= true;
      val (limit^ [0], loffs);
      val (limit^ [1], lcnt);
    end else
      use_limit:= false;
  cnt:= 0;
  // Opening
  file_mark_read (from, fk);
  assign (fh, from);
  reset (fh);
  // Reading header and column data
  hdr:= sds_read_header (fh);
  SetLength (row, hdr.fields);
  res^.fields:= hdr.fields;
  cols:= sds_read_cols (fh, hdr.fields);
  // Performing select MAX ()
  first:= false;
  while not eof (fh) do
  begin
    row:= sds_read_row (fh, hdr.fields);
    if sds_match_condition (cond, @cols, @row) then
    begin
      Inc (cnt);
      if use_limit and (cnt > (loffs+ lcnt)) then  Break;
      if use_limit and (cnt <= loffs) then Continue;
      if not first then
      begin
        first:= true;
        case cols [fields^ [0]].DataType of
            cdtVarchar: st:= row [fields^ [0]];
            cdtInt: val (row [fields^ [0]], nt);
            cdtReal: val (row [fields^ [0]], ft);
            cdtDate: st:= row [fields^ [0]];
            cdtTime: st:= row [fields^ [0]];
            cdtDateTime: st:= row [fields^ [0]];
        end;
        sds_rowset_add (res, @row);
      end else
      begin
        case cols [fields^ [0]].DataType of
            cdtVarchar: if str_comp (row [fields^ [0]], st) <= 0 then Continue else st:= row [fields^ [0]];
            cdtInt: begin
                val (row [fields^ [0]], nr);
                if nr <= nt then Continue else nt:= nr;
            end;
            cdtReal: begin
                val (row [fields^ [0]], fr);
                if fr <= ft then Continue else ft:= fr;
            end;
            cdtDate: if sds_date_comp (row [fields^ [0]], st) <= 0 then Continue else st:= row [fields^ [0]];
            cdtTime: if sds_time_comp (row [fields^ [0]], st) <= 0 then Continue else st:= row [fields^ [0]];
            cdtDateTime: if sds_datetime_comp (row [fields^ [0]], st) <= 0 then Continue else st:= row [fields^ [0]];
        end;
        sds_rowset_set (res, 0, @row);
      end;
    end;
  end;
  // Done
  close (fh);
  file_unmark_read (from, fk);
end;


// Selects a row with minimal value of the key field
procedure sds_select_min (const from: String; fields: SDS_PFieldList; cond: SDS_PCondition; limit: SDS_PRow; var res: SDS_PRowSet);
var
  fh: text;
  hdr: SDS_Header;
  cols: SDS_Cols;
  row: SDS_Row;
  st: String;
  nt, nr: LongInt;
  ft, fr:  Double;
  first: Boolean;
  fk: word;
  cnt, loffs, lcnt: LongWord;
  use_limit: Boolean;
begin
    // Preparing LIMIT
    if length (limit^)= 1 then
        begin
            use_limit:= true;
            loffs:= 0;
            val (limit^ [0], lcnt);
        end
    else if length (limit^)= 2 then
        begin
            use_limit:= true;
            val (limit^ [0], loffs);
            val (limit^ [1], lcnt);
        end
    else use_limit:= false;
    cnt:= 0;
    // Opening
    file_mark_read (from, fk);
    assign (fh, from);
    reset (fh);
    // Reading header and column data
    hdr:= sds_read_header (fh);
    SetLength (row, hdr.fields);
    res^.fields:= hdr.fields;
    cols:= sds_read_cols (fh, hdr.fields);
    // Performing select MIN ()
    first:= false;
    while not eof (fh) do
        begin
            row:= sds_read_row (fh, hdr.fields);
            if sds_match_condition (cond, @cols, @row) then
                begin
                    Inc (cnt);
                    if use_limit and (cnt > (loffs+ lcnt)) then  Break;
                    if use_limit and (cnt <= loffs) then Continue;
                    if not first then
                        begin
                            first:= true;
                            case cols [fields^ [0]].DataType of
                                cdtVarchar: st:= row [fields^ [0]];
                                cdtInt: val (row [fields^ [0]], nt);
                                cdtReal: val (row [fields^ [0]], ft);
                                cdtDate: st:= row [fields^ [0]];
                                cdtTime: st:= row [fields^ [0]];
                                cdtDateTime: st:= row [fields^ [0]];
                            end;
                            sds_rowset_add (res, @row);
                        end
                    else
                        begin
                            case cols [fields^ [0]].DataType of
                                cdtVarchar: if str_comp (row [fields^ [0]], st) >= 0 then Continue else st:= row [fields^ [0]];
                                cdtInt: begin
                                    val (row [fields^ [0]], nr);
                                    if nr >= nt then Continue else nt:= nr;
                                end;
                                cdtReal: begin
                                    val (row [fields^ [0]], fr);
                                    if fr >= ft then Continue else ft:= fr;
                                end;
                                cdtDate: if sds_date_comp (row [fields^ [0]], st) >= 0 then Continue else st:= row [fields^ [0]];
                                cdtTime: if sds_time_comp (row [fields^ [0]], st) >= 0 then Continue else st:= row [fields^ [0]];
                                cdtDateTime: if sds_datetime_comp (row [fields^ [0]], st) >= 0 then Continue else st:= row [fields^ [0]];
                            end;
                            sds_rowset_set (res, 0, @row);
                        end;
                end;
        end;
    // Done
    close (fh);
    file_unmark_read (from, fk);
end;


// SQL update
function sds_update (const from: String; updset: SDS_PSet; cond: SDS_PCondition): LongWord;
var fh, fhn: text;
    i: LongWord;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
begin
    // Initializing
    Result:= 0;
    if not file_exists (from) then exit (0);
    file_mark_write (from);
    // Opening
    assign (fh, from);
    assign (fhn, from+ '.tmp');
    reset (fh);
    rewrite (fhn);
    // Copying header and column data
    hdr:= sds_read_header (fh);
    sds_write_header (fhn, @hdr);
    SetLength (row, hdr.fields);
    cols:= sds_read_cols (fh, hdr.fields);
    sds_write_cols (fhn, @cols);
    // Row-by-row checking and updating
    while not eof (fh) do
        begin
            row:= sds_read_row (fh, hdr.fields);
            if sds_match_condition (cond, @cols, @row) then
                begin
                    // Changing row data
                    for i:= 0 to length (updset^)- 1 do
                        begin
                            // AUTO_INCREMENT PRIMARY KEY can not be modified
                            if updset^ [i].field <> 0 then
                                begin
                                    // Conversion with function support
                                    if upcase (updset^ [i].value)= 'NOW' then
                                        begin
                                            // NOW function
                                            case cols [updset^ [i].field].DataType of
                                                cdtDate: row [updset^ [i].field]:= FormatDateTime ('yyyy-mm-dd', now);
                                                cdtTime: row [updset^ [i].field]:= FormatDateTime ('hh:nn:ss', now);
                                                cdtDateTime: row [updset^ [i].field]:= FormatDateTime ('yyyy-mm-dd hh:nn:ss', now);
                                                else row [updset^ [i].field]:= updset^ [i].value;
                                            end;
                                        end
                                    else row [updset^ [i].field]:= updset^ [i].value;
                                end;
                        end;
                    Inc (Result);
                end;
            sds_write_row (fhn, @row);
        end;
    close (fh);
    close (fhn);
    erase (fh);
    rename (fhn, from);
    file_unmark_write (from);
end;


// Deletes a row by its field value
function sds_delete (const from: String; cond: SDS_PCondition): LongWord;
var fh, fhn: text;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
    buff: String;
begin
    // Initialization, checks, locking
    Result:= 0;
    if not file_exists (from) then exit (0);
    file_mark_write (from);
    assign (fh, from);
    assign (fhn, from+ '.tmp');
    reset (fh);
    rewrite (fhn);
    // Header data
    hdr:= sds_read_header (fh);
    SetLength (row, hdr.fields);
    // We'll have to decrease hdr.rows by Result after the DELETE itself is performed
    sds_write_header (fhn, @hdr);
    // Column data
    cols:= sds_read_cols (fh, hdr.fields);
    sds_write_cols (fhn, @cols);
    while not eof (fh) do
        begin
            row:= sds_read_row (fh, hdr.fields);
            if sds_match_condition (cond, @cols, @row) then Inc (Result)
            else sds_write_row (fhn, @row);
        end;
    close (fh);
    close (fhn);
    erase (fh);
    rename (fhn, from);
    // Modyfing just the header
    assign (fh, from);
    assign (fhn, from+ '.tmp');
    reset (fh);
    rewrite (fhn);
    hdr:= sds_read_header (fh);
    hdr.rows:= hdr.rows- Result;
    sds_write_header (fhn, @hdr);
    cols:= sds_read_cols (fh, hdr.fields);
    sds_write_cols (fhn, @cols);
    // Copying the rest of the file
    while not eof (fh) do
        begin
            ReadLn (fh, buff);
            writeln (fhn, buff);
        end;
    close (fh);
    close (fhn);
    erase (fh);
    rename (fhn, from);
    // Done
    file_unmark_write (from);
end;


// Exports Comma Separated Values from SDS table
function sds_export_csv (const from: String): String;
var fh: text;
    i, j: LongWord;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
    fk: word;
begin
    if not file_exists (from) then exit ('');
    file_mark_read (from, fk);
    assign (fh, from);
    reset (fh);
    hdr:= sds_read_header (fh);
    SetLength (row, hdr.fields);
    cols:= sds_read_cols (fh, hdr.fields);
    for i:= 0 to hdr.rows- 1 do
        begin
            row:= sds_read_row (fh, hdr.fields);
            for j:= 0 to (hdr.fields- 1) do
                begin
                    if pos ('"', row [j]) > 0 then row [j]:= substr_replace (row [j], '"', '""');
                    if (cols [j].DataType in [ cdtVarchar, cdtDate, cdtTime, cdtDateTime]) then
                        begin
                            if j= 0 then Result:= Result+ '"'+ row [j]+ '"'
                            else Result:= Result+ ',"'+ row [j]+ '"';
                        end
                    else
                        begin
                            if j= 0 then Result:= Result+ row [j]
                            else Result:= Result+ ','+ row [j];
                        end;
                end;
            Result:= Result+ #13+ #10;
        end;
    close (fh);
    file_unmark_read (from, fk);
end;


// Exports SDS table `from` to SDS/SQL table dump script.
function sds_export_sds (const from: String): String;
var fh: text;
    i, j: LongWord;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
    buff: String;
    fk: word;
begin
    if not file_exists (from) then exit ('');
    file_mark_read (from, fk);
    Result:= '/**'+ #13+ #10+ '* SDS Table SQL Dump for file:'+ #13+ #10+ '* '+ from+ #13+ #10+ '*/'+ #13+ #10;
    assign (fh, from);
    reset (fh);
    hdr:= sds_read_header (fh);
    SetLength (row, hdr.fields);
    cols:= sds_read_cols (fh, hdr.fields);
    // First CREATE TABLE query
    Result:= Result+ 'CREATE TABLE `'+ from+ '` (`'+ cols [0].name+ '` INT';
    for j:= 1 to (hdr.fields- 1) do
        begin
            case cols [j].DataType of
                cdtVarchar: buff:= 'TEXT';
                cdtInt: buff:= 'INT';
                cdtReal: buff:= 'DOUBLE';
                cdtDate: buff:= 'DATE';
                cdtTime: buff:= 'TIME';
                cdtDateTime: buff:= 'DATETIME';
            end;
            Result:= Result+ ', `'+ cols [j].name+ '` '+ buff;
        end;
    Result:= Result+ ');'+ #13+ #10;
    // Then INSERT ones
    for i:= 0 to (hdr.rows- 1) do
        begin
            Result:= Result+ 'INSERT INTO `'+ from+ '` (';
            for j:= 0 to (hdr.fields- 1) do
                begin
                    if j= 0 then Result:= Result+ '`'+ cols [j].name+ '`'
                    else Result:= Result+ ', `'+ cols [j].name+ '`';
                end;
            Result:= Result+ ') VALUES (';
            row:= sds_read_row (fh, hdr.fields);
            Result:= Result+ row [0];
            // Do RegExp here again to determine whether to use quotes or not ->
            for j:= 1 to (hdr.fields- 1) do
                begin
                    if (cols [j].DataType in [cdtVarchar, cdtDate, cdtTime, cdtDateTime]) then Result:= Result+ ', '''+ sds_escape (row [j])+ ''''
                    else Result:= Result+ ', '+ row [j];
                end;
            Result:= Result+ ');'+ #13+ #10;
        end;
    close (fh);
    file_unmark_read (from, fk);
end;


// Exports SDS table `from` to SQL table dump script for table `sql_table`.
function sds_export_sql (const from, sql_table: String): String;
var fh: text;
    i, j: LongWord;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
    buff: String;
    fk: word;
begin
    if not file_exists (from) then exit ('');
    file_mark_read (from, fk);
    Result:= '/**'+ #13+ #10+ '* SDS Table SQL Dump for file:'+ #13+ #10+ '* '+ from+ #13+ #10+ '*/'+ #13+ #10;
    assign (fh, from);
    reset (fh);
    hdr:= sds_read_header (fh);
    SetLength (row, hdr.fields);
    cols:= sds_read_cols (fh, hdr.fields);
    // First CREATE TABLE query
    Result:= Result+ 'CREATE TABLE `'+ sql_table+ '` (`'+ cols [0].name+ '` INT UNSIGNED PRIMARY KEY AUTO_INCREMENT';
    for j:= 1 to (hdr.fields- 1) do
        begin
            case cols [j].DataType of
                cdtVarchar: buff:= 'TEXT';
                cdtInt: buff:= 'INT';
                cdtReal: buff:= 'DOUBLE';
                cdtDate: buff:= 'DATE';
                cdtTime: buff:= 'TIME';
                cdtDateTime: buff:= 'DATETIME';
            end;
            Result:= Result+ ', `'+ cols [j].name+ '` '+ buff;
        end;
    Result:= Result+ ');'+ #13+ #10;
    // Then INSERT ones
    for i:= 0 to (hdr.rows- 1) do
    begin
        Result:= Result+ 'INSERT INTO `'+ sql_table+ '` (';
        for j:= 0 to (hdr.fields- 1) do
        begin
            if j= 0 then
              Result:= Result+ '`'+ cols [j].name+ '`'
            else
              Result:= Result+ ', `'+ cols [j].name+ '`';
        end;
        
        Result:= Result+ ') VALUES (';
        row:= sds_read_row (fh, hdr.fields);
        Result:= Result+ row [0];
        
        // Do RegExp here again to determine whether to use quotes or not ->
        for j:= 1 to (hdr.fields- 1) do
        begin
            if (cols [j].DataType in [cdtVarchar, cdtDate, cdtTime, cdtDateTime]) then
              Result:= Result+ ', '''+ sds_escapesql (row [j])+ ''''
            else Result:= Result+ ', '+ row [j];
        end;
        Result:= Result+ ');'+ #13+ #10;
    end;
    close (fh);
    file_unmark_read (from, fk);
end;


// Exports SDS table `from` to XML data. Root element tag can be set with
// `table_tag`, each row tag can be set with `row_tag`.
function sds_export_xml (const from, table_tag, row_tag: String): String;
var fh: text;
    i, j: LongWord;
    row: SDS_Row;
    hdr: SDS_Header;
    cols: SDS_Cols;
    fk: word;
begin
    if not file_exists (from) then exit ('');
    file_mark_read (from, fk);
    Result:= '<'+ table_tag+ '>'+ #13+ #10;
    assign (fh, from);
    reset (fh);
    hdr:= sds_read_header (fh);
    SetLength (row, hdr.fields);
    cols:= sds_read_cols (fh, hdr.fields);
    for i:= 0 to (hdr.rows- 1) do
        begin
            Result:= Result+ '<'+ row_tag+ '>'+ #13+ #10;
            row:= sds_read_row (fh, hdr.fields);
            for j:= 0 to (hdr.fields- 1) do
                begin
                    if pos ('<', row [j]) > 0 then row [j]:= substr_replace (row [j], '<', '&lt;');
                    if pos ('>', row [j]) > 0 then row [j]:= substr_replace (row [j], '>', '&gt;');
                    Result:= Result+ '<'+ cols [j].name+ '>'+ row [j]+ '</'+ cols [j].name+ '>'+ #13+ #10;
                end;
            Result:= Result+ '</'+ row_tag+ '>'+ #13+ #10;
        end;
    close (fh);
    file_unmark_read (from, fk);
    Result:= Result+ '</'+ table_tag+ '>';
end;


// Performs SDS/SQL query
function sds_query (const query: String): SDS_Result;
var
  i, j, len: LongWord;
  lex, buff: String;
  quot: char;
  flag: Boolean;
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
    function sds_compile_name: String;
    begin
      // Reading table name
      lex:= '';
      if (query [i]= '''') or (query [i]= '"') or (query [i]= '`') then
      begin
        // Quoted filename
        quot:= query [i];
        Inc (i);
        while (i <= len) and (query [i] <> quot) do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
        Inc (i);
      end
      else
      begin
        // Reading till whitespace or brace
        while (i <= len) and (query [i] <> ' ') and (query [i] <> ' (') do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
      end;
      // Making the Result contain table name
      Result:= lex;
    end;


    // Create fieldset precompiler. Example
    // 'id INT, cash FLOAT, `posts` INT, name VARCHAR'=>
    //  [0].name= 'id',  [0].data_type= 2;  [1].name= 'cash',  [1].data_type= 3,
    //  [2].name= 'posts',  [2].data_type= 3,  [3].name= 'name',  [3].data_type= 1.
    function sds_compile_types: SDS_Cols;
    begin
      // Parsing
      while (i < len) and (query [i] <> ')') do
      begin
        // New Result element
        SetLength (Result, length (Result)+ 1);
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        if query [i]= '`' then
        begin
          // Quoted field name. Getting String inside of backticks
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> '`') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space or colon
          lex:= '';
          while (i <= len) and (query [i] <> ' ') and (query [i] <> ',') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        Result [length (Result)- 1].name:= lex;
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Getting field data type until space or colon or brace
        lex:= '';
        while (i <= len) and (query [i] <> ' ')
                 and (query [i] <> ',') and (query [i] <> ')') do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
        // Converting to upper case
        lex:= upcase (lex);
        if lex= 'DATETIME' then Result [length (Result)- 1].DataType:= cdtDateTime;
        if lex= 'TIME' then Result [length (Result)- 1].DataType:= cdtTime;
        if lex= 'DATE' then Result [length (Result)- 1].DataType:= cdtDate;
        if (lex= 'REAL') or (lex= 'DOUBLE') then
          Result [length (Result)- 1].DataType:= cdtReal;
        if (lex= 'INT') or (lex= 'INTEGER') then
          Result [length (Result)- 1].DataType:= cdtInt;
        if (lex= 'VARCHAR') or (lex= 'TEXT') then
          Result [length (Result)- 1].DataType:= cdtVarchar;
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Skipping everything till the colon or brace as we don't support anything else
        while (i <= len) and (query [i] <> ',') and (query [i] <> ')') do Inc (i);
        // Skipping colon
        if (i <= len) and (query [i]= ',') then Inc (i);
      end;
    end;


   {L505: DEBUG NOTE, errors when there are two text fields parsed, i.e.:

            sds_query ('INSERT INTO `sds/topics.sds` (desc, aid, name, posts) VALUES (testdesc, 2, testname, 0)');
          
          Causes an error when it shouldn't:

            SQL syntax error: ) expected after field list

    }
    // Compiles field list into array of indexes. Example:
    // 'id, posts, login'=>  [0]= 0,  [1]= 4,  [2]= 1.
    function sds_compile_fields (cols: SDS_PCols): SDS_FieldList;
    var j: LongWord;
    begin
      // Parsing while
      while (i < len) and (query [i] <> ')') do
      begin
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Checking for FROM in SELECT
        if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'FROM') then
           Break;
        // New Result element
        SetLength (Result, length (Result)+ 1);
        // Checking for ASC and DESC after ORDER BY in SELECT
        if (((i+ 3) < len) and (upcase (copy (query, i, 3))= 'ASC'))
            or (((i+ 4) < len) and (upcase (copy (query, i, 4))= 'DESC')) then
           Break;
          
        if query [i]= '`' then
        begin
          // Getting String inside of backticks
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> '`') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Getting String till space or comma or brace
          lex:= '';
          while (i <= len) and (query [i] <> ' ') and (query [i] <> ',') and (query [i] <> ')') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        // Ignoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Skipping comma
        if (i <= len) and (query [i]= ',') then Inc (i);
        // Ignoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        for j:= 0 to length (cols^)- 1 do if cols^ [j].name= lex then
          Result [length (Result)- 1]:= j;
      end;
    end;

    // Compiles value list into row. Example:
    // '10, 5.1, "a String"'=>  [0]= '10',  [1]= '5.1',  [2]= 'a String'.
    function sds_compile_values: SDS_Row;
    begin
      while (i <= len) and (query [i] <> ')') do
      begin
        // New array element
        SetLength (Result, length (Result)+ 1);
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        if (query [i]= '"') or (query [i]= '''') then
        begin
          // Getting String inside of quotes
          quot:= query [i];
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> quot) do
          begin
            if (query [i]= '\') and (query [i+ 1]= quot) then
            begin
              SetLength (lex, length (lex)+ 1);
              lex [length (lex)]:= quot;
              i:= i+ 2;
              Continue;
            end;
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Getting column till space or colon
          lex:= '';
          while (i <= len) and (query [i] <> ' ')
                  and (query [i] <> ',') and (query [i] <> ')') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        // Ignoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Skipping colon
        if (i <= len) and (query [i]= ',') then Inc (i);
        // Ignoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        Result [length (Result)- 1]:= lex;
      end;
    end;


    // Compiles String SDS/SQL condition into internal SDS representation
    // Example condition: '`id` > 10 AND `login`= "John" OR `posts` <= 5'
    function sds_compile_condition (cols: SDS_PCols): SDS_Condition;
    var
      idx, j: LongWord;
    begin
      idx:= 0;
      while i < len do
      begin
        lex:= '';
        // Spaces are ignored
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Checking for ORDER BY in SELECT
        if ((i+ 8) < len) and (upcase (copy (query, i, 8))= 'ORDER BY') then  Break;
        // Initializing new element
        SetLength (Result, idx+ 1);
        Result [idx].field:= 0;
        Result [idx].opr:= '=';
        Result [idx].Logic:= blAnd;
        // Checking for AND
        if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'AND ') then
        begin
          Result [idx].Logic:= blAnd;
          Inc (i, 4);
        end;
        // Checking for OR
        if ((i+ 3) < len) and (upcase (copy (query, i, 3))= 'OR ') then
        begin
          Result [idx].Logic:= blOr;
          Inc (i, 3);
        end;
        // Checking for XOR
        if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'XOR ') then
        begin
          Result [idx].Logic:= blXor;
          Inc (i, 4);
        end;
        // Ignoring other spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Checking for NOT
        if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'NOT ') then
        begin
          Inc (Result [idx].Logic, 3);
          Inc (i, 4);
          // Ignoring other spaces
          while (i <= len) and (query [i]= ' ') do Inc (i);
        end;
        // Getting first lexem
        if query [i]= '`' then
        begin
          // Quoted field name, get it!
          Inc (i);
          while (i <= len) and (query [i] <> '`') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Getting whole lexem till ' ' or EOLN
          while (i <= len) and (query [i] <> ' ') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        // Determine field index
        for j:= 0 to length (cols^)- 1 do
        if cols^ [j].name= lex then
        begin
          Result [idx].field:= j;
           Break;
        end;
        // Ignoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Getting the operator
        lex:= '';
        while (i <= len) and (query [i] <> ' ') do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
        Result [idx].opr:= lex;
        // Ignoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Getting the value
        lex:= '';
        if (query [i]= '"') or (query [i]= '''') then
        begin
          quot:= query [i];
          Inc (i);
          // Quoted String value
          while (i <= len) and (query [i] <> quot) do
          begin
            if (query [i]= '\') and (query [i+ 1]= quot) then
            begin
              SetLength (lex, length (lex)+ 1);
              lex [length (lex)]:= quot;
              i:= i+ 2;
              Continue;
            end;
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Unquoted value
          while (i <= len) and (query [i] <> ' ')  do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        Result [idx].value:= lex;
        // Ingnoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Increasing element index
        Inc (idx);
      end;
    end;

    // UPDATE SET compiler. Example:
    // 'id= 15, `name`= "John"'=>  [0].field= 0,  [0].value= '15';  [1].field= 3,
    //  [1].value= 'John'.
    function sds_compile_set (cols: SDS_PCols): SDS_Set;
    var j: LongWord;
    begin
      // Parsing
      while i < len do
      begin
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Checking for WHERE (limit)
        if ((i+ 5) < len) and (upcase (copy (query, i, 5))= 'WHERE') then  Break;
        // New Result element
        SetLength (Result, length (Result)+ 1);
        if query [i]= '`' then
        begin
          // Quoted field name. Getting String inside of backticks
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> '`') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space.
          lex:= '';
          while (i <= len) and (query [i] <> ' ') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        for j:= 0 to length (cols^)- 1 do if cols^ [j].name= lex then
        begin
          Result [length (Result)- 1].field:= j;
           Break;
        end;
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Then '=' should follow, skipping it or throw an error (TODO)
        if query [i] <> '=' then resp^.error:= 'SQL syntax error:= expected';
        Inc (i);
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Getting value depending on field type
        if (query [i]= '''') or (query [i]= '`') or (query [i]= '"') then
        begin
          // VARCHAR, DATE, TIME, DATETIME value (quoted)
          // Getting String inside of quotes
          quot:= query [i];
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> quot) do
          begin
            if (query [i]= '\') and (query [i+ 1]= quot) then
            begin
              SetLength (lex, length (lex)+ 1);
              lex [length (lex)]:= quot;
              i:= i+ 2;
              Continue;
            end;
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // INT/FLOAT value (plain)
          // Getting column name till space or colon
          lex:= '';
          while (i <= len) and (query [i] <> ' ') and (query [i] <> ',') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        Result [length (Result)- 1].value:= lex;
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Skipping colon
        if (i <= len) and (query [i]= ',') then Inc (i);
      end;
    end;


    // Compiles row order data. Example:
    // 'posts DESC, name ASC'=>  [0].key= 4,  [0].dir= true;  [1].key= 1,  [1].dir= false.
    function sds_compile_order (cols: SDS_PCols): SDS_Order;
    var j: LongWord;
    begin
      // Parsing
      while i < len do
      begin
        // Skipping whitespace
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Checking for LIMIT
        if ((i+ 5) < len) and (upcase (copy (query, i, 5))= 'LIMIT') then  Break;
        // New Result element
        SetLength (Result, length (Result)+ 1);
        // Getting field name
        if query [i]= '`' then
        begin
          // Quoted field name. Getting String inside of backticks
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> '`') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space.
          lex:= '';
          while (i <= len) and (query [i] <> ' ') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        // Detecting key value
        for j:= 0 to length (cols^)- 1 do if cols^ [j].name= lex then
        begin
          Result [length (Result)- 1].key:= j;
           Break;
        end;
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Default order
        Result [length (Result)- 1].dir:= false;
        // Is there any ASC or DESC?
        if ((i+ 3) < len) and (upcase (copy (query, i, 3))= 'ASC') then
        begin
          Result [length (Result)- 1].dir:= false;
          i:= i+ 3;
        end
        else if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'DESC') then
        begin
          Result [length (Result)- 1].dir:= true;
          i:= i+ 4;
        end;
        // Ingoring spaces
        while (i <= len) and (query [i]= ' ') do Inc (i);
        // Skipping colon
        if (i <= len) and (query [i]= ',') then Inc (i);
      end;
    end;

begin
    // Turning the timer on
    stamp:= Now;
    // Result init
    new (resp);
    resp^.rows:= 0;
    resp^.fields:= 0;
    resp^.point:= 0;
    resp^.time:= 0.0;
    // Determine SQL query type
    i:= 1;
    len:= length (query);
    lex:= '';
    // Reading String till ' '
    while (i < len) and (query [i] <> ' ') do
    begin
      SetLength (lex, length (lex)+ 1);
      lex [length (lex)]:= query [i];
      Inc (i);
    end;
    resp^.cmd:= upcase (lex);
    // Skipping whitespace
    while (i < len) and (query [i]= ' ') do Inc (i);
    if resp^.cmd= 'CREATE' then
    begin
      // SQL CREATE
      // Next word should be 'TABLE' as only CREATE TABLE syntax is supported
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      if upcase (lex) <> 'TABLE' then
      begin
        resp^.error:= 'SQL syntax error: TABLE expected after CREATE';
        exit (SDS_Result (resp));
      end;
      buff:= sds_compile_name;
      // Skipping whitespace and looking for brace
      while (i < len) and (query [i]= ' ') do Inc (i);
      if query [i] <> ' (' then
      begin
        resp^.error:= 'SQL syntax error: (expected after TABLE';
        exit (SDS_Result (resp));
      end;
      Inc (i);
      // Reading column name/type defination into cols
      cols:= sds_compile_types;
      resp^.cs:= cols;
      // Performing query
      flag:= sds_create (buff, @cols);
      if flag then resp^.rows:= 1 else resp^.rows:= 0;
    end;
    if resp^.cmd= 'DROP' then
    begin
      // SQL DROP
      // Next word should be 'TABLE' as only DROP TABLE syntax is supported
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      if upcase (lex) <> 'TABLE' then
      begin
        resp^.error:= 'SQL syntax error: TABLE expected after DROP';
        exit (SDS_Result (resp));
      end;
      buff:= sds_compile_name;
      // Performing query
      // Checking if exists
      if not file_exists (buff) then
      begin
        resp^.error:= 'File does not exist: '+ buff;
        exit (SDS_Result (resp));
      end;
      flag:= sds_drop (buff);
      if flag then resp^.rows:= 1 else resp^.rows:= 0;
    end;
    if resp^.cmd= 'INSERT' then
    begin
        // SQL INSERT
        // Next word should be 'INTO' as only INSERT INTO syntax is supported
        lex:= '';
        while (i < len) and (query [i] <> ' ') do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
        // Skipping whitespace
        while (i < len) and (query [i]= ' ') do Inc (i);
        if upcase (lex) <> 'INTO' then
        begin
          resp^.error:= 'SQL syntax error: INTO expected after INSERT';
          exit (SDS_Result (resp));
        end;
        buff:= sds_compile_name;
        // Checking if exists
        if not file_exists (buff) then
        begin
          resp^.error:= 'File does not exist: '+ buff;
          exit (SDS_Result (resp));
        end;
        // Reading table data
        sds_read_info (buff, hdr, cols);
        // Skipping whitespace and looking for brace
        while (i < len) and (query [i]= ' ') do Inc (i);
        if query [i] <> ' (' then
        begin
          resp^.error:= 'SQL syntax error: (expected after '+ buff;
          exit (SDS_Result (resp));
        end;
        Inc (i);
        // Getting field list
        fields:= sds_compile_fields (@cols);
        // Closing brace
        if query [i] <> ')' then
        begin
          resp^.error:= 'SQL syntax error: ) expected after field list';
          exit (SDS_Result (resp));
        end;
        Inc (i);
        // Skipping whitespace
        while (i < len) and (query [i]= ' ') do Inc (i);
        // Next word should be 'VALUES'
        lex:= '';
        while (i < len) and (query [i] <> ' ') do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
        if upcase (lex) <> 'VALUES' then
        begin
          resp^.error:= 'SQL syntax error: VALUES expected after the field list';
          exit (SDS_Result (resp));
        end;
        // Skipping whitespace and looking for brace
        while (i < len) and (query [i]= ' ') do Inc (i);
        if query [i] <> ' (' then
        begin
          resp^.error:= 'SQL syntax error: (expected after VALUES';
          exit (SDS_Result (resp));
        end;
        Inc (i);
        // Getting values
        values:= sds_compile_values;
        // Closing brace
        if query [i] <> ')' then
        begin
          resp^.error:= 'SQL syntax error: ) expected after value list';
          exit (SDS_Result (resp));
        end;
        Inc (i);
        // Performing query
        resp^.rows:= sds_insert (buff, @fields, @values);
    end;
    if resp^.cmd= 'DELETE' then
    begin
      // SQL DELETE
      // Next word should be 'FROM' as only DELETE FROM syntax is supported
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      if upcase (lex) <> 'FROM' then
      begin
        resp^.error:= 'SQL syntax error: FROM expected after DELETE';
        exit (SDS_Result (resp));
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Getting name
      buff:= sds_compile_name;
      // Checking if exists
      if not file_exists (buff) then
      begin
        resp^.error:= 'File does not exist: '+ buff;
        exit (SDS_Result (resp));
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Next word should be 'WHERE'
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      if upcase (lex) <> 'WHERE' then
      begin
        resp^.error:= 'SQL syntax error: WHERE expected after '+ buff;
        exit (SDS_Result (resp));
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Retriving table info
      sds_read_info (buff, hdr, cols);
      // Reading and compiling search condition
      cond:= sds_compile_condition (@cols);
      // Performing query
      resp^.rows:= sds_delete (buff, @cond);
    end;
    if resp^.cmd= 'UPDATE' then
    begin
      // SQL UPDATE
      lex:= '';
      // Reading table name
      buff:= sds_compile_name;
      // Checking if exists
      if not file_exists (buff) then
      begin
        resp^.error:= 'File does not exist: '+ buff;
        exit (SDS_Result (resp));
      end;
      // Fetching info
      sds_read_info (buff, hdr, cols);
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Next word should be 'SET'
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      if upcase (lex) <> 'SET' then
      begin
        resp^.error:= 'SQL syntax error: SET expected after '+ buff;
        exit (SDS_Result (resp));
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Parsing SET data
      updset:= sds_compile_set (@cols);
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Next word should be 'WHERE'
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      if upcase (lex) <> 'WHERE' then
      begin
        resp^.error:= 'SQL syntax error: WHERE expected after the UPDATE name= value data';
        exit (SDS_Result (resp));
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Reading condition
      cond:= sds_compile_condition (@cols);
      // Performing query
      resp^.rows:= sds_update (buff, @updset, @cond);
    end;
    if resp^.cmd= 'SELECT' then
    begin
      // SQL SELECT
      // SELECT, SELECT COUNT (*) or SELECT ALL
      bflag:= 0;
      if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'MIN (') then
      begin
        bflag:= 5;
        i:= i+ 4;
      end
      else if ((i+ 4) < len) and (upcase (copy (query, i, 4))= 'MAX (') then
      begin
        bflag:= 4;
        i:= i+ 4;
      end
      else if ((i+ 8) < len) and (upcase (copy (query, i, 8))= 'COUNT (*)') then
      begin
        bflag:= 3;
        i:= i+ 8;
      end
      else if ((i+ 3) < len) and (upcase (copy (query, i, 3))= 'ALL') then
      begin
        bflag:= 2;
        i:= i+ 3
      end
      else if (i < len) and (query [i]= '*') then
      begin
        bflag:= 2;
        Inc (i);
      end
      else if bflag= 0 then bflag:= 1;
      
      if bflag in [1, 4, 5] then
      begin
        // Getting field list
        // We need table name before we can extract fields
        j:= i;
        while (j < len) and (lex <> 'FROM') do
        begin
          lex:= '';
          // Ignoring whitespace
          while (j < len) and (query [j]= ' ') do Inc (j);
          // Reading data till ' '
          while (j < len) and (query [j] <> ' ') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [j];
            Inc (j);
          end;
        end;
        while (j < len) and (query [j]= ' ') do Inc (j);
        lex:= '';
        if (query [j]= '''') or (query [j]= '"') or (query [j]= '`') then
        begin
          // Quoted filename
          quot:= query [j];
          Inc (j);
          while (j <= len) and (query [j] <> quot) do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [j];
            Inc (j);
          end;
          Inc (j);
        end
        else
        begin
          // Reading till whitespace
          while (j <= len) and (query [j] <> ' ') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [j];
            Inc (j);
          end;
        end;
        buff:= lex;
        // Checking if exists
        if not file_exists (buff) then
        begin
          resp^.error:= 'File does not exist: '+ buff;
          exit (SDS_Result (resp));
        end;
        // Fetching table info
        sds_read_info (buff, hdr, cols);
        resp^.fields:= hdr.fields;
        resp^.cs:= cols;
      end;
      if bflag= 1 then
      begin
        // Reading field list
        fields:= sds_compile_fields (@cols);
      end;
      if (bflag= 4) or (bflag= 5) then
      begin
        // Getting name of the key field for MIN/MAX ()
        if query [i]= '`' then
        begin
          // Quoted field name. Getting String inside of backticks
          Inc (i);
          lex:= '';
          while (i <= len) and (query [i] <> '`') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
          Inc (i);
        end
        else
        begin
          // Unquoted field name. Getting column name till space.
          lex:= '';
          while (i <= len) and (query [i] <> ' ') and (query [i] <> ')') do
          begin
            SetLength (lex, length (lex)+ 1);
            lex [length (lex)]:= query [i];
            Inc (i);
          end;
        end;
        // Detecting key value
        for j:= 0 to length (cols)- 1 do if cols [j].name= lex then
        begin
          SetLength (fields, 1);
          fields [0]:= j;
           Break;
        end;
        if query [i] <> ')' then
        begin
          resp^.error:= 'SQL syntax error: ) expected after '+ lex;
          exit (SDS_Result (resp));
        end;
        Inc (i);
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Next word should be 'FROM'
      lex:= '';
      while (i < len) and (query [i] <> ' ') do
      begin
        SetLength (lex, length (lex)+ 1);
        lex [length (lex)]:= query [i];
        Inc (i);
      end;
      if upcase (lex) <> 'FROM' then
      begin
        resp^.error:= 'SQL syntax error: FROM expected after the field list';
        exit (SDS_Result (resp));
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Getting table name
      buff:= sds_compile_name;
      // Checking if exists
      if not file_exists (buff) then
      begin
        resp^.error:= 'File does not exist: '+ buff;
        exit (SDS_Result (resp));
      end;
      sds_read_info (buff, hdr, cols);
      resp^.cs:= cols;
      resp^.fields:= hdr.fields;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Next word should be 'WHERE' if it is not whole table select
      if i < len then
      begin
        lex:= '';
        while (i < len) and (query [i] <> ' ') do
        begin
          SetLength (lex, length (lex)+ 1);
          lex [length (lex)]:= query [i];
          Inc (i);
        end;
        if upcase (lex) <> 'WHERE' then
        begin
          resp^.error:= 'SQL syntax error: WHERE expected after '+ buff;
          exit (SDS_Result (resp));
        end;
        // Skipping whitespace
        while (i < len) and (query [i]= ' ') do Inc (i);
        // Getting search condition
        cond:= sds_compile_condition (@cols);
        // Skipping whitespace
        while (i < len) and (query [i]= ' ') do Inc (i);
      end;
      // Is there any ORDER BY?
      if ((i+ 8) < len) and (upcase (copy (query, i, 8))= 'ORDER BY') then
      begin
        i:= i+ 8;
        order:= sds_compile_order (@cols);
      end;
      // Skipping whitespace
      while (i < len) and (query [i]= ' ') do Inc (i);
      // Is there any LIMIT?
      if ((i+ 5) < len) and (upcase (copy (query, i, 5))= 'LIMIT') then
      begin
        i:= i+ 5;
        values:= sds_compile_values;
      end;
      // Perforiming query
      case bflag of
          1: sds_select (buff, @fields, @cond, @order, @values, resp);
          2: sds_select_all (buff, @cond, @order, @values, resp);
          3: sds_select_count (buff, @cond, @values, resp);
          4: sds_select_max (buff, @fields, @cond, @values, resp);
          5: sds_select_min (buff, @fields, @cond, @values, resp);
      end;
    end;
    // Getting query time
    resp^.time:= sds_stamp_span (stamp, Now);
    // Converting returned value
    Result:= SDS_Result (resp);
end;


// Returns type of the command which caused the Result
function sds_Result_cmd (rp: SDS_Result): String;
var 
  res: SDS_PRowSet;
begin
    res:= SDS_PRowSet (rp);
    Result:= res^.cmd;
end;


// True if the Result pointer is at the end of rowset
function sds_Result_eof (rp: SDS_Result): Boolean;
var
  res: SDS_PRowSet;
begin
    res:= SDS_PRowSet (rp);
    if res^.point >= res^.rows then Result:= true else Result:= false;
end;


// Returns last error message if error was caused by the query
function sds_Result_error (rp: SDS_Result): String;
var 
  res: SDS_PRowSet;
begin
    res:= SDS_PRowSet (rp);
    Result:= res^.error;
end;


// Returns number of field per row in the Result
function sds_Result_fields (rp: SDS_Result): LongWord;
var 
  res: SDS_PRowSet;
begin
    res:= SDS_PRowSet (rp);
    Result:= res^.fields;
end;


// Returns current row pointer position in the Result
function sds_Result_pointer (rp: SDS_Result): LongWord;
var res: SDS_PRowSet;
begin
    res:= SDS_PRowSet (rp);
    Result:= res^.point;
end;


// Rewinds the Result pointer
procedure sds_Result_rewind (rp: SDS_Result);
var res: SDS_PRowSet;
begin
    res:= SDS_PRowSet (rp);
    res^.point:= 0;
end;


// Returns number of rows in the Result
function sds_Result_rows (rp: SDS_Result): LongWord;
var
  res: SDS_PRowSet;
begin
  res:= SDS_PRowSet (rp);
  Result:= res^.rows;
end;


// Seeks the row pointer position in the Result
procedure sds_Result_seek (rp: SDS_Result; index: LongWord);
var
  res: SDS_PRowSet;
begin
  res:= SDS_PRowSet (rp);
  res^.point:= index;
end;


// Returns number of seconds passed during query performance
function sds_Result_time (rp: SDS_Result):  Double;
var
  res: SDS_PRowSet;
begin
  res:= SDS_PRowSet (rp);
  Result:= res^.time;
end;


// Fetches row as SDS_Block object
function sds_fetch_row (rp: SDS_Result): SDS_Array;
var
  res: SDS_PRowSet;
  row: SDS_PBlock;
begin
  res:= SDS_PRowSet (rp);
  if res^.point= res^.rows then exit (nil);
  new (row);
  row^.row:= res^.rs [res^.point];
  row^.cols:= @ (res^.cs);
  Inc (res^.point);
  Result:= SDS_Array (row);
end;


// Frees the Result
procedure sds_free_Result (rp: SDS_Result);
var
  res: SDS_PRowSet;
  i: LongWord;
begin
  if rp= nil then exit;
  res:= SDS_PRowSet (rp);
  if (res^.rows > 0) and (res^.cmd= 'SELECT') and (length (res^.rs) > 0) then
  begin
    for i:= 0 to length (res^.rs)- 1 do dispose (res^.rs [i]);
    SetLength (res^.rs, 0);
  end;
    dispose (res);
end;


// Fetches column specified by index as String
function sds_fetch_column (ap: SDS_Array; index: LongWord): String;
var 
  row: SDS_PBlock;
begin
  row:= SDS_PBlock (ap);
  if index >= length (row^.row^) then exit ('');
  Result:= row^.row^ [index];
end;


// Fetches column specified by index as LongInt
function sds_fetch_column_int (ap: SDS_Array; index: LongWord): LongInt;
var 
  row: SDS_PBlock;
begin
  row:= SDS_PBlock (ap);
  val (row^.row^ [index], Result);
end;


// Fetches column specified by index as  Double
function sds_fetch_column_float (ap: SDS_Array; index: LongWord):  Double;
var 
  row: SDS_PBlock;
begin
  row:= SDS_PBlock (ap);
  val (row^.row^ [index], Result);
end;


// Fetches field specified by name as String
function sds_fetch_field (ap: SDS_Array; const name: String): String;
var 
  row: SDS_PBlock;
  i: LongWord;
begin
  row:= SDS_PBlock (ap);
  Result:= '';
  if length (row^.cols^) > 0 then
  for i:= 0 to length (row^.cols^)- 1 do if row^.cols^ [i].name= name then exit (row^.row^ [i]);
end;


// Fetches field specified by name as LongInt
function sds_fetch_field_int (ap: SDS_Array; const name: String): LongInt;
begin
  val (sds_fetch_field (ap, name), Result);
end;


// Fetches field specified by name as  Double
function sds_fetch_field_float (ap: SDS_Array; const name: String):  Double;
begin
  val (sds_fetch_field (ap, name), Result);
end;


// Frees memory occupied by row
procedure sds_free_row (ap: SDS_Array);
var
  row: SDS_PBlock;
begin
  if ap= nil then exit;
  row:= SDS_PBlock (ap);
  dispose (row);
end;


// Imports SDS2/SQL dump and returns number of queries successfully run
function sds_import_sql (const dump: String): LongWord;
var
  res: SDS_Result;
  sql: String;
  i, len: LongWord;
begin
    // Initialization
    Result:= 0;
    i:= 1;
    len:= length (dump);
    while i < len do
    begin
      sql:= '';
      // Skipping spaces, tabs and new lines
      while (i < len) and ((dump [i]= ' ') or (dump [i]= #13)
               or (dump [i]= #10) or (dump [i]= #9)) do
        Inc (i);
      // Skip the comment
      if ((i+ 1) < len) and (dump [i]= '/') and (dump [i+ 1]= '*') then
      begin
        Inc (i, 2);
        repeat
            Inc (i);
        until (dump [i]= '*') and (dump [i+ 1]= '/');
        Inc (i, 2);
      end;
      // Skipping spaces, tabs and new lines
      while (i < len) and ((dump [i]= ' ') or (dump [i]= #13) or (dump [i]= #10) or (dump [i]= #9)) do Inc (i);
      // Getting SQL command till the semicolon
      if dump [i]= '/' then Continue;
      while i < len do
      begin
        if (dump [i]= ';') and (((i+ 1)= len) or ((dump [i+ 1]= #13) and (dump [i+ 2]= #10))) then
        begin
          Inc (i, 3);
           Break;
        end;
        SetLength (sql, length (sql)+ 1);
        sql [length (sql)]:= dump [i];
        Inc (i);
      end;
      res:= sds_query (sql);
      if sds_Result_error (res)= '' then
        Inc (Result)
      else
        writeln (sds_Result_error (res));
      sds_free_Result (res);
      // Skipping spaces, tabs and new lines
      while (i < len) and ((dump [i]= ' ') or (dump [i]= #13) or (dump [i]= #10) or (dump [i]= #9)) do Inc (i);
    end;
end;



{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

{ TSDSWrapper }

constructor TSDSWrapper.Create;
begin
  inherited;
end;

procedure TSDSWrapper.Free;
begin
  inherited;
end;

procedure TSDSWrapper.Column_Free (cp: SDS_ColumnInfo);
begin
  sds_column_free (cp);
end;

function TSDSWrapper.Column_Index (cp: SDS_ColumnInfo; const Name: String): LongWord;
begin
  Result:= sds_column_index (cp, Name);
end;

function TSDSWrapper.Column_Info (const from: String): SDS_ColumnInfo;
begin
  Result:= sds_column_info (from);
end;

function TSDSWrapper.Column_Name (cp: SDS_ColumnInfo; Index: LongWord): String;
begin
  Result:= sds_column_name (cp, Index);
end;

function TSDSWrapper.Column_Type (cp: SDS_ColumnInfo; Index: LongWord
  ): TColumnDataType;
begin
  Result:= sds_Column_Type (cp, Index);
end;

function TSDSWrapper.Escape (const Str: String): String;
begin
  Result:= sds_escape (Str);
end;

function TSDSWrapper.Export_Csv (const From: String): String;
begin
  Result:= sds_export_csv (From);
end;

function TSDSWrapper.Export_SDS (const From: String): String;
begin
  Result:= sds_export_sds (From);
end;

function TSDSWrapper.Export_SQL (const From, SQLTable: String): String;
begin
  Result:= sds_export_sql (From, SQLTable);
end;

function TSDSWrapper.Export_XML (const From, TableTag, RowTag: String): String;
begin
  Result:= sds_export_xml (From, TableTag, RowTag);
end;

function TSDSWrapper.Fetch_Column (ap: SDS_Array; Index: LongWord): String;
begin
  Result:= sds_fetch_column (ap, Index);
end;

function TSDSWrapper.Fetch_Column_Float (ap: SDS_Array; Index: LongWord):  Double;
begin
  Result:= sds_fetch_column_float (ap, Index);
end;

function TSDSWrapper.Fetch_Column_Int (ap: SDS_Array; index: LongWord): LongInt;
begin
  Result:= sds_fetch_column_int (ap, index);
end;

function TSDSWrapper.Fetch_Field (ap: SDS_Array; const name: String): String;
begin
  Result:= sds_fetch_field (ap, Name);
end;

function TSDSWrapper.Fetch_Field_float (ap: SDS_Array; const Name: String
  ):  Double;
begin
  Result:= sds_fetch_field_float (ap, Name);

end;

function TSDSWrapper.Fetch_Field_int (ap: SDS_Array; const Name: String
  ): LongInt;
begin
  Result:= sds_fetch_field_int (ap, Name);
end;

function TSDSWrapper.Fetch_Row (rp: SDS_Result): SDS_Array;
begin
  Result:= sds_fetch_row (rp);
end;

procedure TSDSWrapper.Free_Result (rp: SDS_Result);
begin
  sds_free_Result (rp);
end;

procedure TSDSWrapper.Free_Row (ap: SDS_Array);
begin
  sds_free_row (ap);
end;

function TSDSWrapper.Import_SQL (const dump: String): LongWord;
begin
  Result:= sds_import_sql (dump);
end;

function TSDSWrapper.Last_ID (const from: String): LongWord;
begin
  Result:= sds_last_id (from);
end;

function TSDSWrapper.Num_Fields (const from: String): LongWord;
begin
  Result:= sds_num_fields (from);
end;

function TSDSWrapper.Num_Rows (const from: String): LongWord;
begin
  Result:= sds_num_rows (from);
end;

function TSDSWrapper.Result_CMD (rp: SDS_Result): String;
begin
  Result:= sds_Result_cmd (rp);
end;

function TSDSWrapper.Result_EOF (rp: SDS_Result): Boolean;
begin
  Result:= sds_Result_eof (rp);
end;

function TSDSWrapper.Result_Error (rp: SDS_Result): String;
begin
  Result:= sds_Result_error (rp);
end;

function TSDSWrapper.Result_Fields (rp: SDS_Result): LongWord;
begin
  Result:= sds_Result_fields (rp);
end;

function TSDSWrapper.Result_Pointer (rp: SDS_Result): LongWord;
begin
  Result:= sds_Result_pointer (rp);
end;

procedure TSDSWrapper.Result_Rewind (rp: SDS_Result);
begin
  sds_Result_rewind (rp);
end;

function TSDSWrapper.Result_Rows (rp: SDS_Result): LongWord;
begin
  Result:= sds_Result_rows (rp);
end;

procedure TSDSWrapper.Result_Seek (rp: SDS_Result; Index: LongWord);
begin
  sds_Result_seek (rp, Index);
end;

function TSDSWrapper.Result_Time (rp: SDS_Result):  Double;
begin
  Result:= sds_Result_time (rp);
end;

function TSDSWrapper.Total_Fields (const From: String): LongWord;
begin
  Result:= sds_total_fields  (From);
end;

function TSDSWrapper.Query  (const QueryString: String): SDS_Result;
begin
  Result:= sds_query  (QueryString);
end;

end.
