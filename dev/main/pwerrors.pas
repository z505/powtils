unit pwerrors; {$ifdef fpc}{$mode objfpc}{$h+}{$endif}

interface
uses 
  pwtypes;
  
type
  errcode = word; // incase we have more than 255 errors, word allows plenty

const
  GENERAL_ERR = 0;
  OK = 1;  
  FILE_READ_ERR = 2; // file not found or can't open
  CFG_PARSE_ERR = 3;

const
  CANT_READ_CFG_FILE = '1A: can''t read cfg file';
  MISSING_INIT_CALL_OR_UNIT = 'missing Init() call or unit';

const
  // lower case string constants
  L_OUTPUT_BUFFERING   = 'output_buffering';
  L_OUTPUT_COMPRESSION = 'output_compression';
  L_HEADER_CHARSET     = 'header_charset';
  L_ERROR_REPORTING    = 'error_reporting';
  L_ERROR_HALT         = 'error_halt';
  L_UPLOAD_MAX_SIZE    = 'upload_max_size';
  L_SESSION_PATH       = 'session_path';
  L_SESSION_LIFE_TIME  = 'session_life_time';

  // upper case string constants
  U_HEADERS_SENT = 'HEADERS_SENT';
  U_ERRORS = 'ERRORS';
  U_FALSE = 'FALSE';

function errtostr(e: errcode): astr;

implementation  

function errtostr(e: errcode): astr;
begin
  case e of
    GENERAL_ERR: result:= 'general err';
    OK: result:= 'ok';
    FILE_READ_ERR: result:= 'file read err';
    CFG_PARSE_ERR: result:= 'cfg parse err';
  end;
end;


end.
  
  
