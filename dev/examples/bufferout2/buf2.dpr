program buf2; {$ifdef fpc}{$mode objfpc}{$h+}{$UNITPATH ../../main/}{$endif}{$APPTYPE console}
uses pwmain, pwinit, strwrap1;  

var buf: TByteArray;
const FNAME = 'smile.png';

begin
  if file2buf(FNAME, buf) < 0 then begin
    outln('could not read :' + FNAME);
    exit;
  end;
  setHeader('Content-Type', 'image/png');
  bufferOut(pointer(buf)^, length(buf));
end.  
