program buf2; {$ifdef fpc}{$mode objfpc}{$h+}{$UNITPATH ../../main/}{$endif}{$APPTYPE console}
uses pwinit, pwmain, pwtypes, pwfileutil;  

var buf: TByteRay;
const FNAME = 'smile.png';

begin
  // convert file to binary bytes 
  if file2bytes(FNAME, buf) < 0 then begin
    outln('File error: ' + FNAME);
    exit;
  end;
  setHeader('Content-Type', 'image/png');
  // output binary bytes
  bytesOut(buf);
end.  
