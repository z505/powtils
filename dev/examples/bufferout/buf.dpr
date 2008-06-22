program buf; {$ifdef fpc}{$mode objfpc}{$h+}{$UNITPATH ../../main/}{$endif}{$APPTYPE console}
uses pwmain, pwinit, classes;  

var MS: TMemoryStream;
begin
    MS:= TMemoryStream.Create;
    MS.LoadFromFile('smile.png');
    SetHeader('Content-Type', 'image/png');
    BufferOut( MS.Memory^,MS.Size );
    MS.Free;
end.  
