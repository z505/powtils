// delphi system compat problems
{$IFNDEF FPC}
 procedure val(s: astr; var i: longint); overload;
 var dummy: integer;
 begin
   system.val(s, i, dummy);
 end;

 procedure val(s: astr; var i: longword); overload;
 var dummy: integer;
 begin
   system.val(s, i, dummy);
 end;

 procedure val(s: astr; var i: double); overload;
 var dummy: integer;
 begin
   system.val(s, i, dummy);
 end;
                            
 function BoolToStr(b:bln):astr;
 begin
   if b = true then result:='TRUE';
   if b = false then result:='FALSE';
 end;
{$ENDIF}