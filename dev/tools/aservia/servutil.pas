{ Utilities for aservia
  By Lars Olson
  License: see aservia license in docs/License.html }

unit servutil; {$ifdef fpc}{$mode objfpc}{$h+}{$endif}

interface
uses pwtypes, pwstrutil;

const LOG_ON = false;
      MESSAGES_ON = false;   
      DEBUG_ON = false;   


procedure Errln(const msg: astr); 
procedure Errln; 

{$ifdef dbug} { debugging to console }
 procedure Dbugln(s: astr);   
 procedure Dbugln(s1,s2:astr);
{$endif}

procedure Logln(var t:text; s: astr); 
procedure Logln(var t: text); 

{ write a line to console if messages on }
procedure Msgln(s1: astr);    
procedure Msgln(s1,s2: astr);    
procedure Msgln(s1,s2,s3: astr);  
procedure Msgln(s1:astr; i:int32); 

{ write a note even if MESSAGES_ON is off }
procedure Noteln(s1: astr);        
procedure Noteln(s1,s2: astr);     
procedure Noteln(s1,s2,s3: astr);  
procedure Noteln(s1,s2,s3,s4: astr); 


{------------------------------------------------------------------------------}
                             implementation
{------------------------------------------------------------------------------}


procedure Errln(const msg: astr); begin Writeln('E: ', msg); end;

procedure Errln; begin Errln(''); end;


{$ifdef dbug} { debugging to console }
 procedure Dbugln(s: astr);   begin if DEBUG_ON then writeln('DEBUG: ', s); end;
 procedure Dbugln(s1,s2:astr);begin dbugln(s1+s2); end;
{$endif}

procedure Logln(var t:text; s: astr); begin if LOG_ON then writeln(t, s); end;
procedure Logln(var t: text); begin logln(t, ''); end;

{ write a line to console if messages on }
procedure Msgln(s1: astr);          begin if MESSAGES_ON then writeln(s1); end;
procedure Msgln(s1,s2: astr);       begin msgln(s1+s2); end;
procedure Msgln(s1,s2,s3: astr);    begin msgln(s1+s2+s3); end;
procedure Msgln(s1:astr; i:int32);  begin msgln(s1+i2s(i)); end;

{ write a note even if MESSAGES_ON is off }
procedure Noteln(s1: astr);            begin  writeln(s1); end;
procedure Noteln(s1,s2: astr);         begin  writeln(s1,s2); end;
procedure Noteln(s1,s2,s3: astr);      begin  writeln(s1,s2,s3); end;
procedure Noteln(s1,s2,s3,s4: astr);   begin  writeln(s1,s2,s3,s4); end;


{------------------------------------------------------------------------------}
                                     end.
{------------------------------------------------------------------------------}
