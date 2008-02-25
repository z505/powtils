Program HelloWorld;

{$DEFINE STATIC}

uses 
{$IFNDEF DYNWEB}pwumain,{$ELSE}dynpwu,{$ENDIF}
 webcrt;    // Heres the "MAGIC" that reprograms Writeln;

Var
  I : Integer;

Begin

 // IMPORTANT NOTE HERE:
 //    We don't have to worry about webheaders and such because
 //    pwu takes care of that just as if we had used WebWrite :) 

  Write('<HTML');
  Writeln('<BODY bgcolor="black">');   
  Write('<CENTER>');
  For I := 1 to 8 do Write('<BR>');
  Write('<H1>');
  Write('<FONT COLOR="white">');
  Write('HELLO WORLD');
  Writeln;
  Writeln('</BODY></HTML>');
end.

  
