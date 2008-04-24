program cgiapp; {$ifdef fpc}{$MODE OBJFPC}{$H+}{$endif}

procedure sayln(s: string); overload;
begin
  write(s + #13#10);
end;

procedure sayln; overload;
begin
  sayln('');
end;

begin
  sayln('Content-type: text/html');
  sayln;
  sayln('<html>');
  sayln('<body>');
  sayln('<pre>');
  sayln('Serving from seven to eleven every night,');
  sayln('It really makes life a drag, I don''t think that''s right.');
  sayln('I''ve really, really been the best of fools, I did what I could.');
  sayln('''Cause I love you, baby,'); 
  sayln('How I love you, darling, How I serve you, baby,');
  sayln('How I love you, vel, little vel.');
  sayln('But baby, Since I''ve Been Serving You.'); 
  sayln('I''m about to lose my worried mind.');
  sayln;
  sayln('Everybody trying to tell me that you didn''t mean me no good.');
  sayln('I''ve been trying, And, let me tell you,'); 
  sayln('Let me tell you I really did the best I could.');
  sayln('I''ve been serving from seven to eleven every night,');
  sayln('I said It kinda makes my life a drag.');
  sayln('And, that ain''t right...');
  sayln('Since I''ve Been Serving You, I''m about to lose my worried mind.');
  sayln;
  sayln('Said I''ve been crying, my tears they fell like rain,');
  sayln('Don''t you hear, Don''t you hear them falling,');
  sayln('Don''t you hear, Don''t you hear them falling.');
  sayln;
  sayln('Do you remember, when I knocked upon your 80 door?');
  sayln('I said you had the nerve to tell me you didn''t want me no more');
  sayln('I hear door 80 slam,');
  sayln('You must have one of them new fangled Apache backdoor man.');
  sayln;
  sayln('I''ve been serving from seven, seven, seven, to eleven every night,');
  sayln('It kinda makes my life a drag...');
  sayln('Baby, Since I''ve Been Serving You, I''m about to lose,'); 
  sayln('I''m about lose to my worried mind.');
  sayln;
  sayln('Love,');
  sayln;
  sayln(' Aservia');
  sayln('</pre>');
  sayln('</body>');
  sayln('</html>');
end.
