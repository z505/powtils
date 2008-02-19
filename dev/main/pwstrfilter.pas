{ General Purpose String Filtering and Validation functions. 

  This unit offers some major filter/validation functions that  I think may 
  replace of some ugly regular expressions that people use. If you need a 
  function that isn't in this unit for validating an HTML form, study how these
  functions are designed and build one that is similar. 

  Trying to cover every single possibility in the world would be impossible,
  because people need to validate all sorts of different text. This is why
  many folks use regular expressions - however I suggest that you write 
  functions like the ones below instead of using regular expressions, wherever 
  possible. It keeps code neater and you don't have to haul in a huge regular 
  expression parser unit. Plus, you learn a lot more - regular expressions are
  high level lazy tools - parsers are low level and are *better* in the end.


What I didn't include
-----------------------

  It is very hard to make a general purpose IsPhoneNumber function unless
  you know the exact country people are from - as some phone numbers allow
  plus signs and are shorter than certain countries. You'd pretty much have
  to let in numbers, dashes, brackets, and plus signs and it still wouldn't
  validate the phone number 100 percent since there are so many variations
  depending on the country. 
  
  However, you can use IsNumDash or IsNumDashBrackPlus or IsNumDashBrack for 
  general checking to make sure someone hasn't given you a dead wrong phone 
  number, and then you can do more custom checking per country if you really 
  need their phone number 100 percent perfect. Or you could give people 
  3-4 separate fields and just use IsNum on each field, disallowing any 
  incoming brackets/dashes at all.


  Author
    Lars (L505)
    http://z505.com
    
  Todo: 
    -since string[i] may be slower than pchar casting, haven't
    tested yet or done any premature optimization.

}

unit strfilter; {$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface

type
  TCharSet = set of char;


function ValidChars(s: string; CharSet: TCharSet): boolean;

function IsEmail(s: string): boolean;

function IsAlphNum(s: string): boolean;
function IsAlphNumSpace(s: string): boolean;
function IsAlphNumSpaceUscor(s: string): boolean;
function IsAlphNumSpaceDashUscor(s: string): boolean;
function IsAlphNumDashUscor(s: string): boolean;
function IsAlphNumUscor(s: string): boolean;
function IsAlphNumDash(s: string): boolean;

function IsNum(s: string): boolean;
function IsNumDash(s: string): boolean;
function IsNumSpace(s: string): boolean;
function IsNumDashSpace(s: string): boolean;
function IsNumSpaceDashBrack(s: string): boolean;
function IsNumDashBrackPlus(s: string): boolean; 
function IsNumDashBrack(s: string): boolean; 

function IsAlph(s: string): boolean;
function IsAlphSpaceUscor(s: string): boolean;
function IsAlphDash(s: string): boolean;
function IsAlphDashSpace(s: string): boolean;
function IsAlphUscor(s: string): boolean;
function IsAlphSpace(s: string): boolean;
function IsLowAlph(s: string): boolean;
function IsLowAlphSpace(s: string): boolean;
function IsLowAlphUscor(s: string): boolean;
function IsLowAlphSpaceUscor(s: string): boolean;
function IsUpAlph(s: string): boolean;
function IsUpAlphUscor(s: string): boolean;
function IsUpAlphSpace(s: string): boolean;
function IsUpAlphSpaceUscor(s: string): boolean;



implementation

{ checks string for characters given as a parameter }
function ValidChars(s: string; CharSet: TCharSet): boolean;
var i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in CharSet) then
      begin result:= false; exit; end;
end;  


{ checks if email address format is correct or extremely close to correct. It is
  impossible to check to see whether the email is 100 percent correct since new
  domain name extensions are being added every year (.ws, .tk, .co.uk.org).. and
  a whitelist would need to be used and be constantly updated.
  Just checks for general "within the ballpark" correctness.
  Does not accept address such as John@localhost it must be in the format
  john@domain.com or john.domain.ca etc.}
function IsEmail(s: string): boolean;
const
  DOMAIN_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '-'];
  ALPHA_NUMERIC = ['a'..'z', 'A'..'Z', '0'..'9'];
var
  i: integer;
  AtFound: byte;
  CharAfterAt: boolean;
  CharBeforeAt: boolean;
  DotsAfterAt: byte;
  
begin
  result:= false;
  if length(s) < 1 then exit;  
  CharAfterAt:= false;
  CharBeforeAt:= false;
  AtFound:= 0;
  DotsAfterAt:= 0;
  for i:= 1 to length(s) do
  begin
    if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-', '_', '@']) then
      begin result:= false; exit; end;
    // must be at least one or more characters and/or numbers before @
    if s[i] = '@' then
    begin
      inc(AtFound);
      // two '@'s bad
      if AtFound > 1 then begin result:= false; exit; end;
      continue; // nothing more to do in current loop round
    end;
    if (AtFound = 0) and (s[i] in ALPHA_NUMERIC) then
      CharBeforeAt:= true;
    // must find at laeast one alphanumeric before @
    if (CharBeforeAt = false) and (atfound = 1) then begin result:= false; exit; end;
    if (s[i] = '.') and (AtFound = 1) then inc(DotsAfterAt);
    // at least one dot must be found after '@'
    if AtFound = 1 then
    begin
      // can't be underscore or other special characters in domain/host name
      if (DotsAfterAt = 0) and (not (s[i] in DOMAIN_CHARS)) then begin result:= false; exit; end;
      // at least one alphanumeric must be found before first dot after '@'
      if (s[i] in ALPHA_NUMERIC) and (DotsAfterAt = 0) then CharAfterAt:= true;
    end;
    if (CharAfterAt) and (s[i] = '.') then result:= true;
  end;
end;

function IsAlphNum(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z']) then
      begin result:= false; exit; end;
end;

// alphabet, numbers, and space allowed
function IsAlphNumSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ']) then
      begin result:= false; exit; end;
end;

// alphabet, numbers, space, and underscore allowed
function IsAlphNumSpaceUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ', '_']) then
      begin result:= false; exit; end;
end;

// alphabet, numbers, space, dash, and underscore allowed
function IsAlphNumSpaceDashUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', ' ', '_', '-']) then
      begin result:= false; exit; end;
end;


// alphabet, numbers, dash, and underscore allowed
function IsAlphNumDashUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-']) then
      begin result:= false; exit; end;
end;

// alphabet, numbers, and underscore allowed
function IsAlphNumUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) then
      begin result:= false; exit; end;
end;

// alphabet, numbers, and dash allowed
function IsAlphNumDash(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '-']) then
      begin result:= false; exit; end;
end;


function IsNumDashBrackPlus(s: string): boolean; 
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9','-','(',')','+']) then
      begin result:= false; exit; end;
end;

function IsNumDashBrack(s: string): boolean; 
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9','-','(',')']) then
      begin result:= false; exit; end;
end;

// numbers only
function IsNum(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9']) then
      begin result:= false; exit; end;
end;

// numbers and dashes allowed
function IsNumDash(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', '-']) then
      begin result:= false; exit; end;
end;

// numbers and spaces allowed
function IsNumSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', ' ']) then
      begin result:= false; exit; end;
end;

// numbers, dashes, and spaces allowed
function IsNumDashSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', '-', ' ']) then
      begin result:= false; exit; end;
end;

// numbers, dashes, brackets, and spaces allowed
function IsNumSpaceDashBrack(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['0'..'9', '-', ' ', '(', ')']) then
      begin result:= false; exit; end;
end;


function IsAlph(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', 'A'..'Z']) then
      begin result:= false; exit; end;
end;

// alphabet plus underscore and space allowed
function IsAlphSpaceUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', 'A'..'Z', '_', ' ']) then
      begin result:= false; exit; end;
end;

// alphabet plus dashes allowed
function IsAlphDash(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', 'A'..'Z', '-']) then
      begin result:= false; exit; end;
end;

// alphabet plus dashes and spaces allowed
function IsAlphDashSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', 'A'..'Z', '-', ' ']) then
      begin result:= false; exit; end;
end;

// alphabet plus underscore allowed
function IsAlphUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', 'A'..'Z', '_']) then
      begin result:= false; exit; end;
end;

// alphabet plus space allowed
function IsAlphSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', 'A'..'Z', ' ']) then
      begin result:= false; exit; end;
end;

// lower case alphabet allowed
function IsLowAlph(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z']) then
      begin result:= false; exit; end;
end;

// lower case alphabet plus space allowed
function IsLowAlphSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', ' ']) then
      begin result:= false; exit; end;
end;

// lower case alphabet plus underscore allowed
function IsLowAlphUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', '_']) then
      begin result:= false; exit; end;
end;

// lower case alphabet plus space and underscore allowed
function IsLowAlphSpaceUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['a'..'z', ' ', '_']) then
      begin result:= false; exit; end;
end;

// upper case alphabet allowed
function IsUpAlph(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['A'..'Z']) then
      begin result:= false; exit; end;
end;

// upper case alphabet and underscore allowed
function IsUpAlphUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['A'..'Z', '_']) then
      begin result:= false; exit; end;
end;

// upper case alphabet and space allowed
function IsUpAlphSpace(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['A'..'Z', ' ']) then
      begin result:= false; exit; end;
end;

// upper case alphabet, and underscores and spaces allowed
function IsUpAlphSpaceUscor(s: string): boolean;
var
  i: integer;
begin
  if length(s) < 1 then begin result:= false; exit; end;
  result:= true;
  for i:= 1 to length(s) do
    if not (s[i] in ['A'..'Z', '_', ' ']) then
      begin result:= false; exit; end;
end;

end.

