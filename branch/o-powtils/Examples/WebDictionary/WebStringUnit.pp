unit WebStringUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  { TWebString }
  {
    This class is an extension of normal String, for handling PUT and GET variable and
    web session data and etc.
  }

  TWebString= class (TObject)
  private
    FValue: String;
  public
    constructor Create (Value: String);
    constructor Create;

    function FilterHTML: TWebString;
    function FilterHTML_S (const SecureLevel: Integer): TWebString;
    function TrimBadChars: TWebString;
    function TrimBadChars_File: TWebString;
    function TrimBadChars_Dir: TWebString;
    function TrimBadChars_S    (const SecureLevel: Integer): TWebString;

    function ToString: String;
    function ToFloat: Double;
    function ToInt: Integer;
    function ToSafeHTML: TWebString;


    function Copy: TWebString;
    function ToUpperCase: TWebString;
    function IsSame (AnotherWebString: TWebString): Boolean;
    function IsSame (AnotherString: String): Boolean;

  end;

  procedure URLDecode (var Str: String);

implementation
uses
  substrings, ExceptionUnit;

procedure URLDecode (var Str: String);
begin
  raise ENotImplementedYet.Create ('URLDecode', '');

end;

{ TWebString }

constructor TWebString.Create (Value: String);
begin
  inherited Create;

  FValue:= Value;
end;

constructor TWebString.Create;
begin
  inherited;
end;


// Replaces special characters with their HTML equivalents
// If you are taking input on a guestook or forum for example, you will want to
// use FilterHTML
//
// Default security level: 2
function TWebString.FilterHTML: TWebString;
begin

  Result:= Self.FilterHTML_S (2);
end;

// Powers the FilterHTML function, here with ability to define security level
//
// Security level 1:
//   Replaces special characters with their HTML equivalents.
//   This one does not filter { or } or $ if you are for example working with
//   templates, because you need those characters.
//
// Security level 2:
//   Similar to level 1, but more filtering of malicious input variable
//   attempts. This filter replaces the special template characters, so if you
//   want your templates to come through use FilterHTML_1
function TWebString.FilterHTML_S(const SecureLevel: integer): TWebString;
begin
  Result:= Self;

  case SecureLevel of
    1:
    begin
      FValue:= substr_replace (FValue, '&', '&amp;');
      FValue:= substr_replace (FValue, '#', '&#35;');
      FValue:= substr_replace (FValue, '', '&quot;');
      FValue:= substr_replace (FValue, '''', '&#39;');  //single quote
      FValue:= substr_replace (FValue, '<', '&lt;');
      FValue:= substr_replace (FValue, '>', '&gt;');
      FValue:= substr_replace (FValue, '|', '&#124;');  //pipe
      FValue:= substr_replace (FValue, '%', '&#37;');   //percent sign
  //    FValue:= substr_replace (FValue, #0, '');        //null character okay to trim?? MUST CONFIRM
    end;
    2:
    begin
      FValue:= substr_replace (FValue, '&', '&amp;');
      FValue:= substr_replace (FValue, '#', '&#35;');   //pound sign
      FValue:= substr_replace (FValue, '"', '&quot;');  //quote
      FValue:= substr_replace (FValue, '''', '&#39;');  //single quote
      FValue:= substr_replace (FValue, '<', '&lt;');    //less than
      FValue:= substr_replace (FValue, '>', '&gt;');    //greater than
      FValue:= substr_replace (FValue, '|', '&#124;');  //pipe
      FValue:= substr_replace (FValue, '%', '&#37;');   //percent sign
  //    FValue:= substr_replace (FValue, #0, '');        //null character okay to trim?? MUST CONFIRM
      FValue:= substr_replace (FValue, '(', '&#40;');   //open bracket
      FValue:= substr_replace (FValue, ')', '&#41;');   //closed bracket
      FValue:= substr_replace (FValue, '{', '&#123;');  //open parenthesis
      FValue:= substr_replace (FValue, '}', '&#125;');  //closed parenthesis
      FValue:= substr_replace (FValue, '$', '&#36;');   //dollar sign
      FValue:= substr_replace (FValue, '?', '&#63;');   //question mark
    end;
  end;
end;

// Trims (deletes) all bad, unsecure characters from a string.
// i.e. hackers sometimes use pipe characters | or ../../ to try to hack the
// server. Mainly useful for trimming URL variables for malicious attempts.
// Note: see also FilterHTML, which replaces characters with real output such
//       as &gt; &quot;
//
// Default security level: 2
function TWebString.TrimBadChars: TWebString;
begin
  Result:= Self.TrimBadChars_S (2);
end;

// Trims (deletes) all bad, unsecure characters from a string that is being used
// for filenames. For example, if you are opening a file, you will want the
// file name in a local directory, only characters like A-Z plus dots and
// brackets, but not things like pipe characters and quotations
function TWebString.TrimBadChars_File: TWebString;
begin
//  FValue:= substr_replace (FValue,'.', '',[rfReplaceAll]);  // Dot is okay
//  FValue:= substr_replace (FValue,'~', '',[rfReplaceAll]);  // Squiggly ~ character is okay for filenames
  FValue:= substr_replace (FValue, '/', '');     // slashes NOT okay. safe means local directory only!
  FValue:= substr_replace (FValue, '\', '');    // slashes NOT okay. safe means local directory only!
  FValue:= substr_replace (FValue, '|', '');    // pipe character bad
  FValue:= substr_replace (FValue, '#', '');
  FValue:= substr_replace (FValue, '@', '');
  FValue:= substr_replace (FValue, '$', '');
  FValue:= substr_replace (FValue, '!', '');
  FValue:= substr_replace (FValue, '%', '');
  FValue:= substr_replace (FValue, '^', '');
  FValue:= substr_replace (FValue, '&', '');
  FValue:= substr_replace (FValue, '*', '');
  FValue:= substr_replace (FValue, '=', '');
  FValue:= substr_replace (FValue, '`', '');
  FValue:= substr_replace (FValue, '?', '');
  FValue:= substr_replace (FValue, '"', '');   // double quote
  FValue:= substr_replace (FValue, '''', '');  // single quote
  FValue:= substr_replace (FValue, '[', '');   // square bracket open
  FValue:= substr_replace (FValue, ']', '');   // square bracket close
  FValue:= substr_replace (FValue, '>', '');   // greater than
  FValue:= substr_replace (FValue, '<', '');   // less than
  FValue:= substr_replace (FValue, ',', '');   // comma

  Result:= Self;
end;

// Trims (deletes) all bad, unsecure characters from a string that is being used
// for a directory. For example, if you are opening a directory or file and
// directory, you will want only characters like A-Z, plus dots, slashes, and
// brackets, but not things like pipe characters and quotations
function TWebString.TrimBadChars_Dir: TWebString;
const
  ToBeSearchedCharacters: array [0..18] of char=
    ('|', '#', '@', '$', '!', '%', '^', '&', '*', '=', '`',
     '?', '"',   // double quote
     '''',  // single quote
     '[',   // square bracket open
     ']',   // square bracket close
     '>',   // greater than
     '<',   // less than
     ','    // comma
//   '.',   // Dot is okay
//   '~',   // Squiggly ~ character is okay for filenames
//   '/',   //slashes okay
//   '\'    //slashes okay

     );
var
  i: Integer;
  PCh: ^Char;
begin

  PCh:= @ToBeSearchedCharacters;

  for i:= 0 to 18 do
  begin
    FValue:= SubStr_Replace (FValue, PCh^, '');
    Inc (PCh);
  end;
  Result:= Self;
end;

// Powers the TrimBadChars function. Ability to define security level
//
// Security level 1: Trims bad (malicious) charachers
// Security level 2:  Even more are trimmed than in security level 1
function TWebString.TrimBadChars_S (const SecureLevel: integer): TWebString;
begin
  Result:= Self;

  case SecureLevel of
    1:
    begin
//    FValue:= substr_replace (FValue, #0, '');   //okay to trim null character or does this screw up ansistrings?
      FValue:= substr_replace (FValue, '/', '');   //slashes bad
      FValue:= substr_replace (FValue, '\', '');
      FValue:= substr_replace (FValue, '|', '');  //pipe character bad
      FValue:= substr_replace (FValue, '?', '');
      FValue:= substr_replace (FValue, '$', '');
      FValue:= substr_replace (FValue, '&', '');
      FValue:= substr_replace (FValue, '<', '');
      FValue:= substr_replace (FValue, '>', '');
    end;
    2:
    begin
//    FValue:= substr_replace (FValue,  #0, '');  // okay to trim null character or does this screw up ansistrings????
      FValue:= substr_replace (FValue, '/', '');   // slashes bad
      FValue:= substr_replace (FValue, '\', '');
      FValue:= substr_replace (FValue, '|', '');  // pipe character bad
      FValue:= substr_replace (FValue, '?', '');
      FValue:= substr_replace (FValue, '$', '');
      FValue:= substr_replace (FValue, '<', '');
      FValue:= substr_replace (FValue, '>', '');
      FValue:= substr_replace (FValue, '#', '');
      FValue:= substr_replace (FValue, '@', '');
      FValue:= substr_replace (FValue, '!', '');
      FValue:= substr_replace (FValue, '%', '');
      FValue:= substr_replace (FValue, '^', '');
      FValue:= substr_replace (FValue, '&', '');
      FValue:= substr_replace (FValue, '*', '');
      FValue:= substr_replace (FValue, '=', '');
      FValue:= substr_replace (FValue, '~', '');
      FValue:= substr_replace (FValue, '{', '');
      FValue:= substr_replace (FValue, '}', '');
      FValue:= substr_replace (FValue, '(', '');
      FValue:= substr_replace (FValue, ')', '');
      FValue:= substr_replace (FValue, '[', '');
      FValue:= substr_replace (FValue, ']', '');
      FValue:= substr_replace (FValue, '`', '');   // backquote
      FValue:= substr_replace (FValue, '"', '');   // double quote
      FValue:= substr_replace (FValue, '''', '');  // single quote
    end;
  end;

end;

function TWebString.ToString: String;
begin
  Result:= FValue;
end;

// Returns value of variable as double precision float
// todo: implement security levels? is it needed?
function TWebString.ToFloat: Double;
begin
  Result:= StrToFloat (FValue);
end;

// Returns value of variable as integer
// todo: implement security levels? is it needed?
function TWebString.ToInt: Integer;
begin
  Result:= StrToInt (FValue);
end;

function TWebString.ToSafeHTML: TWebString;
begin
  Result:= FilterHTML_S (2);
end;

function TWebString.Copy: TWebString;
begin
  Result:= TWebString.Create (FValue);
end;

function TWebString.ToUpperCase: TWebString;
begin
  UpperCase (FValue);
  Result:= Self;
end;

function TWebString.IsSame (AnotherWebString: TWebString): Boolean;
begin
  Result:= AnotherWebString.FValue= Self.FValue;
end;

function TWebString.IsSame(AnotherString: String): Boolean;
begin
  Result:= AnotherString= Self.FValue;;
end;

end.

