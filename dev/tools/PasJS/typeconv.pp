Unit TypeConv;

Interface
Uses Scanner;

Const
  Conversions :
    Array[
      Ord(tkUnknown)..Ord(tkPreCmd),
      Ord(tkUnknown)..Ord(tkPreCmd)
    ] Of Boolean =
  (
    // tkUnknown x all
    (False, False, False, False, False,
     False, False, False, False, False),
    // tkWord x all
    (False, False, False, False, False,
     False, False, False, False, False),
    // tkIdent x all
    (False, False, False, False, False,
     False, False, False, False, False),
    // tkNumber x all
    (False, False, False, True, False,
     False, True, False, False, False),
    // tkString x all
    (False, False, False, False, True,
     False, False, True, False, False),
    // tkBoolean x all
    (False, False, False, False, False,
     True, False, False, False, False),
    // tkFloat x all
    (False, False, False, True, False,
     False, True, False, False, False),
    // tkChar x all
    (False, False, False, False, True,
     False, False, True, False, False),
    // tkComment x all
    (False, False, False, False, False,
     False, False, False, False, False),
    // tkPreCmd x All
    (False, False, False, False, False,
     False, False, False, False, False)
  );

Function MulOpToJS(St : String): String;
Function SumOpToJS(St : String): String;
Function RelOpToJS(St : String): String;

Implementation

Function MulOpToJS(St : String): String;
Begin
  If St = '*' Then
    MulOpToJS := '*'
  Else If St = 'div' Then
    MulOpToJS := '/'
  Else If St = '/' Then
    MulOpToJS := '/'
  Else If St = 'and' Then
    MulOpToJS := '&&';
End;

Function SumOpToJS(St : String): String;
Begin
End;

Function RelOpToJS(St : String): String;
Begin
End;

End.


    


