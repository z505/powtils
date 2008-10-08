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

  Operators : Array[1..14] Of Record
    Pas,
    Js   : String;
  End =
  (
    (Pas : '+';   Js : '+'),
    (Pas : '-';   Js : '-'),
    (Pas : '*';   Js : '*'),

    (Pas : '/';   Js : '/'),
    (Pas : 'div'; Js : '/'),
    (Pas : 'and'; Js : '&&'),

    (Pas : 'or';  Js : '||'),
    (Pas : '=';   Js : '=='),
    (Pas : '<';   Js : '<'),

    (Pas : '>';   Js : '>'),
    (Pas : '<=';  Js : '<='),
    (Pas : '>=';  Js : '>='),

    (Pas : '<>';  Js : '!='),
    (Pas : 'not'; Js : '!')
  );



Function Translate(X : String): String;

Implementation

Function Translate(X : String): String;
Var
  y : LongInt;
Begin
  Translate := '';
  For y := Low(Operators) To High(Operators) Do
    If Operators[y].Pas = X Then
    Begin
      Translate := Operators[y].Js;
      Exit;
    End;
End;

End.


    


