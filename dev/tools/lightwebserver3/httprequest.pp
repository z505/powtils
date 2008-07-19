Uses
  LWS2Util,
  pwInit,
  pwMain;

Begin
  If GetCGIVar('action') = 'testpost' Then
  Begin
    OutLn('<html>');
    OutLn('<head>');
    OutLn('<title>Testing POST handling</title>');
    OutLn('</head>');
    OutLn('<body>');
    OutLn('WORKED !');
    OutLn('</body>');
    OutLn('</html>');
  End
  Else
  Begin
    OutLn('<html>');
    OutLn('<head>');
    OutLn('<title>Testing POST handling</title>');
    OutLn('</head>');
    OutLn('<body>');
    OutLn('<form method="POST" action="/">');
    OutLn('<input type="hidden" name="action" value="testpost">');
    OutLn('<input type="submit" name="submit" value="Try It !">');
    OutLn('</form>');
    OutLn('</body>');
    OutLn('</html>');
  End;
End.
