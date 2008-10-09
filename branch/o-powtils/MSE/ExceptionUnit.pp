unit ExceptionUnit;

interface
uses
  SysUtils;
  
type
  ENotImplemented= class (Exception)
  public
    constructor Create (MName: String);

  end;
  
  ENotImplementedYet= class (Exception)
  public
    constructor Create (MName: String); overload;
    constructor Create (CName, MName: String); overload;

  end;
  
  ERangeCheckError= class (Exception)
  public
    constructor Create (AMethodName: String);

  end;

implementation

{ ERangeCheckError }

constructor ERangeCheckError.Create (AMethodName: String);
begin
  inherited Create ('Range check Error in '+ AMethodName);
  
end;

{ ENotImplemented }

constructor ENotImplemented.Create (MName: String);
begin
  inherited Create ('the method '+ MName+ ' is not implemented, yet!');
  
end;

{ ENotImplementedYet }

constructor ENotImplementedYet.Create(MName: String);
begin
  inherited Create ('the method '+ MName+ ' is not implemented, yet!');

end;

constructor ENotImplementedYet.Create (CName, MName: String);
begin
  inherited Create ('the method '+ MName+ ' in class '+ CName+ ' is not implemented, yet!');

end;

end.
