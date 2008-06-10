{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                    PSP 1.6.x BaseClassUnit

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 Collection Unit
--------------------------------------------------------------------------------

 PSP 1.6.x
 ---------

  [30/MAR/2006 - Amir]
   - This unit was created. This unit contains the base structure for many of
     classes.
}

unit BaseClassUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TBaseClass }

  TBaseClass= class (TObject)
  private
  public
    constructor Create; virtual;
    
    function ToString: String; virtual; abstract;
    function Copy: TBaseClass; virtual; abstract;
  end;

implementation

{ TBaseClass }

constructor TBaseClass.Create;
begin
  inherited;
end;

end.

