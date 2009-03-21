unit AbstractDispatcherUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RequestsQueue;

type

  { TAbstractDispatcher }

  TAbstractDispatcher= class (TObject)
  private
    FPageName: String;
    FShouldBeFreed: Boolean;

  protected
    property ShouldBeFreed: Boolean read FShouldBeFreed;

  public
    property PageName: String read FPageName;

    constructor Create (ThisPageName: String; ShouldBeFreedManually: Boolean= True);
    destructor Destroy; override;

    function CreateNewInstance: TAbstractDispatcher; virtual; abstract;
    procedure MyDispatch; virtual; abstract;

  end;

implementation

{ TAbstractDispatcher }

constructor TAbstractDispatcher.Create (ThisPageName: String;
          ShouldBeFreedManually: Boolean);
begin
  inherited Create;

  FShouldBeFreed:= ShouldBeFreedManually;
  FPageName:= ThisPageName;

end;

destructor TAbstractDispatcher.Destroy;
begin
  inherited Destroy;

end;

end.

