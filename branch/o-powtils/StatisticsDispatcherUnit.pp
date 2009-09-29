unit StatisticsDispatcherUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PageHandlerBaseUnit;

type

  { TStatisticsPage }

  TStatisticsPage= class (THTMLHandler)
  private
  public
    constructor Create (PagePath: AnsiString);


  end;

implementation

{ TStatisticsPage }

constructor TStatisticsPage.Create (PagePath: AnsiString);
begin
  inherited Create ('StatPage', PagePath+ 'StatPage.psp');
end;

end.

