unit ThreadCollectionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TThread, CollectionUnit;
  
type

  { TThreadCollection }

  TThreadCollection= class (TBaseCollection)
  private
    function GetResidentThread (Index: Integer): TResidentPageExcecuteThread;

  public
    property ResidentThread [Index: Integer]: TResidentPageExcecuteThread
         read GetResidentThread;

    constructor Create;
    procedure Free;

  end;

implementation

end.

