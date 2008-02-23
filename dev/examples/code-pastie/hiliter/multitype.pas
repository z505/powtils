unit multitype; {$IFDEF FPC}{$mode objfpc} {$H+}{$ENDIF}
interface

type
  TMultiType = record
    aString: string;
    aFile: file;
    aTextFile: text;
    mtype: (mtString, mtFile, mtTextFile);
  end;


implementation

end.
