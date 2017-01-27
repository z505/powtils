{$ifndef FPC} // delphi
  {$ifdef WIN32}{$define WINDOWS}{$endif}
  {$ifdef WIN64}{$define WINDOWS}{$endif}
  {$define SYSUTILS_ON} // can't use CompactSysUtils in delphi
{$endif}