// if debugging is enabled in compilation
 {$ifdef VERBOSE_ERROR}
  if GetProcAddress(MainLibHandle, 'CountCGIVars') = nil then
  begin
    ThrowLibError(ProcErr + ' 3 ' + LibPath );
    halt;
  end;
    //ERROR #3 (ERR3): Cannot getprocaddress of CountCGIVars from library        // LUFDOC
 {$endif}

