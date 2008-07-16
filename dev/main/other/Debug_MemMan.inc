// if debugging is enabled in compilation
 {$ifdef VERBOSE_ERROR}
  if GetProcAddress(MainLibHandle, 'GetSharedMemMan') = nil then
  begin
    ThrowLibError(ProcErr + ' 2 ' + LibPath );
    halt;
  end;
  //ERROR #2 (ERR2): Cannot getprocaddress of GetMemMan procedure from library // LUFDOC
 {$endif}

