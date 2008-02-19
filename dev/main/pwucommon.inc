{ common constants s to use in cross platform a web program }
const
 {$ifdef unix}
  PATH_DELIM = '/';
  LOCAL_DIR = './';
 {$endif}
 {$ifdef windows}
  PATH_DELIM = '\';
  LOCAL_DIR = '.\';
 {$endif}
