{ This include file is designed so that each time a new release of PWU is being
  created, only this one file needs to be edited to update the version number.
  All source code that makes reference to specific version should include this
  file.

  Makefiles and the Config files do not use this file, unfortunately they need
  to be updated with each release.  }

CONST PWU_VERSION = '1.7.x';
