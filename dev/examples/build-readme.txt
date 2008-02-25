Build.exe or build.elf etc. is a system built by Lars (L505) that builds
the powtils examples.

It is powered by the pwbuildutil.pas unit.

There are several options:

  build default 
	builds the default examples with no special defines

  build clean
	cleans the .crap/ folders of their PPU/.A/.O files

  build all 
	builds all combos (takes a while), i.e. default, plus GZIP_ON, SYSUTILS_ON

You can 
  fpc build

To rebuild the BUILD script (actually just a program!) for your platform.. or use 
the build.exe or build.elf binary if included with your download.

It is like FPMAKE kind of, but fpmake didn't serve my needs, nor did
fpcmake or gnumake.

The build utility isn't perfect yet, and nothing is! It needs more features. 
But a very good codebase in pwbuildutil right now with tons of options to 
build projects.

