For help: see index.html, and another index.html in the docs/ folder

Included are source code, Linux binaries (elf) and Win32 binaries (exe)
BSD was not yet tested.

On linux run ./aservia at command line
On windows run aservia.exe at command line

To compile the server:
  fpc aservia.dpr

If you have trouble with missing units try:
  fpc aservia.dpr -B -Fu../../main/

You must restart the server for config settings to apply.

If your computer is called "localhost" or "yourserver" then check the config 
files to change to the appropriate settings.
