@Echo Off

Copy C:\Guardar\Programacao\Synapse\Source\Lib\*.ppu
Copy C:\Guardar\Programacao\Synapse\Source\Lib\*.o

\fpc\2.2.0\bin\i386-win32\PPC386 %1 -B -S2

Del *.ppu
Del *.o

