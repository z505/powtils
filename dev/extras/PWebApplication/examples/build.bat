@Echo off
Copy %1.pp ..
CD ..
Copy .\dependency\*.o
Copy .\dependency\*.ppu
ppc386 %1 -Sd -B -Gl
del *.ppu
del *.o
Copy %1.exe .\examples
del %1.exe
cd examples
