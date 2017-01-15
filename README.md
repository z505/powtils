Powtils is a web development tool kit or framework for freepascal (and delphi)

Currently work is being done to port these units to freepascal 3.0.  

The compiler used to compile right now is freepascal version 2.2.4

Many of the tools will also be ported to GoLang. Some units have already een ported to GoLang, an interesting oberon/fpc like language. These golang units need to be uploaded to github. Probably seperate repo for it will be started.

Also working on some more examples:
* In the examples there is a wiki parser, this will be updated to be a full functioning wiki. As cross platform file sharing in FPC is now ready this helps make the wiki multi user (file locking). Lack of cross platform file sharing was what was holding me back before.
* Polish up compile-studio (this program allows one to compile freepascal programs online in your browser using a server based compiler, just copy fpc compiler to your server cgi-bin in a folder and set correct permisssions, then you can use compiel studio on any shared web host or any server)
* Static wiki publishing system - create content and generate static html files.  This is an alternative to the regular wiki which is a live program. The static wiki generator will generate static html files to serve the reader, instead of a live program through cgi serving the content. The advantage of static html files is they are secure, as they have no inputs or variables, no url injection,just plain html. Possibly some similarities to Hugo project for golang, but some differences indeed.
* MySQL sessions instead of tet file sessions. Always wanted to demo this.. didn't get around to it.. now is time.
* SQLite sessions instead of text file sessions. Want to demo this too.

Other todo items:
* try instantFPC on the server, allowing programs to run without compiling by developer (act like scripts) 
* move fasthtmlparser to its own project. Jamming too many units into powtils project makes it too much like Delphi JEDI where everything is crammed into one single massive beast. Not good idea I guess.
* fasthtmlparser has already been ported to GoLang... need to upload code
* port wiki parser to golang
* make powtils cgi framework/tools for golang. Some work has been done here, only started.
