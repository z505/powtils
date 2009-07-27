--------
webcmd1
--------
A small simple CGI program to run unix commands on your server in a web program.

You do not need SSH/TELNET/SHELL access.. the web program allows you to do
basically anything telnet would offer, *except* you can't run things like
midnight commander or fp-ide over a web browser. Just console commands that
are not visual. Can't run things that require keyboard prompting, yet either.

It is a simple program for learning purposes.. but you can make improvements.

It is very powerful little program..

Try running some typical unix commands like ls, mv, cp, zip, unzip, etc. Careful
what you do!

This should work on BSD if you compile it.

MS Windows: haven't tried it much but included is an exe

Webcmd sources released under NRCOL public domain.
Powtils released under Artistic license.

Regards,
L505 (Lars)
http://z505.com


----------------
SECURITY NOTICE
----------------

Webcmd is powerful, so you should never upload the program as 'webcmd'..
Rename it to xyzcmd or cmd1234888 so people cannot search for it using google.
(security through obscurity)

If people find 'webcmd' on your server they can take control of your server.

Password protect any directories that 'webcmd' is in by using HTACCESS
permissions or similar.

Disable webcmd when you are done with it.. by disabling execute permissions or
even deleting it off your server until you need it again.
