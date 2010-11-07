SDL_Net WebUpdate Example
-------------------------

This project shows how to use the SDLWeb units to write a simple HTTP web Update program.

This program takes two parameters 

1) The current Version of the application
2) the location of the ini file which contains the update information (see exampleupdate.ini for details)

the program will download the file specified to the local directory with the same name. After
that the host program can do what it needs to.


Example
--------

webupdate 1.0 http://somewebsite/updates/update.ini

Update.ini contains

[LatestVersion]
Version=1.9
URL=http://somewebsite/updates/
Filename=latestbuild.zip