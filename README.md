# This Repository has Moved!

  Due to the fact that GitHub is hosting the files on privately owned
  servers located in the USA, this repository has been moved to
  [Codeberg](https://codeberg.org/ormf/incudine-gui//) in November of
  2025. It will not receive further updates or patches. Issues and
  pull requests will not be looked at here either, please submit your
  patches and issue tickets on Codeberg, or send them directly via
  good old email patches to the author.

  Thanks.## Incudine-gui

This package is intended as a gui extension to
[incudine](http://incudine.sourceforge.net/).

It uses commonqt (QT 4.8) as the gui framework.

At the moment it is just a proof of concept.

## Dependencies

* incudine
* qt (commonqt)

## Usage (with quicklisp)

For compiling commonqt:

- Load (ql:quickload "qt-libs")

- Compile the libs with

  (qt-libs:ensure-standalone-libs :method :install-sources :force T)

They will be installed in .cache/common-lisp/sbcl-<version>/home/<username>/quicklisp/dists/quicklisp/software/qt-libs-<version>-git/

- Load (ql:quickload "qt")

- Wait until you hit the error

  OPERATION-ERROR while invoking #<COMPILE-OP > on
  #<CPP->SO "qt" "so" "commonqt">

Then change into the commqt quicklisp directory

cd /home/<username>/quicklisp/dists/quicklisp/software/commonqt-<version>-git/

issue

qmake-qt4

Then open the created Makefile and add the following include paths at
the end of the line beginning with "INCPATH" (omit the linebreaks!):

 -I/home/orm/.cache/common-lisp/sbcl-<version>-linux-x64/home/orm/quicklisp/dists/quicklisp/software/qt-libs-<version>-git/smokegen/install/include

and

-I/home/orm/.cache/common-lisp/sbcl-<version>-linux-x64/home/orm/quicklisp/dists/quicklisp/software/qt-libs-<version>-git/smokeqt/install/include

copy the generated libs with

sudo cp -av <qt-libs-dir> /usr/lib

run

sudo ldconfig

finally run

make

libcommonqt.so should now be generated

* Copy or link the "incudine-gui" Folder to a place seen by asdf (like
  `"~/quicklisp/local-projects"`).

* Startup a quicklisp enabled Common Lisp

* Evaluate `(ql:quickload "incudine-gui")`

* Then:

```lisp
; SLIME 2.19
CL-USER> (ql:quickload "incudine-gui")
<...>
("incudine-gui")
CL-USER> (cuda-gui::start)
#<PACKAGE "INCUDINE.SCRATCH">
SCRATCH> (cuda-gui:scope :num-chans 2 :id :scope01)
#<INCUDINE-GUI:STETHOSCOPE QWidget 0x7FFFC41386A0>
SCRATCH> 
```
This is the stub README.txt for the "incudine-gui" project.