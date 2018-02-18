## Incudine-gui

This package is intended as a gui extension to
[incudine](http://incudine.sourceforge.net/).

It uses qtools (QT 4.8) as the gui framework.

At the moment it is just a proof of concept.

## Dependencies

* incudine
* qtools

## Usage (with quicklisp)

* Copy or link the "incudine-gui" Folder to a place seen by asdf (like
  `"~/quicklisp/local-projects"`).

* Startup a quicklisp enabled Common Lisp

* Evaluate `(ql:quickload "incudine-gui")`

* Then:

```lisp
; SLIME 2.19
CL-USER> (ql:quickload "incudine-gui")
To load "incudine-gui":
  Load 1 ASDF system:
    incudine-gui
; Loading "incudine-gui"
.....To load "qt":
  Load 1 ASDF system:
    qt
; Loading "qt"
.
> Swapping out QT::LOAD-LIBCOMMONQT for QT-LIBS:LOAD-LIBCOMMONQT.
> Swapping out QT:MAKE-QAPPLICATION for QT-LIBS::MAKE-QAPPLICATION.
> Swapping out QT:ENSURE-SMOKE for QT-LIBS::ENSURE-SMOKE.

..........
("incudine-gui")
CL-USER> (in-package :scratch)
#<PACKAGE "INCUDINE.SCRATCH">
SCRATCH> (rt-start)
:STARTED
SCRATCH> (cuda-gui:meter-gui :num 8)
NIL
SCRATCH> (stereometer 10 0)
; No value
```
