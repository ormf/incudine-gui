## Incudine-gui

This package is intended as a gui extension to
[incudine](http://incudine.sourceforge.net/).

It uses commonqt (QT 4.8) as the gui framework.

At the moment it is just a proof of concept.

## Dependencies

* incudine
* qt (commonqt)

## Usage (with quicklisp)

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
