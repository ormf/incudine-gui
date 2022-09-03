;;;; package.lisp
;;;;
;;;; Copyright (c) 2018-22 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(defpackage #:incudine-gui
  (:nicknames #:cuda-gui)
  (:use :cl :qt :cellctl)
  (:export #:GUI-START
           #:GUI-STOP
           #:FIND-GUI
           #:STETHOSCOPE
           #:SCOPE
           #:METER-GUI
           #:BEATSTEP-GUI
           #:NANOKONTROL-GUI
           #:FADERFOX-GUI
           #:METERS
           #:DSP-NODE-IDS
           #:*TEST*
           #:CHANGE-LEVEL
           #:SET-FADER
           #:INC-FADER
           #:SET-ENCODER-CALLBACK
           #:SET-PUSHBUTTON-CALLBACK
           #:PARAM-BOXES
           #:BUTTONS
           #:STATE
           #:SET-PVB-VALUE
           #:INC-PVB-VALUE
           #:RECALL-PVB-VALUE))
