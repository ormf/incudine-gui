;;;; package.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(defpackage #:incudine-gui
  (:nicknames #:cuda-gui)
  (:use #:cl+qt)
  (:export #:GUI-START
           #:GUI-STOP
           #:FIND-GUI
           #:STETHOSCOPE
           #:SCOPE
           #:METER-GUI
           #:METERS
           #:DSP-NODE-IDS
           #:*TEST*
           #:CHANGE-LEVEL))
