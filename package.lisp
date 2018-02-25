;;;; package.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(defpackage #:incudine-gui
  (:nicknames #:cuda-gui)
  (:use #:cl+qt)
  (:export #:GUI-START
           #:GUI-STOP
           #:METER-GUI
           #:METERS
           #:NODE-IDS
           #:*TEST*
           #:CHANGE-LEVEL))

