;;;; incudine-gui.asd
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:incudine-gui
  :description "Describe incudine-gui here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the GPL v2 or later"
  :serial t
  :depends-on (#:incudine
               #:qtools
               #:qtcore #:qtgui)
  :components ((:file "package")
               (:file "globals")
               (:file "gui-utils")
               (:file "gui-startup")
               (:file "gui-registry")
               (:file "gui-class")
               (:file "numbox")
               (:file "pushbutton")
               (:file "scrollbar")
               (:file "levelmeter-gui")
               (:file "levelmeter-incudine")
               (:file "stethoscope")))

