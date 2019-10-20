;;;; incudine-gui.asd
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:incudine-gui
  :description "Describe incudine-gui here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "licensed under the GPL v2 or later"
  :serial t
  :depends-on (:qt-libs :qt :incudine)
  :components ((:file "package")
               (:file "qt-fast-calls")
               (:file "globals")
               (:file "incudine-setup")
               (:file "gui-utils")
               (:file "gui-registry")
               (:file "gui-startup")
               (:file "gui-class")
               (:file "numbox")
               (:file "labelbox")
               (:file "pushbutton")
               (:file "scrollbar")
               (:file "levelmeter-gui")
               (:file "levelmeter-incudine")
               (:file "stethoscope-gui")
               (:file "stethoscope-incudine")
               (:file "beatstep-gui")
               ))
 
