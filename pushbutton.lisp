;;;; pushbutton.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(define-widget pushbutton (QPushButton)
  ((style :initform "
QPushButton {
         border: 1px solid #838383; 
         background-color: #ffffff;
         selection-color: red;
         cursor-color: white;
         border-radius: 2px;
         selection-background-color: white;
         min-width: 80px;
     }

QPushButton::menu-indicator {
         color: #dddddd;
         subcontrol-origin: padding;
         subcontrol-position: center right;
         left: -5px;
         top: 1px;
     }
" :accessor style)))

(define-initializer (pushbutton setup)
  (q+:set-style-sheet pushbutton style)
  (q+:set-Focus-Policy pushbutton (#_NoFocus "Qt"))
  (q+:set-fixed-height pushbutton 25))

