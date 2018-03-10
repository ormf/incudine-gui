;;;; pushbutton.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defclass pushbutton ()
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
" :accessor style))
  (:metaclass qt-class)
  (:qt-superclass "QPushButton"))

(defmethod initialize-instance :after ((instance pushbutton) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (#_setStyleSheet instance (style instance))
  (#_setFocusPolicy instance (#_NoFocus "Qt"))
  (#_setFixedHeight instance 25))

