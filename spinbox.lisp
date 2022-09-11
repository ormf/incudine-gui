;;; 
;;; spinbox.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :incudine-gui)
(named-readtables:in-readtable :qt)

(defparameter *beatstep-box-style*
 "QSpinBox {
    background-color: #dddddd;
}
")

(defparameter *nanokontrol-box-style*
  "border: 10px solid #838383; 
background-color: #00dddd;
selection-color: black;
border-radius: 20px;
text-align: right;
selection-background-color: white")

(defparameter *custom-spinbox-style*
  "border: 4px solid #838383; 
background-color: #00dddd;
selection-color: black;
border-radius: 20px;
selection-background-color: white;
QSpinBox::ButtonSymbols: NoButtons")

(defclass custom-spinbox ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QSpinBox")
  (:signals
   ("returnPressed(int)"))
  (:override
   ("keyPressEvent" key-press-event)))

(defmethod initialize-instance :after ((instance custom-spinbox) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  )

(defmethod key-press-event ((instance custom-spinbox) ev)
;;;  (format t "~%~a, ~a~%" (#_key ev) (#_modifiers ev))
  (cond ;; Signal Ctl-Space pressed.
    ((= (#_key ev) 16777220)
     (case (#_modifiers ev)
       (0 (emit-signal instance "returnPressed(int)" 0))
       (100663296 (emit-signal instance "returnPressed(int)" 1))))
    ;; Delegate standard.
    (T
     (call-next-qmethod))))
