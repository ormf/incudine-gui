;;; 
;;; beatstep-gui.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defparameter *beatstep-box-style*
  "border: 1px solid #838383; 
background-color: #dddddd;
selection-color: black;
border-radius: 2px;
text-align: right;
selection-background-color: white")

(defparameter *beatstep-pushbutton-style*
"
QPushButton {
         border: 2px solid #838383; 
         border-style: outset;
         background-color: #dddddd;
         border-radius: 2px;
         min-width: 40px;
     }
")

(defparameter *beatstep-pushbutton-style*
"
QPushButton {
         background-color: #dddddd;
         min-width: 40px;
     }
")

(defun empty-fn (&rest args) (declare (ignore args)) (values))

;;; (set-fader (find-gui :bs1) 14 120)

#|

(setf (val (aref (param-boxes (find-gui :bs1)) 10)) 31)

(set-ref (aref (param-boxes (find-gui :nk2)) 10) (cl-boids-gpu::num-boids cl-boids-gpu::*bp*))

;;; (aref (param-boxes (find-gui :nk2)) 2)
|#

(defclass beatstep-grid (cudagui-tl-mixin)
  ((rows :initform 2 :initarg :rows :accessor rows)
   (cols :initform 8 :initarg :cols :accessor cols)
   (cleanup-fn :initform #'empty-fn :initarg :cleanup-fn :accessor cleanup-fn)
   (param-boxes :initform (make-array 16) :accessor param-boxes)
   (buttons :initform (make-array 16) :accessor buttons))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("closeEvent" close-event)))

(defmethod initialize-instance :after ((instance beatstep-grid) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer instance))
  (with-slots (param-boxes buttons rows cols)
      instance
    (let ((main (#_new QVBoxLayout instance))
          (grid (#_new QGridLayout)))
      (loop for row below rows
         do (loop for column below (* 2 cols) by 2
                  do (let* ((idx (+ (/ column 2) (* 8 row)))
                            (new-lsbox (make-instance
                                        'label-spinbox
                                        :label (format nil "~d" (1+ idx))
                                        :text "--"
                                        :id (1+ idx))))
                       (#_setRange (text-box new-lsbox) 0 127)
                       (setf (aref param-boxes idx) new-lsbox)
                       (let ((lsboxlayout (#_new QHBoxLayout)))
                         (#_addWidget grid (label-box new-lsbox) (1+ row)  column)
                         (#_addWidget lsboxlayout (text-box new-lsbox))
                         (#_addStretch lsboxlayout)
                         (#_addLayout grid lsboxlayout (1+ row) (1+ column))))))
      (loop for row below rows
         do (loop for column below (* 2 cols) by 2
                  do (let* ((idx (+ (/ column 2) (* 8 row)))
                            (new-button (make-instance 'toggle :state 0 :id (1+ idx))))
                       (setf (aref buttons idx) new-button)
                       (#_setFocusPolicy new-button (#_StrongFocus "Qt"))
                       (#_setFixedHeight new-button 25)
                    (let ((buttonlayout (#_new QHBoxLayout)))
                      (#_addWidget buttonlayout new-button)
                      (#_addStretch buttonlayout)
                      (#_addLayout grid buttonlayout (+ 3 row) (1+ column))))))
      (#_addLayout main grid))))

;;; pvb is "Parameter-View-Box"

(defmethod close-event ((instance beatstep-grid) ev)
  (declare (ignore ev))
  (remove-gui (id instance))
  (funcall (cleanup-fn instance))
  (stop-overriding))

(defgeneric set-fader (obj idx val))

(defmethod set-fader ((instance beatstep-grid) idx val)
  (emit-signal (aref (param-boxes instance) idx) "setValue(int)" val))

(defgeneric inc-fader (obj idx val))

(defmethod inc-fader ((instance beatstep-grid) idx inc)
  (emit-signal (aref (param-boxes instance) idx) "incValue(int)" inc))

(defgeneric set-encoder-callback (obj idx fn))

(defmethod set-encoder-callback ((instance beatstep-grid) idx fn)
  (setf (callback (aref (param-boxes instance) idx)) fn))

(defgeneric set-pushbutton-callback (obj idx fn))

(defmethod set-pushbutton-callback ((instance beatstep-grid) idx fn)
  (setf (callback (aref (buttons instance) idx)) fn))

(defun beatstep-gui (&key (id :bs1))
  (if (find-gui id) (progn (close-gui id) (sleep 1)))
  (create-tl-widget 'beatstep-grid id))

#|

(emit-signal (aref (param-boxes (find-gui :bs1)) 0) "setValue(int)" 40)

|#
