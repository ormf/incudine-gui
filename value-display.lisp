;;; 
;;; value-display.lisp
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

(defclass valuedisplay ()
  ((value :initarg :value :initform 0 :accessor value)
   (parent :initarg :parent :accessor parent)
   (painter :accessor painter)
   (background :accessor background))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
;;  (:override ("paintEvent" paint-event))
  (:slots
   ("changeValue(int)" (lambda (this newval)
                         (setf (value this) newval)
                         (setf (repaint? (parent this)) nil)
                         (#_repaint this)
                         (setf (repaint? (parent this)) t)
                         )))
  (:signals
   ("setValue(int)")))

(defmethod initialize-instance :after ((instance valuedisplay) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (setf (painter instance) (#_new QPainter instance))
;;  (setf (pen-style instance) (#_NoPen "Qt"))
  ;; (setf (red-brush instance) (#_new QBrush (#_Qt::red) (#_Qt::SolidPattern)))
  (setf (background instance) (#_new QBrush (#_new QColor 150 150 150 255) (#_SolidPattern "Qt")))
  (#_setBackground (painter instance) (background instance))
  (connect instance "setValue(int)" instance "changeValue(int)"))

(defun change-value (valuedisplay value)
  (setf (repaint? (parent valuedisplay)) nil)
  (emit-signal valuedisplay "setValue(int)" value)
  (setf (repaint? (parent valuedisplay)) t))

(defclass valuedisplay-main (cudagui-tl-mixin)
  ((num :initform 2 :initarg :num :accessor num)
   (layout :accessor layout)
   (repaint? :initform t :accessor repaint?)
   (items :initarg :items :accessor items)
   (painter :accessor painter)
   (pen-color :accessor pen-color))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("closeEvent()" close-event)))

(defmethod initialize-instance :after ((instance valuedisplay-main) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (cudagui-tl-initializer instance)
  (#_setGeometry instance 50 50 (* (num instance) 25) 400)
  (with-slots (pen-color painter layout num items) instance
    (setf painter (#_new QPainter instance))
    (setf layout (#_new QHBoxLayout instance))
    (setf pen-color (#_new QColor 255 255 255 255))
    (setf items (make-array (list num)
                             :element-type 'valuedisplay
                             :initial-element (make-instance 'valuedisplay)))

    (loop for idx below num
       do (let ((item (make-instance 'valuedisplay :parent instance)))
            (setf (aref items idx) item)
            (#_addWidget layout item)))))

#|
(defmethod paint-event ((instance valuedisplay-main) ev)
  (declare (ignore ev)
;;;           (optimize (speed 3))
           )
  (if (repaint? instance)
      (let* ((width (#_width instance))
             (height (#_height instance))
             (db-inc 6)
             (margin 12)
             (meter-height (- height (* 2 margin)))
             (ht-inc (float (* meter-height (/ db-inc 100)))))
        ;; (declare (fixnum height width db-inc margin meter-height)
        ;;          (single-float ht-inc))
        ;; (setf (#_background painter)
        ;;       (#_make-qbrush (#_make-qcolor 60 60 60 255) (#_qt.solid-pattern)))
;;        (format t "repaint!~%")
        (let ((painter (painter instance))
              (pen-color (pen-color instance)))
          (qt::fast-begin painter instance)
          (#_eraseRect painter (#_rect instance))
          (#_setColor (#_pen painter) pen-color)
          (dotimes (i (1+ (floor (/ 100 db-inc))))
            (let ((y-pos (+ margin (round (* i ht-inc)))))
              (#_drawLine painter 0 y-pos width y-pos)))
          (#_drawLine painter 0 (- height margin) width (- height margin))
          (#_end painter)))))
|#
