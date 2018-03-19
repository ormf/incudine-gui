;;;; gui-utils.lisp
;;;;
;;;; utility functions for incudine-gui.
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defmacro make-button-menu (button &key (actions nil) text)
  (let ((menu (gensym "MENU")))
    `(let ((,menu (#_new QMenu)))
       ,@(loop
            for action in actions
            append `((setf ,(first action) (#_new QAction ,(second action) ,menu))
                     (#_addAction ,menu ,(first action))))
       (#_setMenu ,button ,menu)
       ,(if text
            `(#_setText ,button ,text)
            `(#_setText ,button ,(cadar actions)))
       ,menu)))

(defmacro with-painter ((painter instance) &body body)
  `(let ((,painter (#_new QPainter ,instance)))
;;;     (#_begin ,painter)
     ,@body
     (#_end ,painter)
     (#_delete ,painter)))

(defmacro with-paint-path ((paint-path) &body body)
  `(let ((,paint-path (#_new QPainterPath)))
     ,@body
     (#_delete ,paint-path)))

(defun find-node (node)
  (incudine:dograph (n)
    (when (equal node (incudine:node-id n))
      (return n))))

(defun normalize (val min max)
  (/ (- val min) (- max min)))

(defun map-value (src-val src-min src-max dest-min dest-max)
  "map a value between src-min and src-max to a destination range of
dest-min and dest-max (linear)."
  (+ dest-min
     (* (normalize src-val src-min src-max)
        (- dest-max dest-min))))

(declaim (inline round-single-float))
(defun round-single-float (x)
  (declare (type (single-float
                  #.(coerce (ash most-negative-fixnum -1) 'single-float)
                  #.(coerce (ash most-positive-fixnum -1) 'single-float)) x))
  (multiple-value-bind (result rem) (round x)
    (declare (ignore rem))
    result))

(defun format-title (title)
  (cond
    ((keywordp title) (string-downcase (format nil "~s" title)))
    (:else (format nil "~a" title))))

(defun start ()
  (incudine:rt-start)
  (sleep 0.5)
  (scratch:setup-io)
  (gui-start)
  (in-package :scratch)) 
