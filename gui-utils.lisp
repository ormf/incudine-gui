;;;; gui-utils.lisp
;;;;
;;;; utility functions for incudine-gui.
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(defun normalize (val min max)
  (/ (- val min) (- max min)))

(defun map-value (src-val src-min src-max dest-min dest-max)
  "map a value between src-min and src-max to a destination range of
dest-min and dest-max (linear)."
  (+ dest-min
     (* (normalize src-val src-min src-max)
        (- dest-max dest-min))))

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
