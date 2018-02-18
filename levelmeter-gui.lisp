;;;; levlemeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(defparameter *test* nil)

(define-widget main (QWidget)
  ((num :initform 2 :initarg :num :accessor num)
   (meters :initarg :meters :accessor meters)))

(define-widget levelmeter (QWidget)
  ((level :initarg :level :initform 0 :accessor level)))

(define-override (levelmeter paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter levelmeter)))
    (let ((width (q+:width levelmeter))
          (height (q+:height levelmeter)))
      (setf (q+:background painter)
            (q+:make-qbrush (q+:make-qcolor 60 60 60 255) (q+:qt.solid-pattern)))
      (q+:erase-rect painter (q+:rect levelmeter))
      (setf (q+:pen painter) (q+:qt.no-pen))
      (cond
        ((> level 90)
         (setf (q+:brush painter) (q+:make-qbrush (q+:qt.red) (q+:qt.solid-pattern))))
        ((> level 80)
         (setf (q+:brush painter) (q+:make-qbrush (q+:qt.yellow) (q+:qt.solid-pattern))))
        (t
         (setf (q+:brush painter) (q+:make-qbrush (q+:qt.green) (q+:qt.solid-pattern)))))
      (q+:draw-rect painter 0 height width (round (- 0 (* height 0.01 level)))))))

(define-subwidget (main layout) (q+:make-qhboxlayout main)
  (setf meters (make-array (list num)
                           :element-type 'levelmeter
                           :initial-element (make-instance 'levelmeter)))
  (loop for idx below num
     do (let ((meter (make-instance 'levelmeter)))
          (setf (aref meters idx) meter)
          (q+:add-widget layout meter))))

(define-initializer (main setup)
  (setf (q+:window-title main) "meters")
  (q+:set-style-sheet main "background-color: #202020;")
  (q+:set-geometry main 50 50 (* num 25) 400)
  (setf *test* main))

(define-signal (levelmeter set-level) (int))

(define-slot (levelmeter set-level) ((value int))
  (declare (connected
            levelmeter
            (set-level int)))
   (setf (level levelmeter) value)
   (q+:repaint levelmeter))

(defun change-level (levelmeter value)
  (signal! levelmeter (set-level int) value))

;;; (change-level (aref (meters *test*) 0) (random 100)

(defun meter-gui (&key (num 2))
  (with-controller ()
    (q+:show (make-instance 'main :num num))))

;;; (meter-gui :num 16)
