;;;; levelmeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(defparameter *test* nil)

(define-widget levelmeter-main (QWidget)
  ((id :initform "Meters" :initarg :id :accessor id)
   (window-title)
   (gui-signal :initform nil :initarg :gui-signal :accessor gui-signal)
   (node-ids :initarg :node-ids :accessor node-ids)
   (num :initform 2 :initarg :num :accessor num)
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

(define-subwidget (levelmeter-main layout) (q+:make-qhboxlayout levelmeter-main)
  (setf meters (make-array (list num)
                           :element-type 'levelmeter
                           :initial-element (make-instance 'levelmeter)))
  (loop for idx below num
     do (let ((meter (make-instance 'levelmeter)))
          (setf (aref meters idx) meter)
          (q+:add-widget layout meter))))

(define-initializer (levelmeter-main setup)
  (setf (q+:window-title levelmeter-main) (format nil "~s" id))
  (q+:set-style-sheet levelmeter-main "background-color: #202020;")
  (q+:set-geometry levelmeter-main 50 50 (* num 25) 400)
  (if (and (add-gui id levelmeter-main)
           (gui-signal levelmeter-main))
      (incudine::sync-condition-signal *from-gui-sync*)))

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

(defun meter-gui (&key (num 2) (id "Meters") node-ids)
  (unwind-protect 
       (with-controller ()
         (q+:show (make-instance 'levelmeter-main :id id :num num :node-ids node-ids :gui-signal t))))
  (incudine::sync-condition-wait *from-gui-sync*)
  (find-gui id))

;;; (meter-gui :num 8 :id "Meters")

;;; (meter-gui :num 8 :id "Meters02")
;;;(find-gui "Meters02")


#|
(let ((num 2) (id "Meters") node-ids)
  (make-instance 'levelmeter-main :id id :num num :node-ids node-ids))
|#

(define-override (levelmeter-main close-event) (ev)
  (declare (ignore ev))
  (dolist (id (node-ids levelmeter-main)) (incudine:free id))
;;  (format t "closing: ~a" levelmeter-main)
  (remove-gui id)
  (call-next-qmethod))
