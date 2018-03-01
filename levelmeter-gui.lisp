;;;; levelmeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(define-widget levelmeter-main (Qwidget cudagui-tl-mixin)
  ((dsp-node-ids :initarg :dsp-node-ids :accessor dsp-node-ids)
   (num :initform 2 :initarg :num :accessor num)
   (meters :initarg :meters :accessor meters)))

(define-initializer (levelmeter-main setup)
  (cudagui-tl-initializer levelmeter-main)
  (q+:set-geometry levelmeter-main 50 50 (* num 25) 400))

(define-override (levelmeter-main paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter levelmeter-main)))
    (let ((width (q+:width levelmeter-main))
          (height (q+:height levelmeter-main)))
      ;; (setf (q+:background painter)
      ;;       (q+:make-qbrush (q+:make-qcolor 60 60 60 255) (q+:qt.solid-pattern)))
      (q+:erase-rect painter (q+:rect levelmeter-main))
      (q+:set-color (q+:pen painter) (q+:make-qcolor 255 255 255 255))
      (let* ((db-inc 6)
             (margin 12)
             (meter-height (- height (* 2 margin)))
             (ht-inc (* meter-height (/ db-inc 100))))
        (dotimes (i (1+ (floor (/ 100 db-inc))))
          (let ((y-pos (+ margin (round (* i ht-inc)))))
            (q+:draw-line painter 0 y-pos width y-pos)))
        (q+:draw-line painter 0 (- height margin) width (- height margin))))))

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

(define-signal (levelmeter set-level) (int))

(define-slot (levelmeter set-level) ((value int))
  (declare (connected
            levelmeter
            (set-level int)))
   (setf (level levelmeter) value)
   (q+:repaint levelmeter))

(defun change-level (levelmeter value)
  (signal! levelmeter (set-level int) value))

(defun meter-gui (&key (num 2) (id "Meters") dsp-node-ids)
  (if (find-gui id)
      (error "widget ~a already existing. Please choose another name." id)
      (create-tl-widget 'levelmeter-main id :num num :dsp-node-ids dsp-node-ids)))

;;; (meter-gui :num 2 :id "Meters")

;;; (meter-gui :num 8 :id "Meters02")
;;;(find-gui "Meters02")


#|
(let ((num 2) (id "Meters") node-ids)
  (make-instance 'levelmeter-main :id id :num num :node-ids node-ids))
|#

(define-override (levelmeter-main close-event) (ev)
  (declare (ignore ev))
  (dolist (id (node-ids levelmeter-main))
    (format t "~&removing: ~a~%" id)
    (incudine:free id))
  (format t "~&closing: ~a" levelmeter-main)
  (remove-gui (id levelmeter-main))
  (call-next-qmethod))
