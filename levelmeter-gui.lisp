;;;; levelmeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

#|
(defparameter *red-brush* (#_new QBrush (#_Qt::red) (#_Qt::SolidPattern)))
(defparameter *green-brush* (#_new QBrush (#_Qt::green) (#_Qt::SolidPattern)))
(defparameter *yellow-brush* (#_new QBrush (#_Qt::yellow) (#_Qt::SolidPattern)))
(defparameter *background-brush* (#_new QBrush (#_new QColor 60 60 60 255) (#_SolidPattern "Qt")))
|#


(defclass levelmeter ()
  ((level :initarg :level :initform 0 :accessor level)
   (parent :initarg :parent :accessor parent)
   (painter :accessor painter)
   (green-brush :accessor green-brush)
   (yellow-brush :accessor yellow-brush)
   (red-brush :accessor red-brush)
   (pen-style :accessor pen-style)
   (background :accessor background))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("paintEvent" paint-event))
  (:slots
   ("changeLevel(int)" (lambda (this newval)
                         (setf (level this)
                               newval)
;;;                         (#_setUpdatesEnabled (parent this) nil)
                         (setf (repaint? (parent this)) nil)
                         (#_repaint this)
                         (setf (repaint? (parent this)) t)
                         )))
  (:signals
   ("setLevel(int)")))



(defmethod initialize-instance :after ((instance levelmeter) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (setf (painter instance) (#_new QPainter instance))
  (setf (pen-style instance) (#_NoPen "Qt"))
  (setf (red-brush instance) (#_new QBrush (#_Qt::red) (#_Qt::SolidPattern)))
  (setf (green-brush instance) (#_new QBrush (#_Qt::green) (#_Qt::SolidPattern)))
  (setf (yellow-brush instance) (#_new QBrush (#_Qt::yellow) (#_Qt::SolidPattern)))
  (setf (background instance) (#_new QBrush (#_new QColor 60 60 60 255) (#_SolidPattern "Qt")))
  (#_setBackground (painter instance) (background instance))
  (connect instance "setLevel(int)" instance "changeLevel(int)"))

(defmethod paint-event ((instance levelmeter) ev)
  (declare (ignore ev))
  ;; (let ((width (#_width instance))
  ;;       (height (#_height instance)))
  ;;   (with-slots (painter background pen-style
  ;;                        red-brush green-brush
  ;;                        yellow-brush level )
  ;;       instance
  ;;     (#_begin painter instance)
  ;;     (#_setBackground painter background)
  ;;     (#_eraseRect painter (#_rect instance))
  ;;     (#_setPen painter pen-style)
  ;;     (cond
  ;;       ((> level 90) (#_setBrush painter red-brush))
  ;;       ((> level 80) (#_setBrush painter yellow-brush))
  ;;       (t (#_setBrush painter green-brush)))
  ;;     (#_drawRect painter 0 height width (round (- 0 (* height 0.01 level))))
  ;;     (#_end painter)
  ;;     ))
  )


(defun change-level (levelmeter value)
  (setf (repaint? (parent levelmeter)) nil)
  (emit-signal levelmeter "setLevel(int)" value)
  (setf (repaint? (parent levelmeter)) t)
  )

(defclass levelmeter-main (cudagui-tl-mixin)
  ((dsp-node-ids :initarg :dsp-node-ids :accessor dsp-node-ids)
   (num :initform 2 :initarg :num :accessor num)
   (layout :accessor layout)
   (repaint? :initform t :accessor repaint?)
   (meters :initarg :meters :accessor meters)
   (painter :accessor painter)
   (pen-color :accessor pen-color))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("paintEvent()" paint-event)
   ("closeEvent()" close-event)))

(defmethod initialize-instance :after ((instance levelmeter-main) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (cudagui-tl-initializer instance)
  (#_setGeometry instance 50 50 (* (num instance) 25) 400)
  (with-slots (pen-color painter layout num meters) instance
    (setf painter (#_new QPainter instance))
    (setf layout (#_new QHBoxLayout instance))
    (setf pen-color (#_new QColor 255 255 255 255))
    (setf meters (make-array (list num)
                             :element-type 'levelmeter
                             :initial-element (make-instance 'levelmeter)))

    (loop for idx below num
       do (let ((meter (make-instance 'levelmeter :parent instance)))
            (setf (aref meters idx) meter)
            (#_addWidget layout meter)))))

(defmethod paint-event ((instance levelmeter-main) ev)
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

(defun meter-gui (&key (num 2) (id "Meters") dsp-node-ids)
  (if (find-gui id)
      (error "widget ~a already existing. Please choose another name." id)
      (create-tl-widget 'levelmeter-main id :num num :dsp-node-ids dsp-node-ids)))

;;; (meter-gui :num 2 :id "Meters")
#|
(time (create-tl-widget 'levelmeter-main "m01" :num 2 :dsp-node-ids nil))
(time (meter-gui :num 16 :id "Meters02"))
(meter-gui :num 16 :id "Meters02")
;;;(find-gui "Meters02")

(time (change-level (aref (meters (find-gui "Meters")) 0) (random 100)))

(scratch::dump (scratch::node 0))
|#

#|

(/ 360192   124.0) 2904.7742

(/ 327440    77.0) 4252.468

(/ 294672  82.0) 3593.561

(/ 851360    274.0) 3107.1533 

(let ((num 2) (id "Meters") node-ids)
  (make-instance 'levelmeter-main :id id :num num :dsp-node-ids node-ids))
|#

(defmethod close-event ((instance levelmeter-main) ev)
  (declare (ignore ev))
  (dolist (id (dsp-node-ids instance))
;;    (format t "~&removing: ~a~%" id)
    (incudine:free id))
;;  (format t "~&closing: ~a" levelmeter-main)
  (remove-gui (id instance))
  (call-next-qmethod))

#|
Messung:
            Audio    gui    Summe
pd (64ch)    7.9      25     36
                     4-10


(#_processEvents *qapplication*)

(#_drawRectinter)

(defparameter *painter* (#_new QPainter))
(defparameter *widget* (#_new QWidget))

(#_eraseRect *painter* (#_rect *widget*))

(LAMBDA ()
  (OPTIMIZED-CALL T *PAINTER* "eraseRect"
                  ((LAMBDA () (OPTIMIZED-CALL T *WIDGET* "rect")))))


  (OPTIMIZED-CALL T *PAINTER* "eraseRect"
                  ((LAMBDA () (OPTIMIZED-CALL T *WIDGET* "rect"))))



(defun const1 (x)
  (declare (optimize (debug 3)))
  (let ((result ()))
    (mapcar (lambda (x) (push (cons (random 10) x) result)) x)
    result))

(defun const2 (x)
  (declare (optimize (debug 3)))
  (mapcar (lambda (x) (cons x (const1 *l1*))) x))

(disassemble
 (defun const3 (x)
   (declare (optimize (debug 3)))
   (mapcar (lambda (x) (cons x (const2 *l1*))) x)))

(defparameter *l1* '(1 2 3 4 5 6 7 8 9))

(const1 *l1*)

(defun conts1 ()
  (loop for i below 20 collect (list i (const1)))
)

|#
