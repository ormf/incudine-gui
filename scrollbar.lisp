;;;; scrollbar.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defvar *scrollbarwidth* 15)

(deftype orientation () '(member :horizontal :vertical))

(defclass scroll-corner-box ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance scroll-corner-box) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (#_setFixedHeight instance *scrollbarwidth*)
  (#_setFixedWidth instance *scrollbarwidth*))

(defclass scrollbar ()
  ((orientation :initform :horizontal :type orientation
                :initarg :orientation :accessor orientation)
   (bg-pen-color :initform (#_new QColor 131 131 131 255)
                 :accessor bg-pen-color)
   (bg-brush :initform (#_new QBrush (#_new QColor 182 182 182 255))
             :accessor bg-brush)
   (thumb-pen-color :initform (#_new QColor 153 153 153)
                    :accessor thumb-pen-color)
   (thumb-brush :initform (#_new QBrush (#_new QColor 255 255 255 255))
                :accessor thumb-brush)
   (line-color :initform (#_new QColor 0 0 0 255)
               :accessor line-color))
  (:metaclass qt-class)
  (:qt-superclass "QScrollBar")
  (:override
   ("paintEvent" paint-event)
   ("mousePressEvent" mouse-press-event)
   ("mouseMoveEvent" mouse-move-event)))

(defmethod initialize-instance :after ((instance scrollbar) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (orientation) instance
    (case orientation
      (:vertical
       (progn
         (#_setOrientation instance (#_Vertical "Qt"))
         (#_setFixedWidth instance *scrollbarwidth*)))
      (t
       (progn
         (#_setOrientation instance (#_Horizontal "Qt"))
         (#_setFixedHeight instance *scrollbarwidth*)))))
  (#_setStyleSheet instance
                      "border: 2px solid #838383; cursor-color: white; border-radius: 5px; selection-background-color: white")
  (#_setMinimum instance 0)
  (#_setMaximum instance 10000))

(defmethod paint-event ((instance scrollbar) ev)
  (declare (ignore ev))
  (let* ((width (#_width instance))
         (height (#_height instance))
         (max (#_maximum instance))
         (min (#_minimum instance))
         (prop (float (/ (- (#_value instance) min)
                         (- max min)))))
    (with-painter (painter instance)
      (#_setRenderHint painter (#_Antialiasing "QPainter"))
      (#_eraseRect painter (#_rect instance))
      (with-paint-path (bg-path)
        (#_addRoundedRect bg-path (#_new QRectF (#_rect instance)) 5 5)
        (#_setColor (#_pen painter) (bg-pen-color instance))
        (#_setWidth (#_pen painter) 2) ;;; border-color of scroll-background
        (#_fillPath painter bg-path (bg-brush instance)) ;;; background-color of scroll-background
        (#_drawPath painter bg-path))
      (with-paint-path (thumb-path)
        (let (thumb-rect
              thumb-line-coords)
          (case (orientation instance)
            (:vertical
             (let ((val-pos (round (+ 2 (* prop (- height 4))))))
               (setf thumb-rect (#_new QRect 0 (max 1 (- val-pos 10))
                                       14 (+ 9 (min 10 val-pos (- height val-pos)))))
               (setf thumb-line-coords `(4 ,val-pos 11 ,val-pos))))
            (t
             (let ((val-pos (round (+ 2 (* prop (- width 4))))))
               (setf thumb-rect (#_new QRect (max 1 (- val-pos 10)) 0
                                       (+ 9 (min 10 val-pos (- width val-pos))) 14))
               (setf thumb-line-coords `(,val-pos 4 ,val-pos 11)))))
          (#_addRoundedRect thumb-path (#_new QRectF thumb-rect) 2 2)
          (#_setColor (#_pen painter) (thumb-pen-color instance))
          (#_fillPath painter thumb-path (thumb-brush instance))
          (#_drawPath painter thumb-path)
          (#_setColor (#_pen painter) (line-color instance))
          (#_setWidth (#_pen painter) 2)
          (destructuring-bind (x1 y1 x2 y2) thumb-line-coords
            (#_drawLine painter x1 y1 x2 y2)))))))

(defmethod mouse-press-event ((instance scrollbar) ev)
  (case (orientation instance)
    (:vertical (#_setValue
                instance
                (round (+ (#_minimum instance)
                          (* (- (#_maximum instance)
                                (#_minimum instance))
                             (/ (#_y ev) (#_height instance)))))))
    (t (#_setValue
                instance
                (round (+ (#_minimum instance)
                          (* (- (#_maximum instance)
                                (#_minimum instance))
                             (/ (#_x ev) (#_width instance)))))))))

(defmethod mouse-move-event ((instance scrollbar) ev)
  (case (orientation instance)
    (:vertical (#_setValue
                instance
                (round (+ (#_minimum instance)
                          (* (- (#_maximum instance)
                                (#_minimum instance))
                             (/ (#_y ev) (#_height instance)))))))
    (t (#_setValue
        instance
        (round (+ (#_minimum instance)
                  (* (- (#_maximum instance)
                        (#_minimum instance))
                     (/ (#_x ev) (#_width instance)))))))))
