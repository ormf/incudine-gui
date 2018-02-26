;;;; sthethoscope.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(define-widget stethoscope-ctl (QDialog)
  ((num-chans :initform 1 :accessor num-chans)
   (bus-num :initform 0 :accessor bus-num)
   (num-chans-box :accessor num-chans-box)
   (bus-num-box :accessor bus-num-box)
   (chans-minval :initform 0 :accessor chans-minval)
   (chans-maxval :initform 26 :accessor chans-maxval)
   (bus-minval :initform 0 :accessor bus-minval)
   (bus-maxval :initform 24 :accessor bus-minval))
  )

(define-subwidget (stethoscope-ctl layout) (q+:make-qhboxlayout stethoscope-ctl)
  (setf num-chans-box (make-instance 'numbox :minval chans-minval :maxval chans-maxval))
  (setf bus-num-box (make-instance 'numbox :minval bus-minval :maxval bus-maxval))
  (q+:add-widget layout bus-num-box)
  (q+:add-widget layout num-chans-box)
  (q+:add-stretch layout)
  (q+:set-margin layout 0)
  (q+:set-spacing layout 2)
  (q+:set-fixed-height stethoscope-ctl 25))

(define-slot ((num-chans-box stethoscope)))

;;; (setf numbox (make-instance 'numbox :minval minval :maxval maxval))
;;; (q+:add-widget numbox-test numbox)

(define-widget stethoscope-view (QWidget)
  ((style :initform "background-color: black;" :accessor style)))

(define-initializer (stethoscope-view setup)
  (q+:set-geometry stethoscope-view 50 50 400 400)
  (q+:set-style-sheet stethoscope-view style)
;;  (q+:set-auto-fill-background stethoscope-view t)
)

(define-override (stethoscope-view paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter stethoscope-view)))
    (let ((width (q+:width stethoscope-view))
          (height (q+:height stethoscope-view)))
       (setf (q+:background painter)
             (q+:make-qbrush (q+:make-qcolor 0 0 0 255) (q+:qt.solid-pattern)))
      (q+:erase-rect painter (q+:rect stethoscope-view))
      (q+:set-color (q+:pen painter) (q+:make-qcolor 255 218 0 255))
      (q+:set-width (q+:pen painter) 1)
      (dotimes (i 3)
        (let ((y-pos (round (* (+ 0.5 i) (/ height 3)))))
          (q+:draw-line painter 0 y-pos width y-pos)))
      )))

(define-widget stethoscope-view-pane (QWidget)
  ((zoom-x :initarg zoom-x :accessor zoom-x)
   (zoom-y :initarg zoom-y :accessor zoom-y)
   (steth-view :accessor steth-view)
   (scroll-x :accessor scroll-x)
   (scroll-y :accessor scroll-y)))

(defparameter *scroll-bar-style* "QScrollBar::add-line {
      border: none;
      background: none;
}

QScrollBar::sub-line {
      border: none;
      background: none;
}

"

  )

#|
QScrollBar::add-line:vertical {
      margin: 0 0 0 15;
      border: none;
      background: none;
}
(define-subwidget (stethoscope-view-pane layout) (q+:make-qhboxlayout stethoscope-view-pane)
  (setf steth-view (make-instance 'stethoscope-view))
  (setf scroll-x (q+:make-qscrollbar (#_Horizontal "Qt") stethoscope-view-pane))
  (setf scroll-y (q+:make-qscrollbar (#_Vertical "Qt") stethoscope-view-pane))
  (q+:set-fixed-height scroll-x 15)
  (q+:set-fixed-width scroll-y 15)
  (q+:set-style-sheet scroll-x *scroll-bar-style*)
  (q+:set-style-sheet scroll-y *scroll-bar-style*)
  (q+:set-margin layout 0)
  (q+:set-spacing layout 2)
  (let ((inner (q+:make-qvboxlayout)))
    (q+:set-margin inner 0)
    (q+:set-spacing inner 2)
    (q+:add-widget inner steth-view)
    (q+:add-widget inner scroll-x)
    (q+:add-layout layout inner)
    (q+:add-widget layout scroll-y)))
|#

(define-subwidget (stethoscope-view-pane layout) (q+:make-qhboxlayout stethoscope-view-pane)
  (setf steth-view (make-instance 'stethoscope-view))
  (setf scroll-x (make-instance 'scrollbar :orientation :horizontal))
  (setf scroll-y (make-instance 'scrollbar :orientation :vertical))
  (q+:set-margin layout 0)
  (q+:set-spacing layout 2)
  (let ((inner1 (q+:make-qvboxlayout))
        (inner2 (q+:make-qvboxlayout))
        (cornerbox (q+:make-qwidget)))
    (q+:set-fixed-height cornerbox 15)
    (q+:set-fixed-width cornerbox 15)
    (q+:set-margin inner1 0)
    (q+:set-spacing inner1 2)
    (q+:set-margin inner2 0)
    (q+:set-spacing inner2 2)
    (q+:add-widget inner1 steth-view)
    (q+:add-widget inner1 scroll-x)
    (q+:add-layout layout inner1)
    (q+:add-widget inner2 scroll-y)
    (q+:add-widget inner2 cornerbox)
    (q+:add-layout layout inner2)))


#|

(define-subwidget (login layout) (q+:make-qvboxlayout login)
  (setf (q+:window-title login) "Login to Twitter")
  (q+:add-widget layout url)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner pin)
    (q+:add-widget inner go)
    (q+:add-layout layout inner)))

|#

(define-widget stethoscope (QDialog cudagui-tl-mixin)
  ((minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)
   (steth-ctl :accessor steth-ctl)
   (steth-view :accessor steth-view)))

(define-subwidget (stethoscope layout) (q+:make-qvboxlayout stethoscope)
  (setf steth-ctl (make-instance 'stethoscope-ctl))
  (setf steth-view (make-instance 'stethoscope-view-pane))
  (q+:set-margin layout 2)
  (q+:set-spacing layout 2)
  (q+:add-widget layout steth-ctl)
  (q+:add-widget layout steth-view))

(define-initializer (stethoscope setup)
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer stethoscope))
  (q+:set-geometry stethoscope 30 30 480 480))

(define-override (stethoscope close-event) (ev)
  (declare (ignore ev))
  (remove-gui (id stethoscope))
  (call-next-qmethod))

;;; (gui-funcall (create-tl-widget 'stethoscope "stethoscope"))

;;; (remove-gui "num01")
