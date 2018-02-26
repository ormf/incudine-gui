;;;; sthethoscope.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(define-widget stethoscope-ctl (QDialog)
  ((num-chans-box :accessor num-chans-box)
   (bus-num-box :accessor bus-num-box)
   (mode-button :accessor mode-button)
   (chans-minval :initform 0 :accessor chans-minval)
   (chans-maxval :initform 26 :accessor chans-maxval)
   (bus-minval :initform 0 :accessor bus-minval)
   (bus-maxval :initform 24 :accessor bus-minval)
   (tracks-action :accessor tracks-action)
   (overlay-action :accessor overlay-action)
   (xy-action :accessor xy-action)))

(define-subwidget (stethoscope-ctl layout) (q+:make-qhboxlayout stethoscope-ctl)
  (setf num-chans-box (make-instance 'numbox :minval chans-minval :maxval chans-maxval))
  (setf bus-num-box (make-instance 'numbox :minval bus-minval :maxval bus-maxval))
  (setf mode-button (make-instance 'pushbutton))
  (q+:set-fixed-width mode-button 85)
  (let ((menu (q+:make-qmenu)))
    (setf tracks-action (q+:make-qaction "Tracks" menu))
    (setf overlay-action (q+:make-qaction "Overlay" menu))
    (setf xy-action (q+:make-qaction "X/Y" menu))
    (q+:add-action menu tracks-action)
    (q+:add-action menu overlay-action)
    (q+:add-action menu xy-action)
    (q+:set-menu mode-button menu)
    (q+:set-text mode-button "Tracks"))
  (q+:add-widget layout bus-num-box)
  (q+:add-widget layout num-chans-box)
  (q+:add-stretch layout)
  (q+:add-widget layout mode-button)
  (q+:add-widget layout (make-instance 'scroll-corner-box))
  (q+:set-margin layout 0)
  (q+:set-spacing layout 2)
  (q+:set-fixed-height stethoscope-ctl 25))

(define-widget stethoscope-view (QWidget)
  ((style :initform "background-color: black;" :accessor style)
   (draw-mode :initform :tracks :accessor draw-mode)
   (main-widget :accessor main-widget)))

(define-initializer (stethoscope-view setup)
  (q+:set-geometry stethoscope-view 50 50 400 400)
  (q+:set-style-sheet stethoscope-view style))

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
      (case draw-mode
        (:tracks
         (dotimes (i 3)
           (let ((y-pos (round (* (+ 0.5 i) (/ height 3)))))
             (q+:draw-line painter 0 y-pos width y-pos))))
        (:overlay
         (dotimes (i 1)
           (let ((y-pos (round (/ height 2))))
             (q+:draw-line painter 0 y-pos width y-pos))))
        (:xy )))))

(define-widget stethoscope-view-pane (QWidget)
  ((steth-view :accessor steth-view)
   (scroll-x :accessor scroll-x)
   (scroll-y :accessor scroll-y)))

#|

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
        (inner2 (q+:make-qvboxlayout)))
    (q+:set-margin inner1 0)
    (q+:set-spacing inner1 2)
    (q+:set-margin inner2 0)
    (q+:set-spacing inner2 2)
    (q+:add-widget inner1 steth-view)
    (q+:add-widget inner1 scroll-x)
    (q+:add-layout layout inner1)
    (q+:add-widget inner2 scroll-y)
    (q+:add-widget inner2 (make-instance 'scroll-corner-box))
    (q+:add-layout layout inner2)))


#|

(define-subwidget (login layout) (q+:make-qvboxlayout login)
  (setf (q+:window-title login) "Login to Twitter")
  (q+:add-widget layout url)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner pin)
    (q+:add-widget inner go)
    (q+:add-layout layout inner)))

(define-signal (levelmeter set-level) (int))

(define-slot (levelmeter set-level) ((value int))
  (declare (connected
            levelmeter
            (set-level int)))
   (setf (level levelmeter) value)
   (q+:repaint levelmeter))

(defun change-level (levelmeter value)
  (signal! levelmeter (set-level int) value))

|#

(define-widget stethoscope (QDialog cudagui-tl-mixin)
  ((num-chans :initform 1 :accessor num-chans)
   (bus-num :initform 0 :accessor bus-num)
   (zoom-x :initarg zoom-x :accessor zoom-x)
   (zoom-y :initarg zoom-y :accessor zoom-y)
   (steth-ctl :accessor steth-ctl)
   (steth-view-pane :accessor steth-view-pane)))

(define-subwidget (stethoscope layout) (q+:make-qvboxlayout stethoscope)
  (setf steth-ctl (make-instance 'stethoscope-ctl))
  (setf steth-view-pane (make-instance 'stethoscope-view-pane))
;;  (setf (main-widget (steth-view steth-view-pane)) stethoscope)
  (q+:set-margin layout 2)
  (q+:set-spacing layout 2)
  (q+:add-widget layout steth-ctl)
  (q+:add-widget layout steth-view-pane))

(define-initializer (stethoscope setup)
  (let ((*background-color* "background-color: #dbdbdb;"))
    (cudagui-tl-initializer stethoscope))
  (q+:set-geometry stethoscope 30 30 480 480))

(define-override (stethoscope close-event) (ev)
  (declare (ignore ev))
  (remove-gui (id stethoscope))
  (call-next-qmethod))

(define-slot (stethoscope tracks-action) ()
  (declare (connected
            (tracks-action (steth-ctl stethoscope))
            (triggered)))
  (setf (draw-mode (steth-view (steth-view-pane stethoscope))) :tracks)
  (q+:set-text (mode-button (steth-ctl stethoscope)) "Tracks")
  (q+:repaint (steth-view-pane stethoscope)))

(define-slot (stethoscope overlay-action) ()
  (declare (connected
            (overlay-action (steth-ctl stethoscope))
            (triggered)))
  (setf (draw-mode (steth-view (steth-view-pane stethoscope))) :overlay)
  (q+:set-text (mode-button (steth-ctl stethoscope)) "Overlay")
  (q+:repaint (steth-view-pane stethoscope)))

(define-slot (stethoscope xy-action) ()
  (declare (connected
            (xy-action (steth-ctl stethoscope))
            (triggered)))
  (setf (draw-mode (steth-view (steth-view-pane stethoscope))) :xy)
  (q+:set-text (mode-button (steth-ctl stethoscope)) "X/Y")
  (q+:repaint (steth-view-pane stethoscope)))

#|
(define-slot (stethoscope num-tracks-changed) ((value int))
  (declare (connected
            ( (steth-ctl stethoscope))
            (triggered)))
  (setf (draw-mode (steth-view (steth-view-pane stethoscope))) :xy)
  (q+:set-text (mode-button (steth-ctl stethoscope)) "X/Y")
  (q+:repaint (steth-view-pane stethoscope)))
|#

;;; (gui-funcall (create-tl-widget 'stethoscope "stethoscope"))

;;; (#_close (find-gui "stethoscope"))
;;; (#_hide (find-gui "stethoscope"))
;;; (#_show (find-gui "stethoscope"))

;;;(style (steth-view (steth-view-pane (find-gui "stethoscope"))))

