;;;; stethoscope-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(define-widget stethoscope-ctl (QDialog) ;;; top area of stethoscope
  ((num-chans-box :initform (make-instance 'numbox) :accessor num-chans-box)
   (bus-num-box :initform (make-instance 'numbox) :accessor bus-num-box)
   (mode-button :initform (make-instance 'pushbutton) :accessor mode-button)
   (tracks-action :accessor tracks-action)
   (overlay-action :accessor overlay-action)
   (xy-action :accessor xy-action)))

(define-subwidget (stethoscope-ctl layout) (q+:make-qhboxlayout stethoscope-ctl)
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
  (q+:set-fixed-width mode-button 85)
  (q+:set-fixed-height stethoscope-ctl 25))

(define-widget stethoscope-view (QWidget) ;;; the plot area
  ((style :initform "background-color: black;" :accessor style)
   (main-widget :accessor main-widget)))

(define-initializer (stethoscope-view setup)
  (q+:set-geometry stethoscope-view 50 50 400 400)
  (q+:set-style-sheet stethoscope-view style))

#|
(define-override (scrollbar paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter scrollbar)))
    (let* ((width (q+:width scrollbar))
           (height (q+:height scrollbar))
           (max (q+:maximum scrollbar))
           (min (q+:minimum scrollbar))
           (prop (float (/ (- (q+:value scrollbar) min)
                           (- max min)))))
      (q+:set-render-hint painter (#_Antialiasing "QPainter"))
      (q+:erase-rect painter (q+:rect scrollbar))
      (let ((bg-path (q+:make-QPainterPath)))
        (q+:add-Rounded-Rect bg-path (q+:make-qrectf (q+:rect scrollbar)) 5 5)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 131 131 131 255))
        (q+:set-width (q+:pen painter) 2) ;;; border-color of scroll-background
        (q+:fill-path painter bg-path (q+:make-qbrush (q+:make-qcolor 182 182 182 255))) ;;; background-color of scroll-background
        (q+:draw-path painter bg-path))
      (let ((thumb-path (q+:make-QPainterPath))
            thumb-line-coords
            thumb-rect)
        (case (orientation scrollbar)
          (:vertical
           (let ((val-pos (round (+ 2 (* prop (- height 4))))))
             (setf thumb-rect `(0 ,(max 1 (- val-pos 10))
                                  14 ,(+ 9 (min 10 val-pos (- height val-pos)))))
             (setf thumb-line-coords `(4 ,val-pos 11 ,val-pos))))
          (t
           (let ((val-pos (round (+ 2 (* prop (- width 4))))))
             (setf thumb-rect `(,(max 1 (- val-pos 10))
                                 0 ,(+ 9 (min 10 val-pos (- width val-pos))) 14))
             (setf thumb-line-coords `(,val-pos 4 ,val-pos 11)))))
        (q+:add-Rounded-Rect thumb-path (q+:make-qrectf (apply #'q+:make-qrect thumb-rect)) 2 2)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 153 153 153))
        (q+:fill-path painter thumb-path (q+:make-qbrush (q+:make-qcolor 255 255 255 255)))
        (q+:draw-path painter thumb-path)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 0 0 0 255))
        (q+:set-width (q+:pen painter) 2)
        (apply #'q+:draw-line painter thumb-line-coords)))))


(define-override (stethoscope-view paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter stethoscope-view)))
    (let ((width (q+:width stethoscope-view))
          (height (q+:height stethoscope-view))
          (num-chans (num-chans (main-widget stethoscope-view))))
       (setf (q+:background painter)
             (q+:make-qbrush (q+:make-qcolor 0 0 0 255) (q+:qt.solid-pattern)))
      (q+:erase-rect painter (q+:rect stethoscope-view))
      (q+:set-color (q+:pen painter) (q+:make-qcolor 255 218 0 255))
      (q+:set-brush (q+:pen painter) (q+:make-qbrush (q+:make-qcolor 255 218 0 255) (q+:qt.solid-pattern)))
      (q+:set-width (q+:pen painter) 1)
      (case (draw-mode main-widget)
        (:tracks
         (dotimes (i num-chans)
           (let ((y-pos (round (* (+ 0.5 i) (/ height num-chans)))))
             (q+:draw-line painter 0 y-pos width y-pos))))
        (:overlay
         (dotimes (i num-chans)
           (let ((y-pos (round (/ height 2))))
             (q+:draw-line painter 0 y-pos width y-pos))))
        (:xy )))))

|#


(define-override (stethoscope-view paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter stethoscope-view)))
    (let ((width (q+:width stethoscope-view))
          (height (q+:height stethoscope-view))
          (num-chans (num-chans (main-widget stethoscope-view)))
          (size (bufsize (main-widget stethoscope-view))))
       (setf (q+:background painter)
             (q+:make-qbrush (q+:make-qcolor 0 0 0 255) (q+:qt.solid-pattern)))
      (q+:erase-rect painter (q+:rect stethoscope-view))
      (q+:set-color (q+:pen painter) (q+:make-qcolor 255 218 0 255))
      (q+:set-width (q+:pen painter) 1)
      (case (draw-mode main-widget)
        (:tracks
         (let* ((num-points (min width size))
                (x-inc (/ width num-points))
                (idx-inc (/ size num-points)))
           (dotimes (i num-chans)
             (let ((y-pos (round (* (+ 0.5 i) (/ height num-chans))))
                   (buf (aref (bufs (main-widget stethoscope-view)) i)))
               (let* ((paint-path (q+:make-QPainterPath)))
                 (q+:move-to paint-path width y-pos)
                 (q+:line-to paint-path 0 y-pos)
                 (dotimes (x num-points)
                   (let ((amp (round (* 40 (incudine:buffer-value buf (round (* x idx-inc)))))))
                     (q+:line-to paint-path (* x x-inc)
                                 (+ y-pos amp))))
                 (q+:close-subpath paint-path)
                 (q+:draw-path painter paint-path)
                 (q+:fill-path painter paint-path (q+:make-qbrush (q+:make-qcolor 165 141 0) (q+:qt.solid-pattern))))))))
        (:overlay
         (dotimes (i num-chans)
           (let ((y-pos (round (/ height 2))))
             (q+:draw-line painter 0 y-pos width y-pos))))
        (:xy )))))

(define-widget stethoscope-view-pane (QWidget) ;;; plot area with scrollbars
  ((steth-view :initform (make-instance 'stethoscope-view) :accessor steth-view)
   (scroll-x :initform (make-instance 'scrollbar :orientation :horizontal) :accessor scroll-x)
   (scroll-y :initform (make-instance 'scrollbar :orientation :vertical) :accessor scroll-y)))

(define-subwidget (stethoscope-view-pane layout) (q+:make-qhboxlayout stethoscope-view-pane)
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

(define-widget stethoscope (QDialog cudagui-tl-mixin)
  ((dsp-node-id :initform nil :accessor dsp-node-id)
   (steth-ctl :initform (make-instance 'stethoscope-ctl) :accessor steth-ctl)
   (steth-view-pane :initform (make-instance 'stethoscope-view-pane)
                    :accessor steth-view-pane)
   (bufs :accessor bufs :type '(simple-array buffer (*)))
   (num-chans :initform 2 :accessor num-chans)
   (bus-num :initform 0 :accessor bus-num)
   (draw-mode :initform :tracks :accessor draw-mode)
   (zoom-x :initarg zoom-x :accessor zoom-x)
   (zoom-y :initarg zoom-y :accessor zoom-y)
   (chans-minval :initform 1 :accessor chans-minval)
   (chans-maxval :initform 64 :accessor chans-maxval)
   (bus-minval :initform 0 :accessor bus-minval)
   (bus-maxval :initform 24 :accessor bus-minval)
   (bufsize :initform 1024 :accessor bufsize)
   (bufmaxsize :initform 8192 :accessor bufmaxsize)))

(define-subwidget (stethoscope layout) (q+:make-qvboxlayout stethoscope)
  (q+:set-text (num-chans-box steth-ctl) (format nil "~a" num-chans))
  (q+:set-text (bus-num-box steth-ctl) (format nil "~a" bus-num))
  (setf (minval (num-chans-box steth-ctl)) chans-minval)
  (setf (maxval (num-chans-box steth-ctl)) chans-maxval)
  (setf (minval (bus-num-box steth-ctl)) bus-minval)
  (setf (maxval (bus-num-box steth-ctl)) bus-maxval)
  (q+:set-margin layout 2)
  (q+:set-spacing layout 2)
  (q+:add-widget layout steth-ctl)
  (q+:add-widget layout steth-view-pane))

(define-initializer (stethoscope setup)
  (let ((*background-color* "background-color: #dbdbdb;"))
    (cudagui-tl-initializer stethoscope))
  (setf (main-widget (steth-view steth-view-pane)) stethoscope)
  (q+:set-geometry stethoscope 30 30 480 480)
  (restart-stethoscope-dsp stethoscope))

(define-override (stethoscope close-event) (ev)
  (declare (ignore ev))
  (incudine:free dsp-node-id)
  (remove-gui (id stethoscope))
  (call-next-qmethod))

(define-slot (stethoscope tracks-action) ()
  (declare (connected
            (tracks-action (steth-ctl stethoscope))
            (triggered)))
  (setf (draw-mode stethoscope) :tracks)
  (q+:set-text (mode-button (steth-ctl stethoscope)) "Tracks")
  (q+:repaint (steth-view-pane stethoscope)))

(define-slot (stethoscope overlay-action) ()
  (declare (connected
            (overlay-action (steth-ctl stethoscope))
            (triggered)))
  (setf (draw-mode stethoscope) :overlay)
  (q+:set-text (mode-button (steth-ctl stethoscope)) "Overlay")
  (q+:repaint (steth-view-pane stethoscope)))

(define-slot (stethoscope xy-action) ()
  (declare (connected
            (xy-action (steth-ctl stethoscope))
            (triggered)))
  (unless (>= (num-chans stethoscope) 2)
      (warn "XY mode needs at least 2 chans to work properly"))
  (progn
    (setf (draw-mode stethoscope) :xy)
    (q+:set-text (mode-button (steth-ctl stethoscope)) "X/Y")
    (q+:repaint (steth-view-pane stethoscope))))

(defun restart-stethoscope-dsp (stethoscope)
  (setf (dsp-node-id stethoscope) (incudine:next-node-id))
  (scratch::scope-dsp (num-chans stethoscope) (bus-num stethoscope) stethoscope
                      (bufsize stethoscope)
                      (bufmaxsize stethoscope)
                      :id (dsp-node-id stethoscope)))

(define-slot (stethoscope num-tracks-changed) ((text string))
  (declare (connected
            (num-chans-box (steth-ctl stethoscope))
            (text-changed string)))
  (and (dsp-node-id stethoscope) (incudine:free (dsp-node-id stethoscope)))
  (sleep 0.5)
  (setf (num-chans stethoscope) (textedit-parse-integer text (chans-minval stethoscope)))
  (restart-stethoscope-dsp stethoscope)
  (q+:repaint (steth-view-pane stethoscope)))

(define-slot (stethoscope bus-num-changed) ((text string))
  (declare (connected
            (bus-num-box (steth-ctl stethoscope))
            (text-changed string)))
  (and (dsp-node-id stethoscope) (incudine:free (dsp-node-id stethoscope)))
  (sleep 0.5)
  (setf (bus-num stethoscope) (textedit-parse-integer text (bus-minval stethoscope)))
  (restart-stethoscope-dsp stethoscope)
  (q+:repaint (steth-view-pane stethoscope)))

(define-signal (stethoscope repaint-view) ())

(define-slot (stethoscope repaint-view) ()
  (declare (connected
            stethoscope
            (repaint-view)))
  (q+:repaint (steth-view (steth-view-pane stethoscope))))

(defun scope (&key (id "Stethoscope"))
  (gui-funcall (create-tl-widget 'stethoscope id)))

;;; (scope)

#|
(signal! (find-gui :stethoscope01) (repaint-view))

|#

;;; (gui-funcall (create-tl-widget 'stethoscope "stethoscope02"))
;;; (gui-funcall (create-tl-widget 'stethoscope :stethoscope01))


;; (buffer-value (aref (cuda-gui::bufs (cuda-gui::find-gui "stethoscope02")) 0) 200)
;; (incudine:buffer-value (aref (cuda-gui::bufs (cuda-gui::find-gui :stethoscope01)) 0) 200)


;;; (#_close (find-gui "stethoscope"))
;;; (#_hide (find-gui "stethoscope"))
;;; (#_show (find-gui "stethoscope"))

;;; (type-of (find-gui "stethoscope02"))

;;;(style (steth-view (steth-view-pane (find-gui "stethoscope"))))

;;; (num-chans (find-gui "stethoscope"))

;;; (setf (num-chans (find-gui "stethoscope")) 3)
