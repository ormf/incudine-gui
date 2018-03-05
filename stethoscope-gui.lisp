;;;; stethoscope-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(defun find-node (node)
  (incudine:dograph (n)
    (when (equal node (incudine:node-id n))
      (return n))))

;; (assert-active (dsp-node-id (find-gui "Stethoscope")))

;;(incudine:dump (incudine:node 0))

(defun assert-active (node)
  (loop
     for n = incudine::*node-root* then (incudine::node-next n)
     while n
     until (equal node (incudine::node-id n))
     finally (return n)))

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

(define-override (stethoscope-view paint-event) (ev)
  (declare (ignore ev) (optimize (speed 3)))
  (let ((stethoscope (main-widget stethoscope-view)))
    (if (redraw? stethoscope)
        (if (slot-boundp stethoscope 'curr-bufs)
            (with-finalizing ((painter (q+:make-qpainter stethoscope-view)))
              (let* ((width (q+:width stethoscope-view))
                     (height (q+:height stethoscope-view))
                     (num-chans (num-chans (main-widget stethoscope-view)))
                     (size (bufsize (main-widget stethoscope-view)))
                     (y-scale (* height 0.5
                                 (- 1 (/ (q+:value (scroll-y (steth-view-pane (main-widget stethoscope-view)))) 10000)))))
                (declare (fixnum width height num-chans size))
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
                     (dotimes (i (length (curr-bufs (main-widget stethoscope-view))))
                       (let ((y-pos (round (* (+ 0.5 i) (/ height num-chans))))
                             (buf (aref (curr-bufs (main-widget stethoscope-view)) i)))
                         (let* ((paint-path (q+:make-QPainterPath)))
                           (q+:move-to paint-path width y-pos)
                           (q+:line-to paint-path 0 y-pos)
                           (dotimes (x num-points)
                             (let ((amp (round (* y-scale (incudine:buffer-value buf (round (* x idx-inc)))))))
                               (q+:line-to paint-path (* x x-inc)
                                           (+ y-pos amp))))
                           (q+:close-subpath paint-path)
                           (q+:draw-path painter paint-path)
                           (q+:fill-path painter paint-path (q+:make-qbrush (q+:make-qcolor 165 141 0) (q+:qt.solid-pattern))))))))
                  (:overlay
                   (let* ((num-points (min width size))
                          (x-inc (/ width num-points))
                          (idx-inc (/ size num-points)))
                     (dotimes (i (length (curr-bufs (main-widget stethoscope-view))))
                       (let ((y-pos (round (* 0.5 height)))
                             (buf (aref (curr-bufs (main-widget stethoscope-view)) i)))
                         (let* ((paint-path (q+:make-QPainterPath)))
                           (q+:move-to paint-path width y-pos)
                           (q+:line-to paint-path 0 y-pos)
                           (dotimes (x num-points)
                             (let ((amp (round (* y-scale (incudine:buffer-value buf (round (* x idx-inc)))))))
                               (q+:line-to paint-path (* x x-inc)
                                           (+ y-pos amp))))
                           (q+:close-subpath paint-path)
                           (q+:draw-path painter paint-path)
                           (q+:fill-path painter paint-path (q+:make-qbrush (q+:make-qcolor 165 141 0) (q+:qt.solid-pattern))))))))
                  (:xy ))))
            (warn "bufs not bound!")))))

;;(length (num-bufs (main-widget stethoscope-view)))

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

(define-widget stethoscope (QTextEdit cudagui-tl-mixin)
  ((dsp-node-id :initform nil :accessor dsp-node-id)
   (key-event :initform nil :accessor key-event)
   (process? :initform t :accessor process?)
   (dsp-group :initform nil :initarg :group :accessor dsp-group)
   (steth-ctl :initform (make-instance 'stethoscope-ctl) :accessor steth-ctl)
   (steth-view-pane :initform (make-instance 'stethoscope-view-pane)
                    :accessor steth-view-pane)
   (bufs-a :accessor bufs-a :type '(simple-array buffer (*)))
   (bufs-b :accessor bufs-b :type '(simple-array buffer (*)))
   (curr-bufs :accessor curr-bufs :type '(simple-array buffer (*)))
   (redraw? :accessor redraw? :initform nil)
   (num-chans :initform 2 :initarg :num-chans :accessor num-chans)
   (bus-num :initform 0 :initarg :bus-num :accessor bus-num)
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
;;;  (setf key-event (q+:install-event-filter *controller* stethoscope))
  (q+:set-geometry stethoscope 30 30 480 480)
  (restart-stethoscope-dsp stethoscope :group dsp-group)
  (setf (redraw? stethoscope) t))

(define-override (stethoscope close-event) (ev)
  (declare (ignore ev))
  (setf (redraw? stethoscope) nil)
  (remove-gui (id stethoscope))
  (and (find-node dsp-node-id) (incudine:free dsp-node-id))
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

(defun restart-stethoscope-dsp (stethoscope &key (group 400))
  (with-slots (num-chans bufmaxsize bufs-a bufs-b curr-bufs) stethoscope
    (mapc (lambda (slot) (and (slot-boundp stethoscope slot)
                         (incudine:free (slot-value stethoscope slot))))
          '(bufs-a bufs-b))
    (setf bufs-a (make-array num-chans)
          bufs-b (make-array num-chans)
          curr-bufs bufs-a
          )
    (incudine::dochannels (idx num-chans)
      (setf (svref bufs-a idx) (incudine:make-buffer (1+ bufmaxsize) :real-time-p t))
      (setf (svref bufs-b idx) (incudine:make-buffer (1+ bufmaxsize) :real-time-p t))))
  (let ((node (dsp-node-id stethoscope)))
           (and (find-node node) (incudine:free node))
           (sleep 0.2)
           (scratch::scope-dsp (num-chans stethoscope) (bus-num stethoscope) stethoscope
                               (bufsize stethoscope)
                               (bufmaxsize stethoscope)
                               :action (lambda (n)
                                         (setf (dsp-node-id stethoscope) (incudine:node-id n)))
                               :tail group
                               :id (dsp-node-id stethoscope))))

(defmacro toggle (slot)
  `(setf ,slot (not ,slot)))

(define-slot (stethoscope toggle-dsp) ()
  (declare (connected stethoscope (toggle-dsp)))
  (incudine:set-control (dsp-node-id (find-gui "Stethoscope"))
                        :process? (toggle (process? stethoscope))))

(define-override (stethoscope key-press-event) (ev)
  (cond ;; Signal Ctl-Space pressed.
        ((= (q+:key ev) (q+:qt.key_space))
         (call-next-qmethod)
         (signal! stethoscope (toggle-dsp)))
        ;; Delegate standard.
        (T
         (call-next-qmethod))))

(define-slot (stethoscope num-tracks-changed) ((text string))
  (declare (connected
            (num-chans-box (steth-ctl stethoscope))
            (text-changed string)))
  (setf (redraw? stethoscope) nil)
  (setf (num-chans stethoscope) (textedit-parse-integer text (chans-minval stethoscope)))
  (restart-stethoscope-dsp stethoscope)
  (setf (redraw? stethoscope) t)
  (q+:repaint (steth-view-pane stethoscope)))

(define-slot (stethoscope bus-num-changed) ((text string))
  (declare (connected
            (bus-num-box (steth-ctl stethoscope))
            (text-changed string)))
  (sleep 0.5)
  (setf (redraw? stethoscope) nil)
  (setf (bus-num stethoscope) (textedit-parse-integer text (bus-minval stethoscope)))
  (restart-stethoscope-dsp stethoscope)
  (setf (redraw? stethoscope) t)
  (q+:repaint (steth-view-pane stethoscope)))

(defun normalize (val min max)
  (/ (- val min) (- max min)))

(define-slot (stethoscope x-scroll-changed) ((value int))
  (declare (connected
            (scroll-x (steth-view-pane stethoscope))
            (value-changed int)))
  (let* ((scrollbar (scroll-x (steth-view-pane stethoscope)))
         (prop (normalize
                value
                (q+:minimum scrollbar)
                (q+:maximum scrollbar)))
         (new-size (round (+ 128 (* prop (- (bufmaxsize stethoscope) 128))))))
    (setf (bufsize stethoscope) new-size)
    (incudine:set-control (dsp-node-id stethoscope) :bufsize new-size)))

#|
(define-slot (stethoscope y-scroll-changed) ((value int))
  (declare (connected
             (scroll-y (steth-view-pane stethoscope))
            (value-changed int)))
  (let* ((scrollbar (scroll-y (steth-view-pane stethoscope)))
         (new-size (round (+ 128 (* prop (- (bufmaxsize stethoscope) 128))))))
    (setf (bufsize stethoscope) new-size)
    (incudine:set-control (dsp-node-id stethoscope) :bufsize new-size)))
|#

(define-signal (stethoscope repaint-view) ())

(define-slot (stethoscope repaint-view) ()
  (declare (connected
            stethoscope
            (repaint-view)))
  (q+:repaint (steth-view (steth-view-pane stethoscope))))

(defun scope (&key (id "Stethoscope") (group 400) (bus 8) (num-chans 2))
  (gui-funcall (create-tl-widget 'stethoscope id :group group :bus-num bus :num-chans num-chans)))

;;; (scope :num-chans 2)

#|

(remove-gui "Stethoscope")


(signal! (find-gui :stethoscope01) (repaint-view))

(incudine:set-control (dsp-node-id (find-gui "Stethoscope")) :bufsize 8192)

(setf (redraw? (find-gui "Stethoscope")) t)
(redraw? (find-gui "Stethoscope"))
(num-chans (find-gui "Stethoscope"))

(bufs-a (find-gui "Stethoscope"))
(bufs-b (find-gui "Stethoscope"))
(curr-bufs (find-gui "Stethoscope"))
(#_repaint (find-gui "Stethoscope"))

(setf
 (curr-bufs (find-gui "Stethoscope"))
 (bufs-a (find-gui "Stethoscope")))


(incudine:buffer-value (aref (bufs-b (find-gui "Stethoscope")) 0) 100)
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

;;; (setf (num-chans (find-gui "Stethoscope")) 6)00000000000000000000

;; (setf (redraw? (find-gui "Stethoscope")) nil)

;; (incudine:free (dsp-node-id (find-gui "Stethoscope")))


(restart-stethoscope-dsp (find-gui "Stethoscope"))

(incudine:set-control (dsp-node-id (find-gui "Stethoscope")) :process? t)

|#
