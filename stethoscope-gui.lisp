;;;; stethoscope-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

;;;
;;; stethoscope-ctl
;;;
;;; top row of stethoscope window with bus and view controls

(defclass stethoscope-ctl () 
  ((num-chans-box :initform (make-instance 'numbox) :accessor num-chans-box)
   (bus-num-box :initform (make-instance 'numbox) :accessor bus-num-box)
   (mode-button :initform (make-instance 'pushbutton) :accessor mode-button)
   (io-button :initform (make-instance 'pushbutton) :accessor io-button)
   (tracks-action :accessor tracks-action)
   (overlay-action :accessor overlay-action)
   (xy-action :accessor xy-action)
   (audio-in-action :accessor audio-in-action)
   (audio-out-action :accessor audio-out-action)
   (bus-action :accessor bus-action)
   (layout :accessor layout))
  (:metaclass qt-class)
  (:qt-superclass "QDialog"))

(defmacro assign-menu-action (slot menu action)
  `(progn
     (setf ,slot ,action)
     (#_addAction ,menu ,slot)))

(defmethod initialize-instance :after ((instance stethoscope-ctl) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (layout audio-in-action audio-out-action bus-action tracks-action
                      overlay-action xy-action io-button mode-button
                      bus-num-box num-chans-box) instance
    (make-button-menu io-button :actions
                      ((audio-in-action "Audio In")
                       (audio-out-action "Audio Out")
                       (bus-action "Bus")))
    (make-button-menu mode-button :actions
                      ((tracks-action "Tracks")
                       (overlay-action  "Overlay")
                       (xy-action "X/Y")))
    (setf layout (#_new QHBoxLayout instance))
    (#_addWidget layout bus-num-box)
    (#_addWidget layout num-chans-box)
    (#_addWidget layout io-button)
    (#_addStretch layout)
    (#_addWidget layout mode-button)
    (#_addWidget layout (make-instance 'scroll-corner-box))
    (#_setMargin layout 0)
    (#_setSpacing layout 2)
    (#_setFixedWidth mode-button 85)
    (#_setFixedWidth io-button 95)
    (#_setFixedHeight instance 25)))

;;;
;;; stethoscope-view
;;;

(defclass stethoscope-view () ;;; the plot area
  ((style :initform "background-color: black;" :accessor style)
   (fill? :initform t :accessor fill?)
   (main-widget :accessor main-widget)
   (painter :accessor painter :type 'qobject)
   (pen :accessor pen :type 'qobject)
   (background-brush  :accessor background-brush)
   (foreground-color :initform (#_new QColor 255 218 0 255) :accessor foreground-color)
   (fill-brush :initform (#_new QBrush (#_new QColor 165 141 0) (#_Qt::SolidPattern)) :accessor foreground-color)
   (paint-path :accessor paint-path)
   (empty-path :accessor empty-path))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("paintEvent" paint-event)))

(defmethod initialize-instance :after ((instance stethoscope-view) &key parent)
  (if parent
      (new instance parent)
      (new instance))  
  (#_setGeometry instance 50 50 400 400)
  (#_setStyleSheet instance (style instance))
  (setf (painter instance) (#_new QPainter instance))
  (setf (pen instance) (#_pen (painter instance)))
  (setf (paint-path instance) (#_new QPainterPath))
  (setf (empty-path instance) (#_new QPainterPath))
  (setf (background-brush instance) (#_new QBrush (#_new QColor 0 0 0 255) (#_Qt::SolidPattern))))

(declaim (inline draw-scope))
(defun draw-scope(num-points idx-inc x-inc y-pos y-scale buf paint-path)
  (declare (optimize speed (safety 0))
           (double-float idx-inc x-inc y-scale y-pos)
           ((integer 0 10000) num-points)
           (qobject paint-path))
  (dotimes (x num-points)
    (let* ((amp (* y-scale
                   (incudine:buffer-value
                    buf
                    (incudine::sample->fixnum (* x idx-inc) :roundp t))))
           (x-eff (float (* x x-inc) 1.0d0))
           (y-eff (float (+ y-pos amp) 1.0d0)))
      (declare (double-float amp x-eff y-eff))
      (qt::fast-lineto paint-path x-eff y-eff))))

(defmacro with-pen ((pen) painter &body body)
  `(let ((,pen (#_pen ,painter)))
     ,@body))

(defmethod paint-event ((instance stethoscope-view) ev)
  (declare (ignore ev) (optimize (speed 3)))
  (let ((stethoscope (main-widget instance)))
    (if (redraw? stethoscope)
        (if (slot-boundp stethoscope 'curr-bufs)
            (with-slots (painter pen background-brush foreground-color
                                 fill-brush main-widget fill?)
                instance
              (let* ((width (#_width instance)) 
                     (height (#_height instance))
                     (dwidth (float width 1.0d0))
                     (dheight (float height 1.0d0))
                     (num-chans (num-chans (main-widget instance)))
                     (size (bufsize (main-widget instance)))
                     (y-scale (* dheight -0.5d0
                                 (- 1.0d0 (/ (float (#_value (scroll-y (steth-view-pane
                                                                        (main-widget instance)))))
                                             10000.0d0)))))
                (declare (fixnum width height num-chans size)
                         (double-float y-scale))
                (qt::fast-begin painter instance)
                (#_eraseRect painter (#_rect instance))
                (with-pen (pen) painter
                          (#_setColor pen foreground-color)
                          (#_setWidth pen 1))
                (case (draw-mode main-widget)
                  (:tracks
                   (let* ((num-points (min width size))
                          (x-inc (/ dwidth num-points))
                          (idx-inc (/ (float size 1.0d0) num-points)))
                     (declare (integer num-points)
                              (double-float idx-inc x-inc))
                     (if t
                         (dotimes (i (length (curr-bufs (main-widget instance))))
                           (let ((y-pos (* (+ 0.5 i) (/ dheight num-chans)))
                                 (buf (incudine::svref (curr-bufs (main-widget instance)) i)))
                             (with-paint-path (paint-path)
                               (qt::fast-moveTo paint-path dwidth y-pos)
                               (qt::fast-lineto paint-path 0.0d0 y-pos)
                               (draw-scope num-points idx-inc x-inc
                                           y-pos y-scale buf paint-path)
                               (#_closeSubpath paint-path)
                               (#_drawPath painter paint-path)
                                (if fill? (#_fillPath painter paint-path fill-brush))
                               ))))))
                  (:overlay
                   (let* ((num-points (min width size))
                          (x-inc (/ (float width 1.0d0) num-points))
                          (idx-inc (/ (float size 1.0d0) num-points)))
                     (declare ((integer 0 100000) num-points)
                              (double-float idx-inc x-inc))
                     (dotimes (i (length (curr-bufs (main-widget instance))))
                       (let ((y-pos (* 0.5 dheight))
                             (buf (incudine::svref (curr-bufs (main-widget instance)) i)))
                         (declare (incudine::buffer-base buf))
                         (with-paint-path (paint-path)
                           (qt::fast-moveto paint-path dwidth y-pos)
                           (qt::fast-lineto paint-path 0.0d0 y-pos)
                           (dotimes (x num-points)
                             (let ((amp (* y-scale
                                           (incudine:buffer-value
                                            buf (the (integer 0 100000)
                                                     (incudine::sample->fixnum (* x idx-inc)))))))
                               (qt::fast-lineTo paint-path (* x x-inc)
                                         (+ y-pos amp))))
                           (#_closeSubpath paint-path)
                           (#_drawPath painter paint-path)
                           (if fill? (#_fillPath painter paint-path fill-brush)))))))
                  (:xy (let* ((num-points (min width size)))
                     (declare ((integer 0 100000) num-points))
                     (if (> (length (curr-bufs (main-widget instance))) 1)
                             (let ((x-buf (incudine::svref (curr-bufs (main-widget instance)) 0))
                                   (y-buf (incudine::svref (curr-bufs (main-widget instance)) 1)))
                               (with-paint-path (paint-path)
                                 (let* ((x-offs (/ dwidth 2))
                                        (y-offs (/ dheight 2)))
                                   (qt::fast-moveTo paint-path
                                             (+ x-offs (* y-scale (incudine:buffer-value x-buf 0)))
                                             (+ y-offs (* y-scale (incudine:buffer-value y-buf 0))))
                                   (dotimes (idx num-points)
                                     (qt::fast-lineTo paint-path
                                               (+ x-offs (* y-scale (incudine:buffer-value x-buf idx)))
                                               (+ y-offs (* y-scale (incudine:buffer-value y-buf idx)))))
                                   (#_closeSubpath paint-path)
                                   (#_drawPath painter paint-path)
;;; (if fill? (#_fill-path painter paint-path fill-brush))
                                   )))))))
                (#_end painter)))
            (warn "bufs not bound!")))))

;;;
;;; stethoscope-view-pane
;;;

(defclass stethoscope-view-pane () ;;; plot area with scrollbars
  ((steth-view :initform (make-instance 'stethoscope-view) :accessor steth-view)
   (scroll-x :initform (make-instance 'scrollbar :orientation :horizontal) :accessor scroll-x)
   (scroll-y :initform (make-instance 'scrollbar :orientation :vertical) :accessor scroll-y)
   (layout :accessor layout))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((instance stethoscope-view-pane) &key parent)
  (if parent
      (new instance parent)
      (new instance))  
  (with-slots (layout steth-view scroll-x scroll-y) instance
    (setf layout (#_new QHBoxLayout instance))
    (#_setMargin layout 0)
    (#_setSpacing layout 2)
    (let ((inner1 (#_new QVBoxLayout))
          (inner2 (#_new QVBoxLayout)))
      (#_setMargin inner1 0)
      (#_setSpacing inner1 2)
      (#_setMargin inner2 0)
      (#_setSpacing inner2 2)
      (#_addWidget inner1 steth-view)
      (#_addWidget inner1 scroll-x)
      (#_addLayout layout inner1)
      (#_addWidget inner2 scroll-y)
      (#_addWidget inner2 (make-instance 'scroll-corner-box))
      (#_addLayout layout inner2))))

;;;
;;; stethoscope
;;;

(defclass stethoscope (cudagui-tl-mixin)
  ((dsp-node-id :initform nil :accessor dsp-node-id)
   (key-event :initform nil :accessor key-event)
   (process? :initform t :accessor process?)
   (dsp-group :initform nil :initarg :group :accessor dsp-group)
   (steth-ctl :initform (make-instance 'stethoscope-ctl) :accessor steth-ctl)
   (steth-view-pane :initform (make-instance 'stethoscope-view-pane)
                    :accessor steth-view-pane)
   (bufs-a :accessor bufs-a :type '(simple-array buffer (1000)))
   (bufs-b :accessor bufs-b :type '(simple-array buffer (1000)))
   (curr-bufs :accessor curr-bufs :type '(simple-array buffer (1000)))
   (redraw? :accessor redraw? :initform nil)
   (num-chans :initform 2 :initarg :num-chans :accessor num-chans)
   (bus-num :initform 0 :initarg :bus-num :accessor bus-num)
   (draw-mode :initform :tracks :accessor draw-mode)
   (io-mode :initform :audio-in :accessor io-mode)
   (zoom-x :initarg zoom-x :accessor zoom-x)
   (zoom-y :initarg zoom-y :accessor zoom-y)
   (chans-minval :initform 1 :accessor chans-minval)
   (chans-maxval :initform 64 :accessor chans-maxval)
   (bus-minval :initform 0 :accessor bus-minval)
   (bus-maxval :initform 24 :accessor bus-minval)
   (bufsize :initform 1024 :accessor bufsize)
   (bufmaxsize :initform 8192 :accessor bufmaxsize)
   (layout :accessor layout))
  (:qt-superclass "QWidget")
  (:metaclass qt-class)
  (:slots
   ("toggleDsp()" toggle-dsp)
   ("audioInAction()" do-audio-in-action)
   ("audioOutAction()" do-audio-out-action)
   ("busAction()" do-bus-action)
   ("tracksAction()" do-tracks-action)
   ("overlayAction()" do-overlay-action)
   ("xyAction()" do-xy-action)
   ("numTracksChanged(QString)" num-tracks-changed)
   ("busNumChanged(QString)" bus-num-changed)
   ("scrollXChanged(int)" scroll-x-changed)
   ("scrollYChanged(int)" scroll-y-changed)
   ("repaintView()" repaint-view))
  (:override
   ("closeEvent" close-event)
   ("keyPressEvent" key-press-event))
  (:signals
   ("toggleDspEvent()")
   ("repaintViewEvent()")))

(defun restart-stethoscope-dsp (stethoscope &key (group 400))
  (with-slots (num-chans bufmaxsize bufs-a bufs-b curr-bufs) stethoscope
    (mapc (lambda (slot) (and (slot-boundp stethoscope slot)
                         (incudine:free (slot-value stethoscope slot))))
          '(bufs-a bufs-b))
    (setf bufs-a (make-array num-chans)
          bufs-b (make-array num-chans)
          curr-bufs bufs-a)
    (incudine::dochannels (idx num-chans)
      (setf (svref bufs-a idx) (incudine:make-buffer (1+ bufmaxsize) :real-time-p t))
      (setf (svref bufs-b idx) (incudine:make-buffer (1+ bufmaxsize) :real-time-p t))))
  (let ((node (dsp-node-id stethoscope)))
    (and (find-node node) (incudine:free node))
    (sleep 0.2)
    (case (io-mode stethoscope)
      (:audio-out
       (setf (num-chans stethoscope) (min (num-chans stethoscope) incudine::*number-of-output-bus-channels*))
       (setf (maxval (num-chans-box (steth-ctl stethoscope))) incudine::*number-of-output-bus-channels*)
       (#_setText (num-chans-box (steth-ctl stethoscope)) (format nil "~d" (num-chans stethoscope)))
       (scratch::audio-out-scope-dsp (num-chans stethoscope) (bus-num stethoscope) stethoscope
                           (bufsize stethoscope)
                           (bufmaxsize stethoscope)
                           :action (lambda (n)
                                     (setf (dsp-node-id stethoscope) (incudine:node-id n)))
                           :tail group
                           :id (dsp-node-id stethoscope)))
      (:bus
       (setf (maxval (num-chans-box (steth-ctl stethoscope))) (chans-maxval stethoscope))
       (scratch::bus-scope-dsp (num-chans stethoscope) (bus-num stethoscope) stethoscope
                           (bufsize stethoscope)
                           (bufmaxsize stethoscope)
                           :action (lambda (n)
                                     (setf (dsp-node-id stethoscope) (incudine:node-id n)))
                           :tail group
                           :id (dsp-node-id stethoscope)))
      (t
       (setf (num-chans stethoscope) (min (num-chans stethoscope) incudine::*number-of-input-bus-channels*))
       (setf (maxval (num-chans-box (steth-ctl stethoscope))) incudine::*number-of-input-bus-channels*)
       (#_setText (num-chans-box (steth-ctl stethoscope)) (format nil "~d" (num-chans stethoscope)))
       (scratch::audio-in-scope-dsp (num-chans stethoscope) (bus-num stethoscope) stethoscope
                           (bufsize stethoscope)
                           (bufmaxsize stethoscope)
                           :action (lambda (n)
                                     (setf (dsp-node-id stethoscope) (incudine:node-id n)))
                           :tail group
                           :id (dsp-node-id stethoscope))))))

(defmethod initialize-instance :after ((instance stethoscope) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (layout num-chans-box bus-num-box chans-minval
                      chans-maxval bus-minval bus-maxval
                      steth-ctl steth-view-pane
                      num-chans bus-num
                      dsp-group
                      bufsize bufmaxsize)
      instance
    (setf layout (#_new QVBoxLayout instance))
    (#_setText (num-chans-box steth-ctl) (format nil "~a" num-chans))
    (#_setText (bus-num-box steth-ctl) (format nil "~a" bus-num))
    (setf (minval (num-chans-box steth-ctl)) chans-minval)
    (setf (maxval (num-chans-box steth-ctl)) chans-maxval)
    (setf (minval (bus-num-box steth-ctl)) bus-minval)
    (setf (maxval (bus-num-box steth-ctl)) bus-maxval)
    (#_setMargin layout 2)
    (#_setSpacing layout 2)
    (#_addWidget layout steth-ctl)
    (#_addWidget layout steth-view-pane)
    (let ((*background-color* "background-color: #dbdbdb;"))
      (cudagui-tl-initializer instance)
      (setf (main-widget (steth-view steth-view-pane)) instance)
      (restart-stethoscope-dsp instance :group dsp-group)
      (sleep 0.2)
      (let* ((scroll-x (scroll-x steth-view-pane)))
        (#_setValue
         scroll-x
         (round (map-value bufsize 128 bufmaxsize
                           (#_minimum scroll-x) (#_maximum scroll-x)))))
      (#_setGeometry instance 30 30 480 480)
      (setf (redraw? instance) t)
      (connect (audio-in-action steth-ctl) "triggered()" instance "audioInAction()")
      (connect (audio-out-action steth-ctl) "triggered()" instance "audioOutAction()")
      (connect (bus-action steth-ctl) "triggered()" instance "busAction()")
      (connect (tracks-action steth-ctl) "triggered()" instance "tracksAction()")
      (connect (overlay-action steth-ctl) "triggered()" instance "overlayAction()")
      (connect (xy-action steth-ctl) "triggered()" instance "xyAction()")
      (connect instance "toggleDspEvent()" instance "toggleDsp()")
      (connect (num-chans-box steth-ctl) "textChanged(QString)" instance "numTracksChanged(QString)")
      (connect (bus-num-box steth-ctl) "textChanged(QString)" instance "busNumChanged(QString)")
      (connect instance "repaintViewEvent()" instance "repaintView()")
      (connect (scroll-x steth-view-pane) "valueChanged(int)" instance "scrollXChanged(int)")
      (connect (scroll-y steth-view-pane) "valueChanged(int)" instance "scrollYChanged(int)")
      ;; (with-objects ((key (#_new QKeySequence (#_Key_Return "Qt"))))
      ;;   (#_new QShortcut key instance (QSLOT "toggleDsp()")))
      (#_setFocus instance)
      (#_setFocusPolicy instance (#_ClickFocus "Qt"))
      )))

(defmethod close-event ((instance stethoscope ) ev)
  (declare (ignore ev))
  (with-slots (dsp-node-id) instance
    (setf (redraw? instance) nil)
    (remove-gui (id instance))
    (and (find-node dsp-node-id) (incudine:free dsp-node-id))
    (call-next-qmethod)))

(defun do-audio-in-action (stethoscope)
  (setf (redraw? stethoscope) nil)
  (setf (io-mode stethoscope) :audio-in)
  (#_setText (io-button (steth-ctl stethoscope)) "Audio in")
;;;  (setf (num-chans stethoscope) (textedit-parse-integer text (chans-minval stethoscope)))
  (restart-stethoscope-dsp stethoscope)
  (setf (redraw? stethoscope) t)
  (#_repaint (steth-view-pane stethoscope)))

(defun do-audio-out-action (stethoscope)
  (setf (redraw? stethoscope) nil)
  (setf (io-mode stethoscope) :audio-out)
  (#_setText (io-button (steth-ctl stethoscope)) "Audio out")
  (restart-stethoscope-dsp stethoscope)
  (setf (redraw? stethoscope) t)
  (#_repaint (steth-view-pane stethoscope)))

(defun do-bus-action (stethoscope)
  (setf (redraw? stethoscope) nil)
  (setf (io-mode stethoscope) :bus)
  (#_setText (io-button (steth-ctl stethoscope)) "Bus")
  (restart-stethoscope-dsp stethoscope)
  (setf (redraw? stethoscope) t)
  (#_repaint (steth-view-pane stethoscope)))

(defun do-tracks-action (stethoscope)
;;  (format t "do-tracks-action~%")
  (setf (draw-mode stethoscope) :tracks)
  (#_setText (mode-button (steth-ctl stethoscope)) "Tracks")
  (#_repaint (steth-view-pane stethoscope)))

(defun do-overlay-action (stethoscope)
;;  (format t "do-overlay-action~%")
  (setf (draw-mode stethoscope) :overlay)
  (#_setText (mode-button (steth-ctl stethoscope)) "Overlay")
  (#_repaint (steth-view-pane stethoscope)))

(defun do-xy-action (stethoscope)
;;  (format t "do-xy-action~%")
  (unless (>= (num-chans stethoscope) 2)
      (warn "XY mode needs at least 2 chans to work properly"))
  (progn
    (setf (draw-mode stethoscope) :xy)
    (#_setText (mode-button (steth-ctl stethoscope)) "X/Y")
    (#_repaint (steth-view-pane stethoscope))))

(defmacro toggle (slot)
  `(setf ,slot (not ,slot)))

(defun toggle-dsp (stethoscope)
;;  (format t "toggle-dsp~%")
  (incudine:set-control (dsp-node-id stethoscope)
                        :process? (toggle (process? stethoscope))))

(defmethod key-press-event ((instance stethoscope) ev)
;;  (format t "~a~%" (#_key ev))
  (cond ;; Signal Ctl-Space pressed.
        ((= (#_key ev) 32)
         (call-next-qmethod)
         (toggle-dsp instance))
        ;; Delegate standard.
        ((= (#_key ev) 70)
         (call-next-qmethod)
         (let ((view (steth-view (steth-view-pane instance))))
           (toggle (fill? view))
           (unless (process? instance)
             (#_repaint view)
             (paint-event view nil))))
        (T
         (call-next-qmethod))))

(defun num-tracks-changed (stethoscope text)
;;  (format t "num-tracks-changed called~%")
  (setf (redraw? stethoscope) nil)
  (setf (num-chans stethoscope) (textedit-parse-integer text (chans-minval stethoscope)))
  (restart-stethoscope-dsp stethoscope)
  (setf (redraw? stethoscope) t)
  (#_repaint (steth-view-pane stethoscope)))

(defun bus-num-changed (stethoscope text)
  (with-slots (bus-num) stethoscope
    (setf bus-num
          (textedit-parse-integer text (chans-minval stethoscope)))
    (incudine:set-control (dsp-node-id stethoscope) :bus-num bus-num)))

(defun scroll-x-changed (stethoscope value)
  (let* ((scrollbar (scroll-x (steth-view-pane stethoscope)))
         (prop (normalize value
                (#_minimum scrollbar)
                (#_maximum scrollbar)))
         (new-size (round (+ 128 (* prop (- (bufmaxsize stethoscope) 128))))))
    (incudine:set-control (dsp-node-id stethoscope) :bufsize new-size)
    (setf (bufsize stethoscope) new-size)
    (#_repaint (steth-view-pane stethoscope))))

(defun scroll-y-changed (stethoscope value)
  (declare (ignore value))
;;  (format t "scroll-y-changed!~%")
  (#_repaint (steth-view-pane stethoscope)))

(defun repaint-view (stethoscope)
;;  (format t "repaint-event~%")
  (#_repaint (steth-view (steth-view-pane stethoscope))))

(defun set-scroll-x (stethoscope value)
  (#_setValue (scroll-x (steth-view-pane stethoscope)) value))

(defun scope (&key (id "Stethoscope") (group 400) (bus 0) (num-chans 2))
  (gui-funcall (create-tl-widget 'stethoscope id :group group :bus-num bus :num-chans num-chans)))

#|

;;;  (#_repaint (steth-view (steth-view-pane stethoscope)))

;;; (scope :id "Stethoscope" :num-chans 2)
;;; (setf (scratch::logger-level) :debug)
;;; (toggle-dsp (find-gui "Stethoscope"))
;;; (close-all-guis)

(toggle-dsp (find-gui "Stethoscope"))


(with-objects ((key (#_new QKeySequence (#_Key_Enter "Qt"))))
  (#_new QShortcut key instance (QSLOT "toggleDSP()")))
;;; (#_Qt::Key_Space)

(remove-gui "Stethoscope")


(signal! (find-gui :stethoscope01) (repaint-view))

(incudine:set-control (dsp-node-id (find-gui "Stethoscope")) :bufsize 400)

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

;;; (setf (num-chans (find-gui "Stethoscope")) 6)

;; (setf (redraw? (find-gui "Stethoscope")) nil)

;; (incudine:free (dsp-node-id (find-gui "Stethoscope")))


(restart-stethoscope-dsp (find-gui "Stethoscope"))

(incudine:set-control (dsp-node-id (find-gui "Stethoscope")) :process? t)



(let* ((gui (find-gui "Stethoscope")))
  (dotimes (f 2)
    (let ((curr-buf (svref (curr-bufs gui) f)))
      (dotimes (i 8192)
        (setf (incudine:buffer-value curr-buf i)  (- (random 1.0) 0.5))))))

(dotimes (i 100) (cuda-gui::emit-signal (find-gui :scope02) "repaintViewEvent()"))

(trace qt::%%call)
(trace )

(dotimes (i 1) (cuda-gui::emit-signal (find-gui :scope02) "repaintViewEvent()"))

(toggle-dsp (find-gui "Stethoscope"))

(progn
(let* ((gui (find-gui "Stethoscope")))
  (dotimes (f 2)
    (let ((curr-buf (svref (curr-bufs gui) f)))
      (dotimes (i 8192)
        (setf (incudine:buffer-value curr-buf i)  (- (random 1.0) 0.5))))))
  (sb-profile:reset)
  (repaint-view (find-gui "Stethoscope"))
  (format t "~%")
  (sb-profile:report :print-no-call-list nil))


(sb-profile:profile "INCUDINE" "QT")

(toggle-dsp (find-gui :scope01))

(progn
  (sb-profile:reset)
  (dotimes (i 100)
    (paint-event (steth-view (steth-view-pane (find-gui "Stethoscope"))) nil))
  (format t "~%")
  (sb-profile:report :print-no-call-list nil))

(time
 (dotimes (i 10)
   (paint-event (steth-view (steth-view-pane (find-gui "Stethoscope"))) nil)))


(trace draw-scope)

(/ 196592 459)
(draw-scope)
(untrace)

(#_lineTo paint-path x-eff y-eff)

(lambda () (optimized-call t paint-path "lineTo" x-eff y-eff))

(optimized-call t paint-path "lineTo" x-eff y-eff)

(list* 1 2 3 nil)

(multiple-value-bind
      (#:instance806 #:instance-qclass804 #:instance-extra-sig805)
    (qt::full-resolve-this paint-path)
  (declare (type (unsigned-byte 24) #:instance-qclass804))
  (let ((#:|arg-0| x-eff) (#:|arg-1| y-eff))
    (let* ((qt::types 'nil)
           (qt::args (list* #:|arg-0| #:|arg-1| 'nil))
           (qt::instance #:instance806)
           (#:|sig-0| (qt::signature-type #:|arg-0|))
           (#:|sig-1| (qt::signature-type #:|arg-1|)))
      (declare (dynamic-extent qt::args)
               (optimize (safety 0)))
      (multiple-value-bind (qt::fun)
          (let* ((#:places-811
                  (load-time-value
                   (make-array qt::+cache-pool-size+ :initial-element nil)))
                 (#:g807 #:instance-qclass804)
                 (#:g808 #:instance-extra-sig805)
                 (#:g809 #:|sig-0|)
                 (#:g810 #:|sig-1|)
                 (#:hash-812
                  (mod (logxor #:g807 (sxhash #:g809) (sxhash #:g810))
                       qt::+cache-pool-size+))
                 (#:previous-813 (svref #:places-811 #:hash-812))
                 (values
                  (if (if #:previous-813
                          (let ((qt::l (cdr #:previous-813)))
                            (if (equal (car qt::l) #:g807)
                                (let ((qt::l (cdr qt::l)))
                                  (if (equal (car qt::l) #:g808)
                                      (let ((qt::l (cdr qt::l)))
                                        (if (equal (car qt::l) #:g809)
                                            (let ((qt::l (cdr qt::l)))
                                              (the t
                                                   (equal (car qt::l)
                                                          #:g810)))
                                            nil))
                                      nil))
                                nil))
                          nil)
                      (car #:previous-813)
                      (the t
                           (let ((values
                                  (qt::resolve-call t qt::instance "lineTo"
                                                    qt::args qt::types)))
                             (sb-kernel:%svset #:places-811 #:hash-812
                                               (list values #:g807 #:g808
                                                     #:g809 #:g810))
                             values)))))
            values)
        (declare (type qt::cont-fun qt::fun))
        (funcall qt::fun #:instance806 qt::args)))))

(qt::resolve-call t qt::instance "lineTo" qt::args qt::types)

(qt::signature-type 320)

(setf *print-case* :downcase)

(untrace)

(incudine::buffer-value)



(progn
  (sb-profile:reset)
  (paint-event (steth-view (steth-view-pane (find-gui :scope01))) nil)
  (format t "~%")
  (sb-profile:report :print-no-call-list nil))

(progn
  (sb-profile:reset)
  (#_width (steth-view (steth-view-pane (find-gui "Stethoscope"))))
  (#_height (steth-view (steth-view-pane (find-gui "Stethoscope"))))
  (format t "~%")
  (sb-profile:report :print-no-call-list nil))

(incudine:buffer-value (svref (curr-bufs (find-gui :scope02))) 1)


(buffer-value (svref curr-bufs idx) sample-idx)

(#_moveTo paint-path width y-pos)


(#_setValue (scroll-x (steth-view-pane (find-gui "Stethoscope"))) 10000)


;; (assert-active (dsp-node-id (find-gui "Stethoscope")))

;;(incudine:dump (incudine:node 0))
(define-slot (stethoscope bus-num-changed) ((text string))
  (declare (connected
            (bus-num-box (steth-ctl stethoscope))
            (text-changed string)))
  (setf bus-num
        (textedit-parse-integer text (chans-minval stethoscope)))
  (incudine:set-control (dsp-node-id stethoscope) :bus-num bus-num))

(define-slot (stethoscope scroll-x-changed) ((value int))
  (declare (connected
            (scroll-x (steth-view-pane stethoscope))
            (value-changed int)))
  (let* ((scrollbar (scroll-x (steth-view-pane stethoscope)))
         (prop (normalize value
                (q+:minimum scrollbar)
                (q+:maximum scrollbar)))
         (new-size (round (+ 128 (* prop (- (bufmaxsize stethoscope) 128))))))
    (incudine:set-control (dsp-node-id stethoscope) :bufsize new-size)
    (setf bufsize new-size)
    (q+:repaint (steth-view-pane stethoscope))))

;;; connect(gameQuit, SIGNAL(triggered()), this, SLOT(close()));

(defun round-test-2 (buf scl x inc)
  (declare (incudine:buffer buf) (single-float scl inc) (fixnum x)
           (optimize speed (safety 0)))
  (incudine::sample->fixnum (* scl (incudine:buffer-value buf (round (* x inc)))) :roundp t))


(defun round-sample (x)
  (declare (type (sample
                  #.(coerce (ash most-negative-fixnum -1) 'sample)
                  #.(coerce (ash most-positive-fixnum -1) 'sample))
                 x))
  (multiple-value-bind (result rem) (round x)
    (declare (ignore rem))
    result))



(defparameter *gui* (find-gui :scope01))

(time (emit-signal *gui* "repaintViewEvent()"))

(time (#_repaint (steth-view (steth-view-pane *gui*))))

(time (gui-funcall (lambda () (#_set-value (scroll-x (steth-view-pane *gui*)) 10))))

(#_set-value (scroll-x (steth-view-pane *gui*)) 10)

;;(length (num-bufs (main-widget stethoscope-view)))

(make-button-menu button :actions ((audio-in-action "Audio In")
                                   (audio-out-action "Audio Out")
                                   (bus-action "Bus")))


(trace qt::resolve-cast)

(untrace)

(type-of (curr-bufs (find-gui "Stethoscope")))

|#
