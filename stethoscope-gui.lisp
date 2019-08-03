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
   (io-button :initform (make-instance 'pushbutton) :accessor io-button)
   (mode-button :initform (make-instance 'pushbutton) :accessor mode-button)
   (tracks-action :accessor tracks-action)
   (overlay-action :accessor overlay-action)
   (xy-action :accessor xy-action)
   (audio-in-action :accessor audio-in-action)
   (audio-out-action :accessor audio-out-action)
   (bus-action :accessor bus-action)
   (layout :accessor layout))
  (:metaclass qt-class)
  (:qt-superclass "QDialog"))

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
   (empty-path :accessor empty-path)
   (rect :accessor rect))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("paintEvent" paint-event)))

(defmethod initialize-instance :after ((instance stethoscope-view) &key parent)
  (if parent
      (new instance parent)
      (new instance))  
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

#|

(defmethod paint-event ((instance stethoscope-view) ev)
  (declare (ignore ev) (optimize (speed 3)))
  (let ((stethoscope (main-widget instance)))
    (if (redraw? stethoscope)
        (with-slots (curr-bufs) stethoscope
;;          (format t "repaint-event~%")
          (declare ((simple-array * *) curr-bufs))
          (if curr-bufs
              (with-slots (painter pen background-brush foreground-color
                                   fill-brush main-widget fill? rect)
                  instance
                (let* ((width (qt::fast-width instance))
                       (height (qt::fast-height instance))
                       (dwidth (float width 1.0d0))
                       (dheight (float height 1.0d0))
                       (num-chans (num-chans main-widget))
                       (size (bufsize (main-widget instance)))
                       (y-scale (* dheight -0.5d0
                                   (- 1.0d0 (* (the (float 0.0 10000.0)
                                                    (float (qt::fast-value
                                                            (scroll-y (steth-view-pane
                                                                       (main-widget instance))))))
                                               0.0001)))))
                  (declare (fixnum width height num-chans size)
                           (double-float y-scale))
                  (qt::fast-begin painter instance)
                  (qt::fast-eraseRect painter rect)
                  ;;                (qt::fast-eraseRect painter rect)
                  (with-pen (pen) painter
                    (qt::fast-setColor pen foreground-color)
                    (qt::fast-setWidth pen 1))
                  (case (draw-mode main-widget)
                    (:tracks
                     (incudine::reduce-warnings
                       (let* ((num-points (min width size))
                              (x-inc (/ dwidth num-points))
                              (idx-inc (/ (float size 1.0d0) num-points)))
                         (declare (integer num-points)
                                  (double-float idx-inc x-inc))
                         (dotimes (i (length curr-bufs))
                           (let ((y-pos (* (+ 0.5 i) (/ dheight num-chans)))
                                 (buf (incudine::svref curr-bufs i)))
                             (declare (double-float y-pos)
                                      (incudine::buffer buf))
                             (with-paint-path (paint-path)
                               (qt::fast-moveTo paint-path dwidth y-pos)
                               (qt::fast-lineto paint-path 0.0d0 y-pos)
                               (draw-scope num-points idx-inc x-inc
                                           y-pos y-scale buf paint-path)
                               (qt::fast-closeSubpath paint-path)
                               (qt::fast-drawPath painter paint-path)
                               (if fill? (qt::fast-fillPath painter paint-path fill-brush))))))))
                    (:overlay
                     (incudine::reduce-warnings
                       (let* ((num-points (min width size))
                              (x-inc (/ (float width 1.0d0) num-points))
                              (idx-inc (/ (float size 1.0d0) num-points)))
                         (declare (integer num-points)
                                  (double-float idx-inc x-inc))
                         (let ((y-pos (* 0.5 dheight)))
                           (declare (double-float y-pos))
                           (with-paint-path (paint-path)
                             (qt::fast-moveto paint-path dwidth y-pos)
                             (qt::fast-lineto paint-path 0.0d0 y-pos)
                             (dotimes (i (length curr-bufs))
                               (let ((buf (incudine::svref curr-bufs i)))
                                 (declare (incudine::buffer buf))
                                 (draw-scope num-points idx-inc x-inc
                                             y-pos y-scale buf paint-path)
;;; implicit?                     (qt::fast-closeSubpath paint-path)
                                 (qt::fast-drawPath painter paint-path)
                                 (if fill? (qt::fast-fillPath painter paint-path fill-brush))))
                             )))))
                    (:xy (incudine::reduce-warnings
                           (let* ((num-points (min width size)))
                             (declare ((integer 0 100000) num-points))
                             (if (> (length curr-bufs) 1)
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
                                       ;; implicit?                     (qt::fast-closeSubpath paint-path)
                                       (qt::fast-drawPath painter paint-path)
;;; (if fill? (#_fill-path painter paint-path fill-brush))
                                       ))))))))
                  (qt::fast-end painter))))))))
|#

(defmethod paint-event ((instance stethoscope-view) ev)
  (declare (ignore ev) (optimize (speed 3)))
  (let ((stethoscope (main-widget instance)))
    (if (redraw? stethoscope)
        (with-slots (curr-bufs) stethoscope
;;          (format t "repaint-event~%")
          (declare ((simple-array * *) curr-bufs))
          (if curr-bufs
              (with-slots (painter pen background-brush foreground-color
                                   fill-brush main-widget fill? rect)
                  instance
                (let* ((width (#_width instance))
                       (height (#_height instance))
                       (dwidth (float width 1.0d0))
                       (dheight (float height 1.0d0))
                       (num-chans (num-chans main-widget))
                       (size (bufsize (main-widget instance)))
                       (y-scale (* dheight -0.5d0
                                   (- 1.0d0 (* (the (float 0.0 10000.0)
                                                    (float (#_value
                                                            (scroll-y (steth-view-pane
                                                                       (main-widget instance))))))
                                               0.0001)))))
                  (declare (fixnum width height num-chans size)
                           (double-float y-scale))
                  (#_begin painter instance)
                  (#_eraseRect painter rect)
                  ;;                (#_eraseRect painter rect)
                  (with-pen (pen) painter
                    (#_setColor pen foreground-color)
                    (#_setWidth pen 1))
                  (case (draw-mode main-widget)
                    (:tracks
                     (incudine::reduce-warnings
                       (let* ((num-points (min width size))
                              (x-inc (/ dwidth num-points))
                              (idx-inc (/ (float size 1.0d0) num-points)))
                         (declare (integer num-points)
                                  (double-float idx-inc x-inc))
                         (dotimes (i (length curr-bufs))
                           (let ((y-pos (* (+ 0.5 i) (/ dheight num-chans)))
                                 (buf (incudine::svref curr-bufs i)))
                             (declare (double-float y-pos)
                                      (incudine::buffer buf))
                             (with-paint-path (paint-path)
                               (#_moveTo paint-path dwidth y-pos)
                               (#_lineTo paint-path 0.0d0 y-pos)
                               (draw-scope num-points idx-inc x-inc
                                           y-pos y-scale buf paint-path)
                               (#_closeSubpath paint-path)
                               (#_drawPath painter paint-path)
                               (if fill? (#_fillPath painter paint-path fill-brush))))))))
                    (:overlay
                     (incudine::reduce-warnings
                       (let* ((num-points (min width size))
                              (x-inc (/ (float width 1.0d0) num-points))
                              (idx-inc (/ (float size 1.0d0) num-points)))
                         (declare (integer num-points)
                                  (double-float idx-inc x-inc))
                         (let ((y-pos (* 0.5 dheight)))
                           (declare (double-float y-pos))
                           (with-paint-path (paint-path)
                             (#_moveTo paint-path dwidth y-pos)
                             (#_lineTo paint-path 0.0d0 y-pos)
                             (dotimes (i (length curr-bufs))
                               (let ((buf (incudine::svref curr-bufs i)))
                                 (declare (incudine::buffer buf))
                                 (draw-scope num-points idx-inc x-inc
                                             y-pos y-scale buf paint-path)
;;;                                 (#_closeSubpath paint-path)
                                 (#_drawPath painter paint-path)
                                 (if fill? (#_fillPath painter paint-path fill-brush))))
                             ;; implicit?                     (#_closeSubpath paint-path)
                             )))))
                    (:xy (incudine::reduce-warnings
                           (let* ((num-points (min width size)))
                             (declare ((integer 0 100000) num-points))
                             (if (> (length curr-bufs) 1)
                                 (let ((x-buf (incudine::svref (curr-bufs (main-widget instance)) 0))
                                       (y-buf (incudine::svref (curr-bufs (main-widget instance)) 1)))
                                   (with-paint-path (paint-path)
                                     (let* ((x-offs (/ dwidth 2))
                                            (y-offs (/ dheight 2)))
                                       (#_moveTo paint-path
                                                        (+ x-offs (* y-scale (incudine:buffer-value x-buf 0)))
                                                        (+ y-offs (* y-scale (incudine:buffer-value y-buf 0))))
                                       (dotimes (idx num-points)
                                         (#_lineTo paint-path
                                                          (+ x-offs (* y-scale (incudine:buffer-value x-buf idx)))
                                                          (+ y-offs (* y-scale (incudine:buffer-value y-buf idx)))))
                                       ;; implicit?                     (#_closeSubpath paint-path)
                                       (#_drawPath painter paint-path)
;;; (if fill? (#_fill-path painter paint-path fill-brush))
                                       ))))))))
                  (#_end painter)))
              (warn "bufs not bound!"))))))

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
   (curr-bufs :accessor curr-bufs :initform nil :type '(simple-array buffer (1000)))
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
   ("resizeEvent" resize-event)
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
      (let ((view (steth-view (steth-view-pane instance))))
        (setf (rect view) (qt::fast-rect view)))
      (#_setFocus instance)
      (#_setFocusPolicy instance (#_ClickFocus "Qt")))))

(defmethod resize-event ((instance stethoscope ) ev)
  (declare (ignore ev))
  (let ((view (steth-view (steth-view-pane instance))))
    (setf (rect view) (qt::fast-rect view)))
  (call-next-qmethod))

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

