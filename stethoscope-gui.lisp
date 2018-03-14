;;;; stethoscope-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defun find-node (node)
  (incudine:dograph (n)
    (when (equal node (incudine:node-id n))
      (return n))))

;; (assert-active (dsp-node-id (find-gui "Stethoscope")))

;;(incudine:dump (incudine:node 0))

(defclass stethoscope-ctl () ;;; top area of stethoscope
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

(defmethod initialize-instance :after ((instance stethoscope-ctl) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (layout
               audio-in-action
               audio-out-action
               bus-action
               tracks-action
               overlay-action
               xy-action
               io-button
               mode-button
               bus-num-box
               num-chans-box)
      instance
    (let ((io-menu (#_new QMenu)))
      (setf layout (#_new QHBoxLayout instance))
      (setf audio-in-action (#_new QAction "Audio in" io-menu))
      (setf audio-out-action (#_new QAction "Audio out" io-menu))
      (setf bus-action (#_new QAction "Bus" io-menu))
      (#_addAction io-menu audio-in-action)
      (#_addAction io-menu audio-out-action)
      (#_addAction io-menu bus-action)
      (#_setMenu io-button io-menu)
      (#_setText io-button "Audio in"))
    (let ((view-menu (#_new QMenu)))
      (setf tracks-action (#_new QAction "Tracks" view-menu))
      (setf overlay-action (#_new QAction "Overlay" view-menu))
      (setf xy-action (#_new QAction "X/Y" view-menu))
      (#_addAction view-menu tracks-action)
      (#_addAction view-menu overlay-action)
      (#_addAction view-menu xy-action)
      (#_setMenu mode-button view-menu)
      (#_setText mode-button "Tracks"))
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

(defclass stethoscope-view () ;;; the plot area
  ((style :initform "background-color: black;" :accessor style)
   (fill? :initform t :accessor fill?)
   (main-widget :accessor main-widget)
   (painter :accessor painter)
   (background-brush :accessor background-brush)
   (paint-path :accessor paint-path))
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
  (setf (background-brush instance) (#_new QBrush (#_new QColor 0 0 0 255) (#_Qt::SolidPattern)))
  (setf (paint-path instance) (#_new QPainter instance)))

(defmethod paint-event ((instance stethoscope-view) ev)
  (declare (ignore ev) ;; (optimize (speed 3))
           )
  (let ((stethoscope (main-widget instance)))
    (if (redraw? stethoscope)
        (if (slot-boundp stethoscope 'curr-bufs)
            (with-slots (painter background-brush main-widget fill?) instance
              (let* ((width (#_width instance))
                     (height (#_height instance))
                     (num-chans (num-chans (main-widget instance)))
                     (size (bufsize (main-widget instance)))
                     (y-scale (* height -0.5
                                 (- 1 (/ (#_value (scroll-y (steth-view-pane (main-widget instance)))) 10000)))))
                (declare (fixnum width height num-chans size))
                (#_begin painter instance)
                (#_setBackground painter background-brush)
            
                (#_eraseRect painter (#_rect instance))
                (#_setColor (#_pen painter) (#_new QColor 255 218 0 255))
                (#_setWidth (#_pen painter) 1)
                (case (draw-mode main-widget)
                  (:tracks
                   (let* ((num-points (min width size))
                          (x-inc (/ width num-points))
                          (idx-inc (/ size num-points)))
                     (if t
                         (dotimes (i (length (curr-bufs (main-widget instance))))
                           (let ((y-pos (round (* (+ 0.5 i) (/ height num-chans))))
                                 (buf (aref (curr-bufs (main-widget instance)) i)))
                             (let* ((paint-path (#_new QPainterPath)))
                               (#_moveTo paint-path width y-pos)
                               (#_lineTo paint-path 0 y-pos)
                               (dotimes (x num-points)
                                 (let ((amp (round (* y-scale (incudine:buffer-value buf (round (* x idx-inc)))))))
                                   (#_lineTo paint-path (* x x-inc)
                                             (+ y-pos amp))))
                               (#_closeSubpath paint-path)
                               (#_drawPath painter paint-path)
                               (if fill? (#_fillPath painter paint-path
                                                     (#_new QBrush (#_new QColor 165 141 0) (#_Qt::SolidPattern))))))))))
                  (:overlay
                   (let* ((num-points (min width size))
                          (x-inc (/ width num-points))
                          (idx-inc (/ size num-points)))
                     (dotimes (i (length (curr-bufs (main-widget instance))))
                       (let ((y-pos (round (* 0.5 height)))
                             (buf (aref (curr-bufs (main-widget instance)) i)))
                         (let* ((paint-path (#_new QPainterPath)))
                           (#_moveTo paint-path width y-pos)
                           (#_lineTo paint-path 0 y-pos)
                           (dotimes (x num-points)
                             (let ((amp (round (* y-scale (incudine:buffer-value buf (round (* x idx-inc)))))))
                               (#_lineTo paint-path (* x x-inc)
                                         (+ y-pos amp))))
                           (#_closeSubpath paint-path)
                           (#_drawPath painter paint-path)
                           (if fill? (#_fillPath painter paint-path (#_new QBrush
                                                                           (#_new QColor 165 141 0) (#_Qt::SolidPattern)))))))))
                  (:xy (let* ((num-points (min width size)))
                         (if (> (length (curr-bufs (main-widget instance))) 1)
                             (let ((x-buf (aref (curr-bufs (main-widget instance)) 0))
                                   (y-buf (aref (curr-bufs (main-widget instance)) 1)))
                               (let* ((paint-path (#_new QPainterPath))
                                      (x-offs (round (/ width 2)))
                                      (y-offs (round (/ height 2))))
                                 (#_moveTo paint-path
                                           (+ x-offs (* y-scale (incudine:buffer-value x-buf 0)))
                                           (+ y-offs (* y-scale (incudine:buffer-value y-buf 0))))
                                 (dotimes (idx num-points)
                                   (#_lineTo paint-path
                                             (+ x-offs (* y-scale (incudine:buffer-value x-buf idx)))
                                             (+ y-offs (* y-scale (incudine:buffer-value y-buf idx)))))
                                 (#_closeSubpath paint-path)
                                 (#_drawPath painter paint-path)
;;; (if fill? (#_fill-path painter paint-path (#_new QBrush (#_new QColor 165 141 0) (Qt::SolidPattern))))
                                 )))))))
              (#_end painter))
            (warn "bufs not bound!")))))
#|
(defparameter *gui* (find-gui :scope01))

(time (emit-signal *gui* "repaintViewEvent()"))

(time (#_repaint (steth-view (steth-view-pane *gui*))))

(time (gui-funcall (lambda () (#_set-value (scroll-x (steth-view-pane *gui*)) 10))))

(#_set-value (scroll-x (steth-view-pane *gui*)) 10)
|#

;;(length (num-bufs (main-widget stethoscope-view)))

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

(defclass stethoscope (cudagui-tl-mixin)
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
   ("keyPressEvent" key-press-event)
   )
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
      ;; (with-objects ((key (#_new QKeySequence (#_Key_Return "Qt"))))
      ;;   (#_new QShortcut key instance (QSLOT "toggleDsp()")))
      )))


#|
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
|#


(defmethod close-event ((instance stethoscope ) ev)
  (declare (ignore ev))
  (with-slots (dsp-node-id) instance
    (setf (redraw? instance) nil)
    (remove-gui (id instance))
    (and (find-node dsp-node-id) (incudine:free dsp-node-id))
    (call-next-qmethod)))


;;; 



;;; connect(gameQuit, SIGNAL(triggered()), this, SLOT(close()));

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
  (format t "toggle-dsp~%")
  (incudine:set-control (dsp-node-id (find-gui "Stethoscope"))
                        :process? (toggle (process? stethoscope))))

(defmethod key-press-event ((instance stethoscope) ev)
  (cond ;; Signal Ctl-Space pressed.
        ((equal (#_key ev) 32)
         (call-next-qmethod)
         (emit-signal instance "toggleDsp()"))
        ;; Delegate standard.
        (T
         (format t "~a~%" (#_key ev))
         (call-next-qmethod))))

#|
(with-objects ((key (#_new QKeySequence (#_Key_Enter "Qt"))))
  (#_new QShortcut key instance (QSLOT "toggleDSP()")))
|#

;;; (#_Qt::Key_Space)

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
  (#_repaint (steth-view-pane stethoscope)))


(defun repaint-view (stethoscope)
  (#_repaint (steth-view (steth-view-pane stethoscope)))
;;  (format t "repaint-event~%")
  )

;;;  (#_repaint (steth-view (steth-view-pane stethoscope)))

(defun scope (&key (id "Stethoscope") (group 400) (bus 0) (num-chans 2))
  (gui-funcall (create-tl-widget 'stethoscope id :group group :bus-num bus :num-chans num-chans)))

;;; (scope :id :steth02 :num-chans 2)

;;; (close-all-guis)

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
