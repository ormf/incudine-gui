;;;; pushbutton.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

#|
"background-color: ~a;
    border-radius: 4px; 
    border-style: outset;
    border-color: #777777;
    border-width: 1px;
    min-width: 45px;
"

(style :initform "
QPushButton {
         background-color: #dddddd;
         selection-color: #ff7777;
         min-width: 40px;
     }
" :initarg :style :accessor style)
|#

(defclass pushbutton ()
  ((callback :initform #'identity :initarg :callback :accessor callback)
   (id :initform nil :initarg :id :accessor id)
   (height :initform 25 :initarg :height :accessor height)
   (style :initform "
QPushButton {
         border: 1px solid #838383;
         background-color: #ffffff;
         selection-color: red;
         border-radius: 0px;
         selection-background-color: white;
         min-width: 80px;
     }

QPushButton::menu-indicator {
         color: #dddddd;
         subcontrol-origin: padding;
         subcontrol-position: center right;
         left: -5px;
         top: 1px;
     }
" :accessor style)
)
  (:metaclass qt-class)
  (:qt-superclass "QPushButton"))

(defmethod initialize-instance :after ((instance pushbutton) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (style height) instance
    (#_setStyleSheet instance style)
    (#_setFocusPolicy instance (#_NoFocus "Qt"))
    (#_setFixedHeight instance height)))

(defclass toggle (pushbutton)
  ((state :initform 0 :initarg :state :accessor state)
   (ref :initform nil :initarg :ref :accessor ref)
   (map-fn :initform #'identity :initarg :map-fn :accessor map-fn)
   (rmap-fn :initform #'identity :initarg :rmap-fn :accessor rmap-fn)
   (off-color :initform "#dddddd" :initarg :off-color :accessor off-color)
   (on-color :initform "#ff7777" :initarg :on-color :accessor on-color))
  (:metaclass qt-class)
  (:qt-superclass "QPushButton")
  (:slots
   ("clickAction()" toggle-state)
   ("setValue(int)" set-state)
   ("changeValue(int)" change-state))
   (:signals
    ("setValue(int)")
    ("changeValue(int)")))

(defgeneric toggle-state (obj))

(defmethod toggle-state ((instance toggle))
  (change-state instance (if (zerop (state instance)) 127 0)))

(defgeneric set-state (obj state)
  (:method ((instance toggle) state)
    (setf (state instance) state)
    (update-view instance)
    (funcall (callback instance) instance)))

(defgeneric change-state (obj state)
  (:method ((instance toggle) state)
    (setf (state instance) state)
    (update-view instance)
    (if (ref instance) (set-cell (ref instance) (funcall (map-fn instance) state) :src instance))
    (funcall (callback instance) instance)))

(defgeneric update-view (toggle)
  (:method ((instance toggle))
    (with-slots (state on-color off-color) instance
      (#_setStyleSheet
       instance
       (format nil
               "background-color: ~a;
    border-radius: 4px; 
    border-style: outset;
    border-color: #777777;
    border-width: 1px;
    min-width: 45px;"
               (if (> state 0) on-color off-color))))))

(defmethod (setf val) (new-val (instance toggle))
  (format t "directly setting value-cell~%")
  (emit-signal instance "changeValue(int)" new-val)
  (when (ref instance)
    (set-cell (ref instance) (funcall (map-fn instance) new-val) :src instance))
  new-val)

(defmethod ref-set-cell ((instance toggle) new-val)
  (with-slots (rmap-fn) instance
    (emit-signal instance "setValue(int)"
                 (funcall (rmap-fn instance) new-val))))

(defmethod set-ref ((instance toggle) new-ref &key map-fn rmap-fn)
  (with-slots (ref) instance
    (when ref (setf (dependents ref) (delete instance (dependents ref))))
    (setf ref new-ref)
    (if new-ref
        (progn
          (pushnew instance (dependents new-ref))
          (if map-fn (setf (map-fn instance) map-fn))
          (if rmap-fn (setf (rmap-fn instance) rmap-fn))
          (ref-set-cell instance (slot-value new-ref 'val)))))
  new-ref)




#|
(let ((tg (make-instance 'toggle)))
  (change-state tg 12)
  )
(toggle-state (make-instance 'toggle))

(untrace)
 
|#
                           
(defmethod initialize-instance :after ((instance toggle) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (height style) instance
    (#_setStyleSheet instance style)
    (#_setFocusPolicy instance (#_NoFocus "Qt"))
    (#_setFixedHeight instance height)
    (connect instance "released()" instance "clickAction()")
    (connect instance "setValue(int)" instance "setValue(int)")
    (connect instance "changeValue(int)" instance "changeValue(int)")))
  
(defclass label-pushbutton (pushbutton)
  ((label :initform "" :initarg :label :accessor label)
   (label-box :initform (#_new QLabel) :accessor label-box))
  (:metaclass qt-class)
  (:qt-superclass "QPushButton"))

(defmethod initialize-instance :after ((instance label-pushbutton) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (label-box label) instance
    (#_setFixedWidth instance 45)
    (#_setFixedHeight instance 25)
    (#_setText label-box label)
    (#_setAlignment label-box (make-align 130))
    (connect instance "setLabel(QString)" label-box "setText(QString)")))

(defclass label-toggle (toggle)
  ((label :initform "" :initarg :label :accessor label)
   (label-box :initform (#_new QLabel) :accessor label-box))
  (:metaclass qt-class)
  (:qt-superclass "QPushButton"))

(defmethod initialize-instance :after ((instance label-toggle) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (height style) instance
    (#_setStyleSheet instance style)
    (#_setFocusPolicy instance (#_NoFocus "Qt"))
    (#_setFixedHeight instance height)
    (connect instance "released()" instance "clickAction()")
    (connect instance "setValue(int)" instance "setValue(int)")
    (connect instance "changeValue(int)" instance "changeValue(int)"))
  (with-slots (label-box label) instance
    (#_setFixedWidth instance 45)
    (#_setText label-box label)
    (#_setAlignment label-box (make-align 130))
    (connect instance "setLabel(QString)" label-box "setText(QString)")))
