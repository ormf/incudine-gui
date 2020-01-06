;;;; pushbutton.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defclass pushbutton ()
  ((style :initform "
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
   (height :initform 25 :initarg :height :accessor height))
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

(defclass toggle ()
  ((style :initform "
QPushButton {
         background-color: #dddddd;
         selection-color: #ff7777;
         min-width: 40px;
     }
" :initarg :style :accessor style)
   (id :initform nil :initarg :id :accessor id)
   (height :initform 25 :initarg :height :accessor height)
   (state :initform 0 :initarg :state :accessor state)
   (off-color :initform "#dddddd" :initarg :off-color :accessor off-color)
   (on-color :initform "#ff7777" :initarg :on-color :accessor on-color)
   (callback :initform #'identity :initarg :callback :accessor callback))
  (:metaclass qt-class)
  (:qt-superclass "QPushButton")
  (:slots
   ("clickAction()" toggle-state)
   ("stateAction(int)" set-state))
  (:signals
   ("setState(int)")))

(defgeneric toggle-state (obj))

(defmethod toggle-state ((instance toggle))
  (set-state instance (if (zerop (state instance)) 127 0)))

(defgeneric set-state (obj state))

(defmethod set-state ((instance toggle) state)
  (setf (state instance) state)
;;  (format t "~&set-state: ~a~%" state)
  (#_setStyleSheet instance
                   (format nil
                           "background-color: ~a;
border-radius: 4px; 
border-style: outset;
border-color: #777777;
border-width: 1px;
min-width: 45px;
"
                           (if (> state 0) (on-color instance) (off-color instance))))
  (funcall (callback instance) instance))

(format nil "background-color: ~a; width: 40px;"
                           )

(defmethod initialize-instance :after ((instance toggle) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (height style state) instance
    (#_setStyleSheet instance style)
    (#_setFocusPolicy instance (#_NoFocus "Qt"))
    (#_setFixedHeight instance height)
    (set-state instance state)
    (connect instance "released()" instance "clickAction()")
    (connect instance "setState(int)" instance "stateAction(int)")))
