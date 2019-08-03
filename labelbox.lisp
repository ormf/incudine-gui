;;;; labelbox.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defclass textbox ()
  ((label :accessor label :initarg :label)
   (style :initform "border: 1px solid #838383; 
background-color: #ffffff;
selection-color: black;
cursor-color: red;
border-radius: 2px;
selection-background-color: white" :accessor style))
  (:metaclass qt-class)
  (:qt-superclass "QLineEdit")
  (:override
   ;; ("paintEvent" paint-event)
))

(defmethod initialize-instance :after ((instance textbox) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (style label) instance
    (#_setStyleSheet instance style)
    (#_setFixedWidth instance 65)
    (#_setFixedHeight instance 25)
    (#_setAlignment instance (#_AlignLeft "Qt"))
    (#_setText instance (format nil "~4,2f" 32.18763))
    (#_setReadOnly instance t)
;;;    (connect instance "setText(QString)" instance "changeText(QString)")
    )
;;  (connect instance "processQueueSig()" instance "processQueue()")
  )

(defclass label-textbox ()
  ((label :initform "" :initarg :label :accessor label)
   (text :initform "" :initarg :text :accessor text)
   (label-box :initform (#_new QLabel) :accessor label-box)
   (text-box :initform (make-instance 'textbox) :accessor text-box))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
;;   (:slots
;;    ("changeText(QString)" (lambda (this newval)
;;                          (#_setText this newval)
;; ;;;                         (#_setUpdatesEnabled (parent this) nil)
;;                          )))
  (:signals
   ("setText(QString)")
   ("setLabel(QString)")))

(defmethod initialize-instance :after ((instance label-textbox) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (text-box label-box label text) instance
    (let ((layout (#_new QHBoxLayout)))
      (#_setStyleSheet text-box (style text-box))
      ;;    (#_setFixedWidth instance 45)
      (#_setFixedHeight text-box 25)
      (#_setAlignment text-box (#_AlignLeft "Qt"))
      (#_setText text-box text)
      (#_setReadOnly text-box t)
      (#_setText label-box label)
      (#_addWidget layout label-box)
      (#_addWidget layout text-box)
      (#_addStretch layout)
      (#_setLayout instance layout))
    (connect instance "setText(QString)" text-box "setText(QString)")
    (connect instance "setLabel(QString)" label-box "setText(QString)")
    )
;;,  (connect instance "processQueueSig()" instance "processQueue()")
  )

;;; (#_AlignCenter "Qt") <=> (#_Qt::AlignCenter)

;;;   

#|

Usage example:

(defclass labelbox-test (cudagui-tl-mixin)
  ((labelbox :accessor labelbox)
   (minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)
   (layout :initform nil :accessor layout))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override
   ("closeEvent" close-event)))

(defmethod initialize-instance :after ((instance labelbox-test) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer instance))
  (with-slots (layout labelbox minval maxval) instance
    (setf layout (#_new QHBoxLayout instance))
    (setf labelbox (make-instance 'labelbox :minval minval :maxval maxval))
    (#_addWidget layout labelbox)))

(defmethod close-event ((instance labelbox-test) ev)
  (declare (ignore ev))
  (remove-gui (id instance))
  (stop-overriding))


;;; (gui-funcall (create-tl-widget 'labelbox-test "num02" :minval 0 :maxval 12000))

(close-all-guis)

|#
