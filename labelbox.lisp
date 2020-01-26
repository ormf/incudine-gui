;;;; labelbox.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

;;; Zusammenfassung von Parameter und seinem Label. Da Label und
;;; Parameter in einem globalen Grid angeordnet werden sollen, werden
;;; sie hier nur instantiiert, aber noch nicht einem Layout
;;; zugewiesen. Das passiert dann in den jeweils übergeordneten guis.

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defclass textbox ()
  ((label :accessor label :initarg :label)
   (style :initform "border: 1px solid #838383; 
background-color: #ffffff;
selection-color: black;
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
;;      (#_setFixedWidth instance 45)
;;      (#_setFixedWidth text-box 45)
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

(defclass label-spinbox ()
  ((label :initform "" :initarg :label :accessor label)
   (id :initform 0 :initarg :id :accessor id)
   (ref :initform nil :initarg :ref :accessor ref)
   (map-fn :initform #'identity :initarg :map-fn :accessor map-fn)
   (rmap-fn :initform #'identity :initarg :rmap-fn :accessor rmap-fn)
   (callback :initform #'identity :accessor callback)
   (label-box :initform (#_new QLabel) :accessor label-box)
   (text-box :initform (make-instance 'custom-spinbox) :accessor text-box))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots
   ("setValue(int)" set-pvb-value)
   ("incValue(int)" inc-pvb-value)
   ("recallValue(int)" recall-pvb-value))
  (:signals
   ("setValue(int)")
   ("incValue(int)")
   ("recallValue()")
   ("setLabel(QString)")))

(defun make-align (value)
  (let ((align  (#_AlignVCenter "Qt")))
    (setf (slot-value align 'qt::value) value)
    align))

(defmethod (setf val) (new-val (instance label-spinbox))
  (format t "directly setting value-cell~%")
  (emit-signal instance "setValue(int)" new-val)
  (when (ref instance)
    (set-cell (ref instance) (funcall (map-fn instance) new-val) :src instance))
  new-val)

(defmethod ref-set-cell ((instance label-spinbox) new-val)
  (with-slots (rmap-fn) instance
    (emit-signal instance "setValue(int)"
                 (funcall (rmap-fn instance) new-val))))

(defmethod set-ref ((instance label-spinbox) new-ref &key map-fn rmap-fn)
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

;;; (slot-value (#_AlignVCenter "Qt") 'qt::value)

;;;(funcall #_AlignVcenter "Qt")

(defmethod initialize-instance :after ((instance label-spinbox) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (text-box label-box label) instance
    (#_setStyleSheet text-box *beatstep-box-style*)
    (#_setFixedWidth text-box 45)
    (#_setFixedWidth instance 45)
    ;;    (#_setFixedWidth instance 45)
    (#_setFixedHeight text-box 25)
    (#_setAlignment text-box (#_AlignRight "Qt"))
;;    (#_setText text-box text)
;;    (#_setReadOnly text-box t)
    (#_setText label-box label)
    (#_setAlignment label-box (make-align 130))
    (connect instance "setLabel(QString)" label-box "setText(QString)")
    (connect text-box "valueChanged(int)" instance "setValue(int)")
    (connect instance "setValue(int)" instance "setValue(int)")
    (connect instance "incValue(int)" instance "incValue(int)")
    (connect text-box "returnPressed(int)" instance "recallValue(int)")))

(defgeneric set-pvb-value (instance value))

(defmethod set-pvb-value ((instance label-spinbox) value)
  (#_setValue (text-box instance) value)
  (funcall (callback instance) (#_value (text-box instance)))
  (if (ref instance)
      (set-cell (ref instance) (funcall (map-fn instance) value) :src instance)))

(defgeneric inc-pvb-value (instance value)
  (:method ((instance label-spinbox) inc)
    (#_setValue (text-box instance)
                (+ (#_value (text-box instance)) inc))
    (funcall (callback instance) (#_value (text-box instance)))))

(defgeneric recall-pvb-value (instance val)
  (:method ((instance label-spinbox) val)
    (declare (ignore val))
    (funcall (callback instance) (#_value (text-box instance)))))

(defmethod val ((instance label-spinbox))
  (#_value (text-box instance)))


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
