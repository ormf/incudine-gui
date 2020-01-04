;;;; labelbox.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

(defclass labelbox ()
  ((minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)
   (cursor :initform nil :accessor cursor)
   (stepsize :initform 1 :initarg :stepsize :accessor stepsize)
   (decvalidator :accessor decvalidator)
   (mouse-pressed :initform nil :accessor mouse-pressed)
   (mouse-start-pos-y :initform 0 :accessor mouse-start-pos-y)
   (mouse-last-y :initform 0 :accessor mouse-last-y)
   (start-value :initform 0 :accessor mouse-start-pos-y)
   (style :initform "border: 1px solid #838383; 
background-color: #ffffff;
selection-color: red;
border-radius: 2px;
selection-background-color: white" :accessor style))
  (:metaclass qt-class)
  (:qt-superclass "QLineEdit")
  (:override
   ;; ("paintEvent" paint-event)
   ("keyPressEvent" key-press-event)
   ("mousePressEvent" mouse-press-event)
   ("mouseMoveEvent" mouse-move-event)
   ("mouseReleaseEvent" mouse-release-event)))

(defmethod initialize-instance :after ((instance labelbox) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (minval maxval cursor style decvalidator) instance
    (setf decvalidator (#_new QIntValidator minval maxval instance))
;;    (setf cursor (#_new QCursor))
;;    (#_setShape cursor (#_SizeVerCursor "Qt"))
;;    (#_setCursor instance cursor)
    (#_setValidator instance decvalidator)
    (#_setStyleSheet instance style)
    (#_setFixedWidth instance 45)
    (#_setFixedHeight instance 25)
    (#_setAlignment instance (#_AlignCenter "Qt"))
    (#_setText instance (format nil "~d" minval))
    (#_setReadOnly instance t)
    )
;;,  (connect instance "processQueueSig()" instance "processQueue()")
  )

;;; (#_AlignCenter "Qt") <=> (#_Qt::AlignCenter)

(defmethod mouse-press-event ((instance labelbox) ev)
  (with-slots (mouse-start-pos-y cursor minval start-value mouse-last-y mouse-pressed)
      instance           
    (setf mouse-start-pos-y (#_y ev))
;;    (format t "mouse-start: ~a" mouse-start-pos-y)
    (setf start-value (textedit-parse-integer (#_text instance) minval))
    (setf mouse-last-y (#_y ev))
    (setf mouse-pressed t)
;;    (#_setReadOnly instance nil)
;;    (#_setShape cursor (#_WaitCursor "Qt"))
  ))

(defmacro clip (value minval maxval)
  `(min ,maxval (max ,minval ,value)))

(defmethod mouse-move-event ((instance labelbox) ev)
  (with-slots (mouse-pressed
               start-value
               mouse-last-y
               minval maxval)
      instance
    (if mouse-pressed
        (let* ((mouse-diff (- mouse-last-y (#_y ev)))
               (multiplier (max 0.5 (/ (abs mouse-diff) 4.0))))
;;          (format t "mouse-diff: ~a~%" mouse-diff)
          (#_setText
           instance
           (format nil "~d"
                   (setf start-value (clip
                                      (round 
                                       (+ start-value
                                          (* multiplier
                                             (- mouse-last-y
                                                (#_y ev)))))
                                      minval maxval))))
          (setf mouse-last-y (#_y ev))))))

(defmethod mouse-release-event ((instance labelbox) ev)
  (setf (mouse-pressed instance) nil)
  ;;(#_setCursor instance (#_DragLinkCursor "Qt"))
  (stop-overriding))

(defmethod key-press-event ((instance labelbox) ev)
  (#_setReadOnly instance nil)
  (call-next-qmethod)
  (#_setReadOnly instance t))

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
