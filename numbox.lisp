;;;; numbox.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(define-widget numbox (QLineEdit)
  ((minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)
   (stepsize :initform 1 :initarg :stepsize :accessor stepsize)
   (decvalidator :accessor decvalidator)
   (mouse-pressed :initform nil :accessor mouse-pressed)
   (mouse-start-pos-y :initform 0 :accessor mouse-start-pos-y)
   (mouse-last-y :initform 0 :accessor mouse-last-y)
   (start-value :initform 0 :accessor mouse-start-pos-y)
   (style :initform "border: 1px solid #838383; 
background-color: #ffffff;
selection-color: red;
cursor-color: white;
border-radius: 2px;
selection-background-color: white" :accessor style)))

(defun textedit-parse-integer (str default)
  (if (string/= str "")
      (parse-integer str)
      default))

(define-initializer (numbox setup)
  (setf decvalidator (q+:make-qintvalidator minval maxval numbox))
  (q+:set-style-sheet numbox style)
;;  (q+:set-geometry numbox 50 50 60 30)
  (q+:set-fixed-width numbox 45)
  (q+:set-fixed-height numbox 25)
  (q+:set-alignment numbox (#_AlignCenter "Qt"))
  (q+:set-text numbox (format nil "~d" minval))
  (q+:set-read-only numbox t)

;;;  (setf hexvalidator (q+:make-qregexpvalidator (q+:make-qregexp "[0-9A-Fa-f]{1,2}") hexedit))
;;;  (setf binvalidator (q+:make-qregexpvalidator (q+:make-qregexp "[01]{1,8}") binedit))
  (q+:set-validator numbox decvalidator)
  )

(define-override (numbox mouse-press-event) (ev)
  (setf mouse-start-pos-y (q+:y ev))
;;  (format t "mouse-start: ~a" mouse-start-pos-y)
  (setf start-value (textedit-parse-integer (q+:text numbox) minval))
  (setf mouse-last-y (q+:y ev))
  (setf mouse-pressed t)
;;  (q+:set-cursor numbox (#_DragLinkCursor "Qt"))
  )


(define-override (numbox mouse-move-event) (ev)
  (if mouse-pressed
      (let ((multiplier (max 0.5 (/ (abs (- mouse-last-y (q+:y ev))) 4.0))))
;;        (format t "mouse-diff: ~a~%" mouse-diff)
        (q+:set-text numbox
                     (format nil "~d"
                             (setf start-value (min maxval
                                                    (max minval
                                                         (+ start-value
                                                            (round (* (- mouse-last-y (q+:y ev))
                                                                      multiplier))))))))
        (setf mouse-last-y (q+:y ev)))))

(define-override (numbox mouse-release-event) (ev)
  (setf mouse-pressed nil)
  ;;(q+:set-cursor numbox (#_DragLinkCursor "Qt"))
  (call-next-qmethod)
  )

(define-override (numbox key-press-event) (ev)
  (q+:set-read-only numbox nil)
  (call-next-qmethod)
  (q+:set-read-only numbox t)
  )
;;;   

#|

Usage example:

(define-widget numbox-test (QDialog cudagui-tl-mixin)
  ((numbox :accessor numbox)
   (minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)))

(define-subwidget (numbox-test layout) (q+:make-qhboxlayout numbox-test)
  (setf numbox (make-instance 'numbox :minval minval :maxval maxval))
  (q+:add-widget layout numbox)
  (q+:add-stretch layout)
  )

(define-initializer (numbox-test setup)
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer numbox-test)))
  (setf numbox (make-instance 'numbox :minval minval :maxval maxval))
  (q+:add-widget numbox-test numbox)

(define-override (numbox-test close-event) (ev)
  (declare (ignore ev))
  (remove-gui (id numbox-test))
  (call-next-qmethod))


;;; (gui-funcall (create-tl-widget 'numbox-test "num01" :minval 0 :maxval 12000))

|#

