;;;; numbox.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(define-widget numbox (QDialog)
  ((minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)
   (stepsize :initform 1 :initarg :stepsize :accessor stepsize)
   (decvalidator :accessor decvalidator)
   (mouse-start-pos-y :initform 0 :accessor mouse-start-pos-y)
   (start-value :initform 0 :accessor mouse-start-pos-y)))

(define-subwidget (numbox decedit)
    (let ((*background-color* "background-color: #ffffff;"))
      (q+:make-qlineedit numbox))
  (q+:set-style-sheet decedit "background-color: #ffffff;")
  (q+:set-geometry decedit 50 50 60 30)
  (q+:set-text decedit (format nil "~d" minval))
;;  (q+:set-value decedit minval)
  )

(define-override (numbox mouse-press-event) (ev)
  (setf mouse-start-pos-y (q+:y ev))
  (format t "mouse-start: ~a" mouse-start-pos-y)
  (setf start-value (parse-integer (q+:text decedit)))) 

(define-override (numbox mouse-move-event) (ev)
  (let ((multiplier 0.5))
    (q+:set-text decedit
                 (format nil "~d"
                         (min maxval
                              (max minval
                                   (+ start-value
                                      (round (* (- mouse-start-pos-y (q+:y ev))
                                                multiplier)))))))))

;;;   


(define-initializer (numbox setup)
  (setf decvalidator (q+:make-qintvalidator minval maxval decedit))

;;;  (setf hexvalidator (q+:make-qregexpvalidator (q+:make-qregexp "[0-9A-Fa-f]{1,2}") hexedit))
;;;  (setf binvalidator (q+:make-qregexpvalidator (q+:make-qregexp "[01]{1,8}") binedit))
;;;  (q+:set-validator decedit decvalidator)
  )

(define-widget numbox-test (QDialog cudagui-tl-mixin)
  ((numbox :accessor numbox)
   (minval :initform 0  :initarg :minval :accessor minval)
   (maxval :initform 255 :initarg :maxval :accessor maxval)))

(define-subwidget (numbox-test layout) (q+:make-qhboxlayout numbox-test)
  (setf numbox (make-instance 'numbox :minval minval :maxval maxval))
  (q+:add-widget layout numbox)
;;  (q+:add-stretch layout)
  )

#|


(define-subwidget (numbox-test frame) (q+:make-qframe numbox-test)
  (setf numbox (make-instance 'numbox :minval minval :maxval maxval))
  (q+:set-geometry numbox 50 50 100 100)
  (q+:add frame numbox-test))
|#

(define-initializer (numbox-test setup)
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer numbox-test)))

#|
  (setf numbox (make-instance 'numbox :minval minval :maxval maxval))
  (q+:add-widget numbox-test numbox)

(make-instance 'numbox)
|#

(define-override (numbox-test close-event) (ev)
  (declare (ignore ev))
  (remove-gui (id numbox-test))
  (call-next-qmethod))


;;; (gui-funcall (create-tl-widget 'numbox-test "num01" :minval 10 :maxval 10000))

;;; (gui-funcall (create-tl-widget 'numbox "num01" :minval 10 :maxval 10000))

;;; (remove-gui "num01")
