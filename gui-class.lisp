;;;; gui-class.lisp
;;;;
;;;; cuda-tl-widget is the superclass of all incudine-gui toplevel
;;;; widgets. Using this will ensure proper handling of
;;;; gui-registration.
;;;;
;;;; create-tl-widget makes sure, the widget is existing and
;;;; registered by the gui-thread before returning.
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(defclass cudagui-tl-mixin ()
  ((id :initform (error ":id required") :initarg :id :accessor id)
   (window-title)
   (gui-signal :initform nil :initarg :gui-signal :accessor gui-signal)))

(defun cudagui-tl-initializer (widget)
  (let ((id (id widget)))
    (setf (q+:window-title widget) (format-title id ))
    (q+:set-style-sheet widget *background-color*)
    (q+:set-geometry widget 50 50 100 100)
    (unwind-protect
         (add-gui id widget)
      (if (gui-signal widget)
          (incudine::sync-condition-signal *widget-finalized-sync*)))))

(defun create-tl-widget (widget-class id &rest args)
  "make, register and show instance of toplevel widget-class and
return it. id has to be unique among all toplevel widgets."
  (with-controller ()
    (q+:show (apply #'make-instance widget-class :gui-signal t :id id args)))
  (incudine::sync-condition-wait *widget-finalized-sync*)
  (find-gui id))
