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
(named-readtables:in-readtable :qt)

(defclass cudagui-tl-mixin ()
  ((id :initform (error ":id required") :initarg :id :accessor id)
   (window-title)
   (gui-signal :initform nil :initarg :gui-signal :accessor gui-signal)))

(defun cudagui-tl-initializer (widget)
  (let ((id (id widget)))
    (#_setWindowTitle widget (format-title id))
    (#_setStyleSheet widget *background-color*)
    (#_setGeometry widget 50 50 100 100)
    (unwind-protect
         (add-gui id widget)
      (if (gui-signal widget)
          (incudine::sync-condition-signal *widget-finalized-sync*)))))

(defun create-tl-widget (widget-class id &rest args)
  "make, register and show instance of toplevel widget-class and
return it. id has to be unique among all toplevel widgets."
  (with-controller ()
    (let* ((win (apply #'make-instance widget-class :gui-signal t :id id args))
           (width (getf args :width (#_width win)))
           (height (getf args :height (#_height win)))
           (x (getf args :x-pos 0))
           (y (getf args :y-pos 0)))
      (#_setGeometry win x y width height)
      (#_show win)))
  (incudine::sync-condition-wait *widget-finalized-sync*)
  (find-gui id))

(defun cudagui-window-initializer (widget)
  (let ((id (id widget)))
    (#_setWindowTitle widget (format-title id))
    (#_setStyleSheet widget *background-color*)
    (#_setGeometry widget 50 50 100 100)
    (unwind-protect
         (add-gui id widget)
      (if (gui-signal widget)
          (incudine::sync-condition-signal *widget-finalized-sync*)))))
