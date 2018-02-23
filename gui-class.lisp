;;;; gui-class.lisp
;;;;
;;;; cuda-tl-widget is the superclass of all incudine-gui toplevel
;;;; widgets. Using this will ensure
;;;; gui-registration. create-tl-widget makes sure, the widget is
;;;; existing and registered in the gui-thread before returning.
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)
 

(define-widget cuda-tl-widget (QWidget)
  ((id :initform (error ":id required") :initarg :id :accessor id)
   (window-title)
   (gui-signal :initform nil :initarg :gui-signal :accessor gui-signal)))

(define-initializer (cuda-tl-widget setup)
  (setf (q+:window-title cuda-tl-widget) (format-title id))
  (q+:set-style-sheet cuda-tl-widget *background-color*)
  (q+:set-geometry cuda-tl-widget 50 50 100 100)
  (unwind-protect
       (add-gui id cuda-tl-widget)
    (if gui-signal
        (incudine::sync-condition-signal *widget-finalized-sync*))))

(defun create-tl-widget (widget-class id &rest args)
  "make, register and show instance of toplevel widget-class and
return it. id has to be unique among all toplevel widgets."
  (with-controller ()
    (q+:show (apply #'make-instance widget-class :gui-signal t :id id args)))
  (incudine::sync-condition-wait *widget-finalized-sync*)
  (find-gui id))
