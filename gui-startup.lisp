(in-package #:incudine-gui)
(in-readtable :qtools)

(defvar *controller* NIL)

(define-widget controller (QObject)
  ((queue :initform NIL :accessor queue)))

(define-signal (controller process-queue) ())

(define-slot (controller process-queue) ()
  (declare (connected controller (process-queue)))
  (loop for function = (pop (queue controller))
        while function
        do (funcall function)))

(defun call-with-controller (function &optional (controller *controller*))
  (push function (queue controller))
  (signal! controller (process-queue)))

(defmacro with-controller ((&optional (controller '*controller*)) &body
body)
  `(call-with-controller (lambda () ,@body) ,controller))

;;; controller startup

(defvar *gui-thread* nil)
(declaim (type (or bt:thread null) *gui-thread*))

(defvar *gui-priority* incudine.config:*nrt-priority*)
(declaim (type fixnum *gui-priority*))

(defvar *gui-sync* (incudine::make-sync-condition "gui"))
(declaim (type incudine::sync-condition *gui-sync*))

(defvar *gui-event* nil)

(defun init-controller ()
  (with-main-window
      (controller 'controller
                  :show NIL :main-thread T
                  :blocking NIL)
    (q+:qapplication-set-quit-on-last-window-closed NIL)
    (setf *controller* controller)))

(defun gui-funcall (function)
  (setf *gui-event* (list 'run function))
  (incudine::sync-condition-signal *gui-sync*)
  function)

(defun gui-start ()
  (incudine::with-new-thread (*gui-thread* "gui-thread" *gui-priority*
                              "GUI thread started")
    (setf *gui-event* nil)
    ;; (qt-init)
    (loop (incudine::sync-condition-wait *gui-sync*)
          (when (eq (first *gui-event*) 'run)
            (funcall (second *gui-event*))
            (setf *gui-event* nil))))
  (sleep .1)
  (gui-funcall #'init-controller)
  (and *gui-thread* (bt:thread-alive-p *gui-thread*) :started))