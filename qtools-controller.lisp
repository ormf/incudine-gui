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

(with-main-window (controller 'controller
                              :show NIL :main-thread T
                              :blocking NIL)
  (q+:qapplication-set-quit-on-last-window-closed NIL)
  (setf *controller* controller))

;;; controller startup

(defun init-controller ()
  (with-main-window
      (controller 'controller
                  :show NIL :main-thread T
                  :blocking NIL)
    (q+:qapplication-set-quit-on-last-window-closed NIL)
    (setf *controller* controller)))
