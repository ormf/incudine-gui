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

(defvar *to-gui-sync* (incudine::make-sync-condition "to-gui"))
(declaim (type incudine::sync-condition *to-gui-sync*))

(defvar *from-gui-sync* (incudine::make-sync-condition "from-gui"))
(declaim (type incudine::sync-condition *from-gui-sync*))

(defvar *widget-finalized-sync* (incudine::make-sync-condition "widget-finalized"))
(declaim (type incudine::sync-condition *widget-finalized-sync*))

(defvar *gui-event* nil)

(defun init-controller ()
  (with-main-window
      #+darwin (controller 'controller
                           :show NIL
                           :main-thread T
                           :blocking NIL)
      #-darwin (controller 'controller
                           :show NIL
                           :main-thread NIL
                           :blocking NIL)
    (q+:qapplication-set-quit-on-last-window-closed NIL)
    (setf *controller* controller)))

(defun gui-funcall (function)
  (setf *gui-event* (list 'run function))
  (incudine::sync-condition-signal *to-gui-sync*)
  function)

(defun gui-start ()
  (if incudine.util:*rt-thread*
      (progn
        (incudine::with-new-thread (*gui-thread* "gui-thread" *gui-priority*
                                                 "GUI thread started")
          (setf *gui-event* nil)
          ;; (qt-init)
          (init-controller)
          (loop (incudine::sync-condition-wait *to-gui-sync*)
             (when (eq (first *gui-event*) 'run)
               (funcall (second *gui-event*))
               (setf *gui-event* nil))))
        (sleep .1)
        (and *gui-thread* (bt:thread-alive-p *gui-thread*) :started))
      (warn "Couldn't start Gui. Please evaluate (incudine:rt-start) first!")))

(defun gui-stop ()
  (maphash (lambda (id gui) (declare (ignore id))
              (gui-funcall (q+:close gui)))
           *guis*)
  (bt:destroy-thread *gui-thread*)
  (setf *gui-thread* nil))

#|
(defun gui-start-thread-only ()
  (if incudine.util:*rt-thread*
      (progn
        (incudine::with-new-thread (*gui-thread* "gui-thread" *gui-priority*
                                                 "GUI thread started")
          (setf *gui-event* nil)
          ;; (qt-init)
          (loop (incudine::sync-condition-wait *gui-sync*)
             (when (eq (first *gui-event*) 'run)
               (funcall (second *gui-event*))
               (setf *gui-event* nil))))
        (sleep .1)
        (and *gui-thread* (bt:thread-alive-p *gui-thread*) :started))
      (warn "Couldn't start Gui. Please evaluate (incudine:rt-start) first!")))

  (incudine::sync-condition-signal *widget-finalized-sync*)

|#
