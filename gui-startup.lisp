(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

;;; for file in standalone/*.so; do ln -s $file systems/`echo $file | sed s/standalone\\\\/qtlibs\!//g`; done
;;; for file in standalone/*.so; do ln -s $file -t systems ../standalone/`echo $file | sed s/standalone\\\\/qtlibs\!//g`; done
;;; for file in standalone/*.so; do ln -s $file -t systems ../`echo $file | sed s/qtlibs\!//g`; done

(defvar *controller* NIL)

(defclass controller ()
  ((queue :initform NIL :accessor queue))
  (:metaclass qt-class)
  (:qt-superclass "QObject")
  (:slots ("void processQueue()" process-queue))
  (:signals ("processQueueSig()")))

(defmethod initialize-instance :after ((instance controller) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (connect instance "processQueueSig()" instance "processQueue()"))

(defun process-queue (controller)
  (loop for function = (pop (queue controller))
     while function
     do (funcall function)))

(defun call-with-controller (function &optional (controller *controller*))
  (push function (queue controller))
  (emit-signal controller "processQueueSig()"))

(defmacro with-controller ((&optional (controller '*controller*)) &body
body)
  `(call-with-controller (lambda () ,@body) ,controller))


#|
(defmethod initialize-instance :after ((instance controller) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (setf (auto-shoot-timer instance) (#_new QTimer instance))
  (connect (auto-shoot-timer instance) "timeout()" instance "moveShot()")
  (with-objects ((col (#_new QColor 250 250 200))
                 (pal (#_new QPalette col)))
    (#_setPalette instance pal))
  (#_setAutoFillBackground instance t)
  (new-target instance))

"moveShot()" ist ein slot in cannon-field, gebunden an 'move-shot

;;;; Ein slot ist eine callback Funktion, ein Signal deren Aufruf. Das
;;;; Signal muss mit dem Slot verbunden worden sein, damit es
;;;; funktioniert. Zum Auslösen muss emit-signal aufgerufen werden,
;;;; was wiederum bei einer bestehenden Verbindung dann die mit dem
;;;; Slot verbundene Callback Funktion aufruft.

(angle (make-instance 'lcd-range :text "ANGLE"))

;;; "angleChanged(int)" ist ein signal in cannon-field

(connect cf "angleChanged(int)" angle "setValue(int)")

(emit-signal instance "angleChanged(int)" newval)


(connect angle "valueChanged(int)" cf "setAngle(int)")

;;; in :slots von cannon-field:

("setAngle(int)" (lambda (this newval)
                   (setf (current-angle this)
                         (min (max 5 newval) 70))))

;;;


(connect (auto-shoot-timer instance) "timeout()" instance "moveShot()")
|#


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

(defvar *gui-event* nil) ;;; event queue for commands to be executed in the gui thread

(defmacro with-traps-masked (&body body)
  #+sbcl `(sb-int:with-float-traps-masked (:underflow :overflow :invalid :inexact)
            ,@body)
  #-sbcl `(progn ,@body))

(defun init-controller ()
  (if *controller*
      (warn "Controller already initialized.")
  (with-traps-masked
    (make-qapplication)
    (with-objects ((controller (make-instance 'controller)))
      (#_QApplication::setQuitOnLastWindowClosed nil)
      (setf *controller* controller)      
      (#_exec *qapplication*)))))

#|
                               :show NIL
                               :main-thread NIL
                               :blocking NIL

#+darwin (controller 'controller
:show NIL
:main-thread T
:blocking NIL)
(start)
|#

(defun gui-funcall (function)
  (setf *gui-event* (list 'run function))
  (incudine::sync-condition-signal *to-gui-sync*)
  function)

(defun gui-start ()
  "initialize and start the gui's event loop"
  (if (and *gui-thread* (bt:thread-alive-p *gui-thread*))
      :started
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
            (sleep 2.5)
            (if (and *gui-thread* (bt:thread-alive-p *gui-thread*))
                (progn
                  (qt::init-qt-fast-calls)
                  :started)))
          (warn "Couldn't start Gui. Please evaluate (incudine:rt-start) first!"))))

(defun gui-stop ()
  (maphash (lambda (id gui) (declare (ignore id))
              (gui-funcall (#_close gui)))
           *guis*)
;;;  (gui-funcall (q+:quit *qapplication*))
;;;  (bt:destroy-thread *gui-thread*)
;;;  (setf *controller* nil)
;;;  (setf *gui-thread* nil)
  :stopped
  )

#|
(time (with-controller ()
        (#_show (#_new QWidget))))


;;; (gui-funcall (q+:quit *qapplication*))
;;; (q+:quit *qapplication*)
;;; (setf *qapplication* nil)
;;; (setf *controller* nil)
(gui-start)

(start)
(init-controller)
(gui-stop)

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
