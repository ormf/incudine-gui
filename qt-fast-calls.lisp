;;;; qt-fast-calls.lisp
;;;;
;;;; for painter overrides we need to avoid all consing. Therefore
;;;; commonqt's calling mechanism of selected qt painting functions
;;;; needed by the overrides is stripped down here to the direct
;;;; smoke calls. After loading the qt libs, #'init-qt-fast-calls
;;;; collects the method indexes, function pointers etc. in a hash
;;;; table to be accessible by the calling functions. The functions
;;;; have the same call semantics as the original #_ functions of
;;;; commonqt and are prefixed fast-<qt-name>.
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl
;;;; <orm.finnendahl@selma.hfmdk-frankfurt.de>


(in-package :qt)
(named-readtables:in-readtable :qt)

(defvar *qt-fast-call-hash* (make-hash-table))

(declaim (inline fast-call))
(defun fast-call (name)
     (gethash name *qt-fast-call-hash*))

(defun set-fast-call (name value)
  (setf (gethash name *qt-fast-call-hash*) value))

(defsetf fast-call set-fast-call)

(defun init-qt-fast-calls ()
  (setf (fast-call 'qwidget) (#_new QWidget)) ;;; class-instances
  (setf (fast-call 'qpainter) (#_new QPainter))
  (setf (fast-call 'qpainter-path) (#_new QPainterPath))
  (let ((lineto-method (find-applicable-method
                        (fast-call 'qpainter-path) "lineTo" '(0 0) nil)))
    (setf (fast-call 'lineto-method-idx)
          (qmethod-classfn-index
           lineto-method))
    (setf (fast-call 'lineto-fn)
          (qclass-trampoline-fun (qmethod-class lineto-method))))
  (let ((moveto-method (find-applicable-method
                        (fast-call 'qpainter-path) "moveTo" '(0 0) nil)))
    (setf (fast-call 'moveto-method-idx)
          (qmethod-classfn-index moveto-method))
    (setf (fast-call 'moveto-fn)
          (qclass-trampoline-fun (qmethod-class moveto-method))))
  (let ((begin-method (find-applicable-method
                       (fast-call 'QPainter) "begin" (list (fast-call 'QWidget)) nil)))
    (setf (fast-call 'begin-method-idx) (qmethod-classfn-index begin-method))
    (setf (fast-call 'begin-method-argtype) (car (list-qmethod-argument-types begin-method)))
    (setf (fast-call 'begin-fn) (qclass-trampoline-fun (qmethod-class begin-method)))
    (setf (fast-call 'begin-cast)
          (multiple-value-list (resolve-cast (qobject-class (fast-call 'qwidget))
                                             (qtype-class (fast-call 'begin-method-argtype))))))
  (let ((drawpath-method (find-applicable-method
                          (fast-call 'QPainter) "drawPath" (list (fast-call 'qpainter-path)) nil)))
    (setf (fast-call 'drawpath-method-idx) (qmethod-classfn-index drawpath-method))
    (setf (fast-call 'drawpath-method-argtype) (car (list-qmethod-argument-types drawpath-method)))
    (setf (fast-call 'begin-fn) (qclass-trampoline-fun (qmethod-class drawpath-method)))))

#|
(let ((method (find-applicable-method (fast-call 'qpainter-path) "closeSubpath" '() nil)))
  (list-qmethod-argument-types method))

(let ((method (find-applicable-method (fast-call 'qpainter-path) "drawPath" (list (fast-call 'QPainter)) nil)))
  (list-qmethod-argument-types method))

(let ((method (find-applicable-method (fast-call 'QPainter) "drawPath" (list (fast-call 'qpainter-path)) nil)))
  (list-qmethod-argument-types method))
|#
;; (init-qt-fast-calls)

(declaim (inline fast-lineto))
(defun fast-lineto (paint-path x y)
    (declare (optimize speed (safety 0))
             (double-float x y)
             (qobject paint-path))
    (let* ((object (slot-value paint-path 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 3)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'double)
            x)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 2))
             '(:union StackItem)
             'double)
            y)
      (call-class-fun (the sb-sys:system-area-pointer (fast-call 'lineto-fn))
                      (the integer (fast-call 'lineto-method-idx)) object stack))))

(declaim (inline fast-moveto))
(defun fast-moveto (paint-path x y)
    (declare (optimize speed (safety 0))
             (double-float x y)
             (qobject paint-path))
    (let* ((object (slot-value paint-path 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 3)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'double)
            x)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 2))
             '(:union StackItem)
             'double)
            y)
      (call-class-fun (the sb-sys:system-area-pointer (fast-call 'moveto-fn))
                      (the integer (fast-call 'moveto-method-idx)) object stack))))

(declaim (inline fast-begin))
(defun fast-begin (painter instance)
    (declare (optimize speed (safety 0))
             (qobject painter instance))
    (let* ((object (slot-value painter 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (let ((begin-cast (fast-call 'begin-cast)))
              (%perform-cast (qobject-pointer instance)
                             (first begin-cast)
                             (second begin-cast)
                             (third begin-cast))))
      (call-class-fun (fast-call 'begin-fn) (fast-call 'begin-method-idx) object stack))))

(declaim (inline fast-drawpath))
(defun fast-drawpath (painter instance)
    (declare (optimize speed (safety 0))
             (qobject painter instance))
    (let* ((object (slot-value painter 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (multiple-value-bind (castfn <from> <to>)
                (resolve-cast (qobject-class instance)
                              (qtype-class (fast-call 'drawpath-method-argtype)))
              (%perform-cast (qobject-pointer instance)
                             castfn <from> <to>)))
      (call-class-fun (fast-call 'drawpath-fn) (fast-call 'drawpath-method-idx) object stack))))


#|
(declaim (inline opt-end-painter))
(defun opt-end-painter (fn method-idx object)
    (declare (optimize speed (safety 0))
             (integer method-idx)
             (sb-sys:system-area-pointer fn object))
    (cffi:with-foreign-object (stack '(:union StackItem) 1)
      (call-class-fun fn method-idx object stack)))


(defparameter *widget* (#_new QWidget))
(defparameter *steth-view* (make-instance 'cuda-gui::stethoscope-view))

(let ((paint-path (fast-call 'qpainter-path))
      (painter (fast-call 'QPainter))
      (instance *widget*))
  (sb-profile:reset)
  (time (dotimes (i 1000)
          (fast-begin painter instance)))
  (sb-profile:report :print-no-call-list nil)
)

(trace resolve-cast)

(let ((paint-path (fast-call 'qpainter-path))
      (painter (fast-call 'QPainter))
      (instance (make-instance 'cuda-gui::stethoscope-view)))
  (sb-profile:reset)
  (time
   (dotimes (i 1000)
           (fast-begin painter instance)))
  (sb-profile:report :print-no-call-list nil))

(resolve-cast 28868 15620)

(let ((paint-path (fast-call 'qpainter-path))
      (painter (fast-call 'QPainter))
      (instance (make-instance 'cuda-gui::stethoscope-view)))
  (fast-drawpath painter paint-path))

(defparameter *instance* (make-instance 'cuda-gui::stethoscope-view))
(defparameter *painter* (fast-call 'QPainter))

(let* ((instance (make-instance 'cuda-gui::stethoscope-view))
       (painter (#_new QPainter instance)))

  (#_begin painter instance)
)

(#_begin *painter* *instance*)

(qtype-stack-item-slot 35655)

(typep *instance* 'qobject)

(SET-THUNK #<INCUDINE-GUI::STETHOSCOPE-VIEW QWidget 0x7FFFBC000E30> 35655)


(qtype-stack-item-slot <type>)

(find-applicable-method
 (#_new QPainter) "begin" (list *instance*) nil)


(#_begin *painter* *instance*)

(arglist-marshaller)

(list-qmethod-argument-types (find-applicable-method
 (#_new QPainter) "begin" (list *instance*) nil))

(find-applicable-method
 (#_new QPainter) "begin" (list (#_new QWidget)) nil)549509

(type-of )


(declaim (inline %%rcall))
(defun %%rcall (casted-instance-pointer
                rarglist-marshaller
                classfn
                method-index
                return-value-function
                &rest args)
  (apply rarglist-marshaller
           (lambda (stack)
             (call-class-fun classfn method-index casted-instance-pointer
                             stack)
            (funcall return-value-function stack)
             )
           args))

;;; (arglist-marshaller)

(defun set-object-binding (classfn object binding)
  (cffi:with-foreign-object (stack '(:union StackItem) 2)
    (setf (cffi:foreign-slot-value
           (cffi:mem-aptr stack '(:union StackItem) 1)
           '(:union StackItem)
           'ptr)
          binding)
    ;; Method index 0 sets the binding
    (call-class-fun classfn 0 object stack)))



(defun rargstep-marshaller (argtypes i &rest for-values)
  (if argtypes
      (let ((marshal-thunk (marshaller (car for-values)
                                       (car argtypes)))
            (next-thunk (apply #'rargstep-marshaller
                               (cdr argtypes)
                               (1+ i)
                               (cdr for-values))))
        (lambda (stack final-cont &rest args)
          (funcall marshal-thunk
                   (car args)
                   stack
                   i
                   (lambda ()
                     (apply next-thunk
                            stack
                            final-cont
                            (cdr args))))))
      (lambda (stack final-cont &rest args)
        (declare (ignore args))
        (funcall final-cont stack))))

(defun rarglist-marshaller (argtypes &rest for-values)
  (let ((thunk (apply #'rargstep-marshaller argtypes 1 for-values))
        (n (1+ (length argtypes))))
    (declare (type (unsigned-byte 16) n))
    (named-lambda arglist-marshaller (final-cont &rest args)
      (cffi:with-foreign-object (stack '(:union StackItem) n)
        (apply thunk stack final-cont args)))))

(defun rresolve-call (allow-override-p instance method fix-types &rest args)
;;  (format *trace-output* "cache miss for ~A::~A~%" instance method)
  (let ((name method)
        (method (etypecase method
                  (integer method)
                  (string (find-applicable-method
                           instance method args fix-types)))))
    (unless method
      (error "No applicable method ~A found on ~A with arguments ~S"
             name instance args))
    (let* ((precompiled-override
             (when allow-override-p
               (find-method-override instance method)))
           (arglist-marshaller
             (apply #'rarglist-marshaller (list-qmethod-argument-types method) args))
           (classfn
             (qclass-trampoline-fun (qmethod-class method)))
           (method-index
             (qmethod-classfn-index method))
           (rtype
             (qmethod-return-type method))
           (return-value-function
             (unmarshaller rtype)))
      (cond
        ((integerp instance)
         (unless (qmethod-static-p method)
           (error "~a::~a is not a static method"
                  (qclass-name instance) name))
         (assert (not precompiled-override))
         (lambda (<class> args)
           (declare (ignore <class>))
           (apply #'%%rcall (cffi:null-pointer)
                   arglist-marshaller
                   classfn
                   method-index
                   return-value-function
                   args)))
        (t
         (let ((<from> (qobject-class instance)))
           (multiple-value-bind (castfn <from> <to>)
               (resolve-cast <from> (qmethod-class method))
             (let ((cont
                     (if precompiled-override
                         (lambda (actual-instance &rest args)
                           (block nil
                             (catch 'stop-overriding-tag
                               (return (override precompiled-override
                                                 actual-instance method args)))
                             (apply #'%%rcall (perform-cast actual-instance castfn <from> <to>)
                                     arglist-marshaller
                                     classfn
                                     method-index
                                     return-value-function
                                     args)))
                         (progn
                           (format t "~&here!~%")
                           (lambda (actual-instance &rest args)
                             (apply #'%%rcall (perform-cast actual-instance castfn <from> <to>)
                                    arglist-marshaller
                                    classfn
                                    method-index
                                    return-value-function
                                    args))))))
               (if (alexandria:starts-with #\~ (qmethod-name method))
                   (lambda (actual-instance &rest args)
                     (prog1 (funcall cont actual-instance args)
                       (note-deleted actual-instance)))
                   cont)))))))))

(declaim (inline my-line-to))
(defun my-line-to (fn method-idx object x y)
  (declare (optimize speed (safety 0))
           (double-float x y)
           (integer method-idx)
           (sb-sys:system-area-pointer fn object))
  (cffi:with-foreign-object (stack '(:union StackItem) 3)
    (setf (cffi:foreign-slot-value
           (cffi:mem-aptr stack '(:union StackItem)
                          (the (unsigned-byte 16) 1))
           '(:union StackItem)
           'double)
          x)
    (setf (cffi:foreign-slot-value
           (cffi:mem-aptr stack '(:union StackItem)
                          (the (unsigned-byte 16) 2))
           '(:union StackItem)
           'double)
          y)
    (call-class-fun fn method-idx object stack)))


|#
