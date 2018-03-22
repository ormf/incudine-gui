(in-package :qt)
(named-readtables:in-readtable :qt)

(defvar *widget* nil)
(defvar *lineto-method-idx* nil)
(defvar *lineto-fn* nil)
(defvar *moveto-method-idx* nil)
(defvar *moveto-fn* nil)
(defvar *begin-method-idx* nil)
(defvar *begin-method-argtype* nil)
(defvar *begin-fn* nil)
(defvar *begin-method-idx* nil)
(defvar *begin-method-argtype* nil)
(defvar *begin-cast* nil)
(defvar *begin-fn* nil)
(defvar *closesubpath-method-idx* nil)
(defvar *closesubpath-fn* nil)
(defvar *drawPath-method-idx* nil)
(defvar *drawPath-method-argtype* nil)
(defvar *drawPath-fn* nil)


(defun init-qt-fast-calls ()
  (setf *widget* (#_new QWidget))
  (let ((lineto-method (qt::find-applicable-method
                         (#_new QPainterPath) "lineTo" '(0 0) nil)))
        (setf *lineto-method-idx*
              (qt::qmethod-classfn-index
               lineto-method))
        (setf *lineto-fn*
              (qt::qclass-trampoline-fun (qt::qmethod-class lineto-method))))
  (let ((moveto-method (qt::find-applicable-method
                         (#_new QPainterPath) "moveTo" '(0 0) nil)))
        (setf *moveto-method-idx*
              (qt::qmethod-classfn-index
               moveto-method))
        (setf *moveto-fn*
              (qt::qclass-trampoline-fun (qt::qmethod-class moveto-method))))
  (let ((begin-method (qt::find-applicable-method
                       (#_new QPainter) "begin" (list (#_new QWidget)) nil)))


    (setf *begin-method-idx* (qt::qmethod-classfn-index begin-method))
    (setf *begin-method-argtype* (car (list-qmethod-argument-types begin-method)))
    (setf *begin-fn* (qt::qclass-trampoline-fun (qt::qmethod-class begin-method)))
    (setf *begin-cast*
          (multiple-value-list (resolve-cast (qobject-class *widget*)
                                     (qtype-class *begin-method-argtype*)))))
  (let ((drawpath-method (qt::find-applicable-method
                       (#_new QPainter) "drawPath" (list (#_new QPainterPath)) nil)))
    (setf *drawpath-method-idx* (qt::qmethod-classfn-index drawpath-method))
    (setf *drawpath-method-argtype* (car (list-qmethod-argument-types drawpath-method)))
    (setf *begin-fn* (qt::qclass-trampoline-fun (qt::qmethod-class drawpath-method)))))

#|
(let ((method (qt::find-applicable-method (#_new QPainterPath) "closeSubpath" '() nil)))
  (list-qmethod-argument-types method))

(let ((method (qt::find-applicable-method (#_new QPainterPath) "drawPath" (list (#_new QPainter)) nil)))
  (list-qmethod-argument-types method))

(let ((method (qt::find-applicable-method (#_new QPainter) "drawPath" (list (#_new QPainterPath)) nil)))
  (list-qmethod-argument-types method))
|#
;; (init-qt-fast-calls)

(declaim (inline fast-lineto))
(defun fast-lineto (paint-path x y)
    (declare (optimize speed (safety 0))
             (double-float x y)
             (qobject paint-path))
    (let* ((object (slot-value paint-path 'qt::pointer)))
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
      (qt::call-class-fun (the sb-sys:system-area-pointer *lineto-fn*)
                          (the integer *lineto-method-idx*) object stack))))

(declaim (inline fast-moveto))
(defun fast-moveto (paint-path x y)
    (declare (optimize speed (safety 0))
             (double-float x y)
             (qobject paint-path))
    (let* ((object (slot-value paint-path 'qt::pointer)))
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
      (qt::call-class-fun (the sb-sys:system-area-pointer *moveto-fn*)
                          (the integer *moveto-method-idx*) object stack))))

#|

(defparameter *widget* (#_new QWidget))
(defparameter *steth-view* (make-instance 'cuda-gui::stethoscope-view))

(let ((paint-path (#_new QPainterPath))
      (painter (#_new QPainter))
      (instance *widget*))
  (sb-profile:reset)
  (time (dotimes (i 1000)
          (fast-begin painter instance)))
  (sb-profile:report :print-no-call-list nil)
)

(trace resolve-cast)

(let ((paint-path (#_new QPainterPath))
      (painter (#_new QPainter))
      (instance (make-instance 'cuda-gui::stethoscope-view)))
  (sb-profile:reset)
  (time
   (dotimes (i 1000)
           (fast-begin painter instance)))
  (sb-profile:report :print-no-call-list nil))

(resolve-cast 28868 15620)

(let ((paint-path (#_new QPainterPath))
      (painter (#_new QPainter))
      (instance (make-instance 'cuda-gui::stethoscope-view)))
  (fast-drawpath painter paint-path))
|#

(declaim (inline fast-begin))
(defun fast-begin (painter instance)
    (declare (optimize speed (safety 0))
             (qobject painter instance))
    (let* ((object (slot-value painter 'qt::pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (%perform-cast (qobject-pointer instance)
                           (first *begin-cast*)
                           (second *begin-cast*)
                           (third *begin-cast*)))
      (qt::call-class-fun *begin-fn* *begin-method-idx* object stack)
      )))


(untrace)

(declaim (inline fast-drawpath))
(defun fast-drawpath (painter instance)
    (declare (optimize speed (safety 0))
             (qobject painter instance))
    (let* ((object (slot-value painter 'qt::pointer)))
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
                              (qtype-class *drawpath-method-argtype*))
              (%perform-cast (qobject-pointer instance)
                             castfn <from> <to>)))
      (qt::call-class-fun *drawpath-fn* *drawpath-method-idx* object stack))))


(declaim (inline opt-end-painter))
(defun opt-end-painter (fn method-idx object)
    (declare (optimize speed (safety 0))
             (integer method-idx)
             (sb-sys:system-area-pointer fn object))
    (cffi:with-foreign-object (stack '(:union StackItem) 1)
      (call-class-fun fn method-idx object stack)))

#|


(defparameter *instance* (make-instance 'cuda-gui::stethoscope-view))
(defparameter *painter* (#_new QPainter))

(let* ((instance (make-instance 'cuda-gui::stethoscope-view))
       (painter (#_new QPainter instance)))

  (#_begin painter instance)
)

(#_begin *painter* *instance*)

(qtype-stack-item-slot 35655)

(typep *instance* 'qobject)

(SET-THUNK #<INCUDINE-GUI::STETHOSCOPE-VIEW QWidget 0x7FFFBC000E30> 35655)


(qtype-stack-item-slot <type>)

(qt::find-applicable-method
 (#_new QPainter) "begin" (list *instance*) nil)


(#_begin *painter* *instance*)

(arglist-marshaller)

(list-qmethod-argument-types (qt::find-applicable-method
 (#_new QPainter) "begin" (list *instance*) nil))

(qt::find-applicable-method
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
