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
  (setf (fast-call 'qpainterpath) (#_new QPainterPath))
  (setf (fast-call 'qscrollbar) (#_new QScrollBar))
  (setf (fast-call 'qpen) (#_new QPen))
  (setf (fast-call 'qcolor) (#_new QColor))
  (setf (fast-call 'qbrush) (#_new QBrush))
  (let ((lineto-method (find-applicable-method
                        (fast-call 'qpainterpath) "lineTo" '(0 0) nil)))
    (setf (fast-call 'lineto-method-idx)
          (qmethod-classfn-index
           lineto-method))
    (setf (fast-call 'lineto-fn)
          (qclass-trampoline-fun (qmethod-class lineto-method))))
  (let ((moveto-method (find-applicable-method
                        (fast-call 'qpainterpath) "moveTo" '(0 0) nil)))
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
  (let ((width-method (find-applicable-method
                       (fast-call 'QWidget) "width" '() nil)))
    (setf (fast-call 'width-method-idx) (qmethod-classfn-index width-method))
    (setf (fast-call 'width-fn) (qclass-trampoline-fun (qmethod-class width-method))))
  (let ((height-method (find-applicable-method
                       (fast-call 'QWidget) "height" '() nil)))
    (setf (fast-call 'height-method-idx) (qmethod-classfn-index height-method))
    (setf (fast-call 'height-fn) (qclass-trampoline-fun (qmethod-class height-method))))
  (let ((value-method (find-applicable-method
                       (fast-call 'QScrollBar) "value" '() nil)))
    (setf (fast-call 'value-method-idx) (qmethod-classfn-index value-method))
    (setf (fast-call 'value-fn) (qclass-trampoline-fun (qmethod-class value-method))))
  (let* ((rect-method (find-applicable-method
                       (fast-call 'QWidget) "rect" '() nil))
         (rtype (qmethod-return-type rect-method)))
    (setf (fast-call 'qrect-qtype-class) (qtype-class rtype))
    (setf (fast-call 'rect-method-idx) (qmethod-classfn-index rect-method))
    (setf (fast-call 'rect-fn) (qclass-trampoline-fun (qmethod-class rect-method))))
  (let* ((eraserect-method (find-applicable-method
                             (fast-call 'QPainter) "eraseRect" (list (#_new QRect)) nil)))
    (setf (fast-call 'eraserect-fn) (qclass-trampoline-fun (qmethod-class eraserect-method)))
    (setf (fast-call 'eraserect-method-idx) (qmethod-classfn-index eraserect-method))
    (setf (fast-call 'eraserect-method-argtype) (car (list-qmethod-argument-types eraserect-method))))
  (let* ((drawpath-method (find-applicable-method
                          (fast-call 'QPainter) "drawPath" (list (fast-call 'qpainterpath)) nil)))
    (setf (fast-call 'drawpath-fn) (qclass-trampoline-fun (qmethod-class drawpath-method)))
    (setf (fast-call 'drawpath-method-idx) (qmethod-classfn-index drawpath-method))
    (setf (fast-call 'drawpath-method-argtype) (car (list-qmethod-argument-types drawpath-method))))
  (let* ((closesubpath-method (find-applicable-method
                               (fast-call 'QPainterPath) "closeSubpath" () nil)))
    (setf (fast-call 'closesubpath-fn) (qclass-trampoline-fun (qmethod-class closesubpath-method)))
    (setf (fast-call 'closesubpath-method-idx) (qmethod-classfn-index closesubpath-method))
    (setf (fast-call 'closesubpath-method-argtype) (car (list-qmethod-argument-types closesubpath-method))))
  (let* ((end-method (find-applicable-method
                               (fast-call 'QPainter) "end" () nil)))
    (setf (fast-call 'end-fn) (qclass-trampoline-fun (qmethod-class end-method)))
    (setf (fast-call 'end-method-idx) (qmethod-classfn-index end-method))
    (setf (fast-call 'end-method-argtype) (car (list-qmethod-argument-types end-method))))
  (let* ((setcolor-method (find-applicable-method
                           (fast-call 'qpen) "setColor" (list (fast-call 'qcolor)) nil))
         (rtype (qmethod-return-type setcolor-method)))
    (setf (fast-call 'setcolor-fn) (qclass-trampoline-fun (qmethod-class setcolor-method)))
    (setf (fast-call 'setcolor-method-idx) (qmethod-classfn-index setcolor-method))
    (setf (fast-call 'setcolor-method-argtype) (car (list-qmethod-argument-types setcolor-method)))
    (setf (fast-call 'setcolor-cast)
          (multiple-value-list
           (resolve-cast
            (qobject-class (fast-call 'qpen))
            (qtype-class (fast-call 'setcolor-method-argtype))))))
  (let* ((setwidth-method (find-applicable-method
                               (fast-call 'QPen) "setWidth" '(0) nil)))
    (setf (fast-call 'setwidth-fn) (qclass-trampoline-fun (qmethod-class setwidth-method)))
    (setf (fast-call 'setwidth-method-idx) (qmethod-classfn-index setwidth-method))
    (setf (fast-call 'setwidth-method-argtype) (car (list-qmethod-argument-types setwidth-method))))
  (let* ((fillpath-method
          (find-applicable-method
           (fast-call 'QPainter) "fillPath" (list (fast-call 'qpainterpath)
                                                  (fast-call 'qbrush))
           nil)))
    (setf (fast-call 'fillpath-fn) (qclass-trampoline-fun (qmethod-class fillpath-method)))
    (setf (fast-call 'fillpath-method-idx) (qmethod-classfn-index fillpath-method))
    (setf (fast-call 'fillpath-method-argtype) (car (list-qmethod-argument-types fillpath-method))))
  )

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
(defun fast-begin (painter widget)
    (declare (optimize speed (safety 0))
             (qobject painter widget))
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
              (%perform-cast (qobject-pointer widget)
                             (first begin-cast)
                             (second begin-cast)
                             (third begin-cast))))
      (call-class-fun (fast-call 'begin-fn) (fast-call 'begin-method-idx) object stack))))

(declaim (inline fast-drawpath))
(defun fast-drawpath (painter path)
    (declare (optimize speed (safety 0))
             (qobject painter path))
    (let* ((object (slot-value painter 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (qobject-pointer path))
      (call-class-fun (fast-call 'drawpath-fn) (fast-call 'drawpath-method-idx) object stack))))

(declaim (inline fast-width))
(defun fast-width (qwidget)
    (declare (optimize speed (safety 0))
             (qobject qwidget))
    (let* ((object (slot-value qwidget 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
      (cffi:with-foreign-object (stack '(:union StackItem) 1)
        (declare (sb-sys:system-area-pointer stack))
      (call-class-fun (fast-call 'width-fn) (fast-call 'width-method-idx) object stack)
      (cffi:foreign-slot-value ;;; return value is at idx 0 of stack 
       (cffi:mem-aptr stack '(:union StackItem)
                      (the (unsigned-byte 16) 0))
       '(:union StackItem)
       'int))))

(declaim (inline fast-height))
(defun fast-height (qwidget)
    (declare (optimize speed (safety 0))
             (qobject qwidget))
    (let* ((object (slot-value qwidget 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
      (cffi:with-foreign-object (stack '(:union StackItem) 1)
        (declare (sb-sys:system-area-pointer stack))
      (call-class-fun (fast-call 'height-fn) (fast-call 'height-method-idx) object stack)
      (cffi:foreign-slot-value ;;; return value is at idx 0 of stack 
       (cffi:mem-aptr stack '(:union StackItem)
                      (the (unsigned-byte 16) 0))
       '(:union StackItem)
       'int))))

(declaim (inline fast-value))
(defun fast-value (qwidget)
    (declare (optimize speed (safety 0))
             (qobject qwidget))
    (let* ((object (slot-value qwidget 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
      (cffi:with-foreign-object (stack '(:union StackItem) 1)
        (declare (sb-sys:system-area-pointer stack))
      (call-class-fun (fast-call 'value-fn) (fast-call 'value-method-idx) object stack)
      (cffi:foreign-slot-value ;;; return value is at idx 0 of stack 
       (cffi:mem-aptr stack '(:union StackItem)
                      (the (unsigned-byte 16) 0))
       '(:union StackItem)
       'int))))

(declaim (inline fast-rect))
(defun fast-rect (qwidget)
    (declare (optimize speed (safety 0))
             (qobject qwidget))
    (let* ((object (slot-value qwidget 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
      (cffi:with-foreign-object (stack '(:union StackItem) 1)
        (declare (sb-sys:system-area-pointer stack))
        (call-class-fun (fast-call 'rect-fn) (fast-call 'rect-method-idx) object stack)
        (make-instance 'qobject
                       :class (fast-call 'qrect-qtype-class)
                       :pointer (cffi:foreign-slot-value ;;; return value is at idx 0 of stack 
                                 (cffi:mem-aptr stack '(:union StackItem)
                                                (the (unsigned-byte 16) 0))
                                 '(:union StackItem)
                                 'class)))))

(declaim (inline fast-eraserect))
(defun fast-eraserect (painter rect)
    (declare (optimize speed (safety 0))
             (qobject painter rect))
    (let* ((object (slot-value painter 'pointer)))
      (declare 
             (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (qobject-pointer rect))
      (call-class-fun (fast-call 'eraserect-fn) (fast-call 'eraserect-method-idx) object stack))))

(declaim (inline fast-closesubpath))
(defun fast-closesubpath (paint-path)
    (declare (optimize speed (safety 0))
             (qobject paint-path))
    (let* ((object (slot-value paint-path 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 1)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 0))
             '(:union StackItem)
             'class)
            (qobject-pointer paint-path))
      (call-class-fun (the sb-sys:system-area-pointer (fast-call 'closesubpath-fn))
                      (the integer (fast-call 'closesubpath-method-idx)) object stack))))

(declaim (inline fast-end))
(defun fast-end (paint-path)
    (declare (optimize speed (safety 0))
             (qobject paint-path))
    (let* ((object (slot-value paint-path 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 1)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 0))
             '(:union StackItem)
             'class)
            (qobject-pointer paint-path))
      (call-class-fun (the sb-sys:system-area-pointer (fast-call 'end-fn))
                      (the integer (fast-call 'end-method-idx)) object stack))))

(declaim (inline fast-setcolor))
(defun fast-setcolor (pen color)
    (declare (optimize speed (safety 0))
             (qobject pen color))
    (let* ((object (slot-value pen 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (qobject-pointer color))
      (call-class-fun (fast-call 'setcolor-fn)
                      (fast-call 'setcolor-method-idx) object stack))))

(declaim (inline fast-setwidth))
(defun fast-setwidth (pen x)
    (declare (optimize speed (safety 0))
             ((unsigned-byte 32) x)
             (qobject pen))
    (let* ((object (slot-value pen 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 2)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'long)
            x)
      (call-class-fun (the sb-sys:system-area-pointer (fast-call 'setwidth-fn))
                      (the integer (fast-call 'setwidth-method-idx)) object stack))))

(declaim (inline fast-fillpath))
(defun fast-fillpath (painter painterpath brush)
    (declare (optimize speed (safety 0))
             (qobject painter painterpath brush))
    (let* ((object (slot-value painter 'pointer)))
      (declare (sb-sys:system-area-pointer object))
    (cffi:with-foreign-object (stack '(:union StackItem) 3)
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 1))
             '(:union StackItem)
             'class)
            (qobject-pointer painterpath))
      (setf (cffi:foreign-slot-value
             (cffi:mem-aptr stack '(:union StackItem)
                            (the (unsigned-byte 16) 2))
             '(:union StackItem)
             'class)
            (qobject-pointer brush))
      (call-class-fun (fast-call 'fillpath-fn)
                      (fast-call 'fillpath-method-idx) object stack))))


#|

;; (init-qt-fast-calls)

(qmethod-return-type
 (find-applicable-method
  (fast-call 'qpen) "setColor" (list (fast-call 'qcolor)) nil))

(let* ((pen (#_new QPen))
       (width 1))
  (fast-setwidth pen width))

Example: setcolor

(let* ((obj (fast-call 'qpen))
       (<from> (qobject-class obj))
       (<type> (fast-call 'setcolor-method-argtype)))
  (multiple-value-bind (castfn <from> <to>)
      (resolve-cast <from> (qtype-class <type>))
    (list castfn <from> <to>)
    ))


(multiple-value-list
 (resolve-cast
  (qobject-class (#_new QRect))
  (qtype-class (fast-call 'eraserect-method-argtype))))

;;;



(declare (fixnum <from> <to>))
                 (if (eql <from> <to>)
                     (lambda (val stack i)
                       (setf (si class)
                             (qobject-pointer val)))
                     (lambda (val stack i)
                       (setf (si class)
                             (%perform-cast (qobject-pointer val)
                                            castfn <from> <to>))))


(let* ((pen (#_new QPen))
       (width 1))
)
(let* ((painter (#_new QPainter))
       (path (#_new QPainterPath)))
  (#_drawPath painter path))

(get (qtype-interned-name 84743) 'marshaller/primary)
(get (qtype-interned-name 84743) 'marshaller/around)

#<QPainterPath 0x7F4E70001590>
(qtype-stack-item-slot 84743)

(type-of )

(resolve-cast (qobject-class (#_new QPainterPath)) (qtype-class 84743))

(defun set-thunk (obj <type>)
  (macrolet
      ((si (slot)
         `(cffi:foreign-slot-value
           (cffi:mem-aptr stack '(:union StackItem)
                          (the (unsigned-byte 16) i))
           '(:union StackItem)
           ',slot))
       (dispatching ((getter slot) &body body)
         `(ecase ,slot
            ,@ (mapcar (lambda (slot)
                         `((,slot)
                           (macrolet ((,getter () `(si ,',slot)))
                             ,@body)))
                '(ptr bool char uchar short ushort int
                  uint long ulong float double enum class)))))
    (let ((slot (qtype-stack-item-slot <type>)))
      (case slot
        (bool
         (lambda (val stack i) (setf (si bool) (if val 1 0))))
        (class
         (if (typep obj 'qobject)
             (let ((<from> (qobject-class obj)))
               (multiple-value-bind (castfn <from> <to>)
                   (resolve-cast <from> (qtype-class <type>))
                 (declare (fixnum <from> <to>))
                 (if (eql <from> <to>)
                     (lambda (val stack i)
                       (setf (si class)
                             (qobject-pointer val)))
                     (lambda (val stack i)
                       (setf (si class)
                             (%perform-cast (qobject-pointer val)
                                            castfn <from> <to>))))))
             (lambda (val stack i)
               (setf (si class)
                     (if (typep val 'cffi:foreign-pointer)
                         val
                         (qobject-pointer val))))))
        (enum
         (etypecase obj
           (integer
            (lambda (val stack i) (setf (si enum) val)))
           (enum
            (lambda (val stack i) (setf (si enum) (enum-value val))))))
        (int
         (etypecase obj
           (integer
            (lambda (val stack i) (setf (si int) val)))
           (enum
            (lambda (val stack i) (setf (si int) (enum-value val))))))
        (uint
         (etypecase obj
           (integer
            (lambda (val stack i) (setf (si uint) val)))
           (enum
            (lambda (val stack i) (setf (si uint) (enum-value val))))))
        (float
         (lambda (val stack i)
           (setf (si float) (float val 1.0s0))))
        (double
         (lambda (val stack i)
           (setf (si double) (float val 1.0d0))))
        ;; that leaves:
        ;;   ptr char uchar short ushort int uint long ulong
        (t
         (dispatching (%si slot)
                      (lambda (val stack i)
                        (setf (%si) val))))))))

(QT::ARGLIST-MARSHALLER (#<QPainterPath 0x7F4E70001030>) (84743))

(QT::MARSHALLER (#_new QRect) 86407)

(qobject-pointer (#_new QRect))

(resolve-cast (qobject-pointer (#_new QRect)) (qtype-class 86407))



#_drawPath

(let* ((eraseRect-method (find-applicable-method
                      (fast-call 'QPainter) "eraseRect" (list (#_new QRect)) nil)))
  (list
   (list-qmethod-argument-types eraseRect-method)
   (qmethod-classfn-index eraserect-method)
   (qmethod-return-type eraseRect-method)))

((86407) 200 7)

(let* ((height-method (find-applicable-method
                       (fast-call 'QWidget) "height" '() nil)))
    (qmethod-return-type height-method))

(let ((drawpath-method (find-applicable-method
                          (fast-call 'QPainter) "drawPath" (list (fast-call 'qpainter-path)) nil)))
  (list
   (list-qmethod-argument-types drawpath-method)
   (qmethod-classfn-index drawpath-method)
   (qmethod-return-type drawpath-method)
   ))
-> ((84743) 77 7)

(let 
(fast-rect (#_new QWidget))

(fast-eraserect (#_new QPainter) (#_new QRect)))

|#


#|
(let ((method (find-applicable-method (fast-call 'qpainter-path) "closeSubpath" '() nil)))
  (list-qmethod-argument-types method))

(let ((method (find-applicable-method (fast-call 'qpainter-path) "drawPath" (list (fast-call 'QPainter)) nil)))
  (list-qmethod-argument-types method))

(let ((method (find-applicable-method (fast-call 'QPainter) "drawPath" (list (fast-call 'qpainter-path)) nil)))
  (list-qmethod-argument-types method))
|#



#|


(let* ((painter (#_new QPainter))
       (painterpath (#_new QPainterPath))
       (brush (#_new QBrush)))

  (fast-fillpath painter painterpath brush))


(let* ((color (fast-call 'qcolor))
       (setcolor-cast (fast-call 'setcolor-cast)))
  (list (qobject-pointer color)
        (first setcolor-cast)
        (second setcolor-cast)
        (third setcolor-cast)
        (%perform-cast (qobject-pointer color)
                       (first setcolor-cast)
                       (second setcolor-cast)
                       (third setcolor-cast))))
#_fillPath 

(let ((painter (#_new QPainter))
      (widget (#_new QWidget)))

  (fast-begin painter widget)
  (fast-end painter)

  )

(let ((painter (#_new QPainter))
      (color (#_new QColor)))

(fast-setcolor painter color)

  )

(let* ((painter (#_new QPainter))
(brush (#_new QBrush)))

(fast-setcolor painter color)

  )

(qtype-class (fast-call ))

(qobject-pointer val)

(qobject-pointer val)

(fast-closeSubpath (#_new QPainterPath))

(let ((<from> (qobject-class obj)))
  (multiple-value-bind (castfn <from> <to>)
      (resolve-cast <from> (qtype-class <type>))
    (declare (fixnum <from> <to>))
    (if (eql <from> <to>)
        (lambda (val stack i)
          (setf (si class)
                (qobject-pointer val)))
        (lambda (val stack i)
          (setf (si class)
                (%perform-cast (qobject-pointer val)
                               castfn <from> <to>))))))
(#_eraseRect (#_new QPainter) (#_new QRect))
(fast-eraserect (#_new QPainter) (#_new QRect))
(funcall )

(read-smoke-lambda "(#_drawPath (#_new QPainter) (#_new QPainterPath))" #\_ NIL)


 ;

 ;

(trace qt::lambda-)                                      ;


;;; (setf (fast-call 'eraserect-fn) (qt::resolve-call t (#_new QPainter) "eraseRect" (list (#_new QRect)) NIL))
(defun fast-eraserect (painter instance)
  (funcall (fast-call 'eraserect-fn) painter (list instance)))


(multiple-value-bind (buffer length)
    (sb-alien::make-alien-string "foo")
  (format t "buffer: ~a~%length: ~a~%" buffer length))

(time
 (dotimes (i 10000)
   (fast-rect (fast-call 'qwidget))))

(time
(dotimes (i 100)
(fast-rect (fast-call 'qwidget))))

(time
 (dotimes (i 10000)
   (fast-call 'qwidget)))

(time
 (dotimes (i 10000)
   (#_rect (fast-call 'qwidget))))

(#_eraseRect (#_new QPainter) (#_rect (fast-call 'qwidget)))
(fast-eraseRect (#_new QPainter) (#_rect (fast-call 'qwidget)))


(untrace)                                      ;



(OPTIMIZED-CALL T ((LAMBDA () (OPTIMIZED-NEW "QPainter"))) "eraseRect"
      ((LAMBDA ()
                            (OPTIMIZED-CALL T (FAST-CALL 'QWIDGET) "rect"))))

(qt:new)                                      ;

(qt::fast-eraseRect (#_new QPainter) (#_rect (fast-call 'qwidget)))


11(let ((class (fast-call 'qrect-qtype-class)))
  (qsubclassp class (find-qclass "QObject")))




(defun %qobject (class ptr)
  (let ((cached (pointer->cached-object ptr)))
    (if (and cached
             (qsubclassp (qobject-class cached) class))
        cached
        (if (cffi:null-pointer-p ptr)
            (make-instance 'null-qobject :class class)
            (let ((actual-class (or (when (qsubclassp class (find-qclass "QObject"))
                                      (instance-qclass ptr nil))
                                    class)))
              (make-instance 'qobject :class actual-class :pointer ptr))))))

(let* ((method (find-applicable-method
                       (fast-call 'QWidget) "rect" '() nil))
       (rtype (qmethod-return-type method)))
  (list rtype (unmarshaller rtype)))

(defun unmarshaller (type)
  (if (qtype-void-p type)
      (constantly nil)
      (let ((thunk (unmarshaller-2 type)))
        (dispatching-on-stack-item (get-value (qtype-stack-item-slot type))
          (lambda (stack-item)
            (funcall thunk (get-value stack-item) type))))))

(defun unmarshaller-2 (type)
(let ((name (qtype-name type)))
    (or (get-static-unmarshaller name)
        (case (qtype-stack-item-slot type)
          (class
           (let ((qtype-class (qtype-class type)))
             (lambda (value type)
               (declare (ignore type))
               (%qobject qtype-class value))))
          (enum
           (let ((type-name (qtype-interned-name type)))
             (lambda (value type)
               (declare (ignore type))
               (enum value type-name))))
          (t
           (lambda (value type)
             (declare (ignore type))
             value))))))


;;; qtype-name: "QRect"

qrect-type
(%qobject qrect-qtype-class stack-item)

(let ((type 42375))
;;  (unmarshaller-2 type)
  (qtype-name type)
  (qtype-stack-item-slot type)
  (qtype-class type)
)

(#_value (#_new QScrollBar))

'int

(qtype-stack-item-slot
 (qmethod-return-type
  (find-applicable-method
   (fast-call 'QWidget) "width" '() nil)))

(unmarshaller-2 96327)

(get-static-unmarshaller (qtype-name 96327))

(qtype-stack-item-slot 96327)

(time
 (dotimes (i 1000) (#_width (fast-call 'qwidget))))

(time
 (dotimes (i 1000) (#_width (fast-call 'qwidget))))

(let ((method))) 

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


(time
 (dotimes (i 1000) (fast-width (fast-call 'qwidget))))

|#
