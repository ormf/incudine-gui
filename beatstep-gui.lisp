;;; 
;;; beatstep-gui.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package #:incudine-gui)
(named-readtables:in-readtable :qt)

;;; Zusammenfassung von Parameter und seinem Label. Da Label und
;;; parameter in einem globalen Grid angeordnet werden sollen, werden
;;; sie hier nur instantieert, aber noch nicht einem Layout
;;; zugewiesen. Das passiert dann in beatstep-grid.

(defparameter *beatstep-box-style*
  "border: 1px solid #838383; 
background-color: #dddddd;
selection-color: black;
cursor-color: red;
border-radius: 2px;
selection-background-color: white")


(defclass custom-spinbox ()
  ((style :initform *beatstep-box-style* :initarg :style :accessor style))
  (:metaclass qt-class)
  (:qt-superclass "QSpinBox")
  (:signals
   ("returnPressed(int)")
   ("setValue(int)"))
  (:override
   ("keyPressEvent" key-press-event)))

(defmethod initialize-instance :after ((instance custom-spinbox) &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(defclass label-spinbox ()
  ((label :initform "" :initarg :label :accessor label)
   (text :initform "" :initarg :text :accessor text)
   (id :initform 0 :initarg :id :accessor id)
   (callback :initform #'identity :accessor callback)
   (label-box :initform (#_new QLabel) :accessor label-box)
   (text-box :initform (make-instance 'custom-spinbox) :accessor text-box))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots
   ("setValue(int)" set-pvb-value)
   ("recallValue(int)" recall-pvb-value))
  (:signals
   ("setValue(int)")
   ("setLabel(QString)")))

(defmethod initialize-instance :after ((instance label-spinbox) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (with-slots (text-box label-box label text) instance
    (#_setStyleSheet text-box *beatstep-box-style*)
    ;;    (#_setFixedWidth instance 45)
    (#_setFixedHeight text-box 25)
    (#_setAlignment text-box (#_AlignLeft "Qt"))
;;    (#_setText text-box text)
;;    (#_setReadOnly text-box t)
    (#_setText label-box label)
    (connect instance "setLabel(QString)" label-box "setText(QString)")
    (connect text-box "valueChanged(int)" instance "setValue(int)")
    (connect instance "setvalue(int)" instance "setValue(int)")
    (connect text-box "returnPressed(int)" instance "recallValue(int)")))

(defclass beatstep-grid (cudagui-tl-mixin)
  ((rows :initform 2 :initarg :rows :accessor rows)
   (cols :initform 8 :initarg :cols :accessor cols)
   (popup-menu :initform (make-instance 'pushbutton) :accessor popup-menu)

   (load-action :accessor load-action) ;;; the action accessors for the menu
   (save-action :accessor save-action)
   (saveas-action :accessor saveas-action)
   (param-boxes :initform (make-array 16) :accessor param-boxes)
   (audio-args :initform (#_new QTextEdit) :accessor audio-args)
   (midi-cc-fns :initform (#_new QTextEdit) :accessor midi-cc-fns)
   (midi-note-fns :initform (#_new QTextEdit) :accessor midi-note-fns))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots
   ("loadAction()" do-load-action)
   ("saveAction()" do-save-action)
   ("saveasAction()" do-saveas-action)
   )
  (:signals
   ("setAudioArgs(QString)")
   ("setMidiCCFns(QString)")
   ("setMidiNoteFns(QString)"))
  (:override
   ("closeEvent" close-event)))

(defmethod initialize-instance :after ((instance beatstep-grid) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer instance))
  (with-slots (param-boxes popup-menu rows cols audio-args midi-cc-fns midi-note-fns
               load-action save-action saveas-action
               )
      instance
    (let ((main (#_new QVBoxLayout instance))
          (grid (#_new QGridLayout))
          )
      (make-button-menu popup-menu :actions
                  ((load-action "Load")
                         (save-action  "Save")
                         (saveas-action "Save As"))
                        :label "File")
      (#_setText popup-menu "File")
      (#_setStyleSheet popup-menu *beatstep-box-style*)
      (#_setStyleSheet audio-args *beatstep-box-style*)
      (#_setStyleSheet midi-cc-fns *beatstep-box-style*)
      (#_setStyleSheet midi-note-fns *beatstep-box-style*)
      (loop for row below rows
         do (loop for column below (* 2 cols) by 2
                  do (let* ((idx (+ (/ column 2) (* 8 row)))
                            (new-lsbox (make-instance 'label-spinbox :label (format nil "~a:" (1+ idx))
                                                                      :text "--"
                                                                      :id (1+ idx))))
                       (#_setRange (text-box new-lsbox) 0 127)
                       (setf (aref param-boxes idx) new-lsbox)
                       
;;                    (#_setStyleSheet (text-box new-lsbox) *beatstep-box-style*)
                    (let ((lsboxlayout (#_new QHBoxLayout)))
                      (#_addWidget grid (label-box new-lsbox) (1+ row)  column)
                      (#_addWidget lsboxlayout (text-box new-lsbox))
                      (#_addStretch lsboxlayout)
                      (#_addLayout grid lsboxlayout (1+ row) (1+ column))))))
      (#_addLayout main grid)
;      (#_addStretch main)
      (#_addWidget main audio-args)
;      (#_addStretch main)
      (#_addWidget main midi-cc-fns)
;      (#_addStretch main)
      (#_addWidget main midi-note-fns)
      (#_setReadOnly audio-args t)
      (#_setReadOnly midi-cc-fns t)
      (#_setReadOnly midi-note-fns t))
    (connect instance "setAudioArgs(QString)" audio-args "setText(QString)")
    (connect instance "setMidiCCFns(QString)" midi-cc-fns "setText(QString)")
    (connect instance "setMidiNoteFns(QString)" midi-note-fns "setText(QString)")
    (connect load-action "triggered()" instance "loadAction()")
    (connect save-action "triggered()" instance "saveAction()")
    (connect saveas-action "triggered()" instance "saveasAction()")))

(defmethod set-pvb-value ((instance label-spinbox) value)
  (funcall (callback instance) (#_value (text-box instance))))

(defmethod recall-pvb-value ((instance label-spinbox) value)
  (funcall (callback instance) (#_value (text-box instance))))


;;; popup Menu actions (not implemented in Beatstep)

(defun do-load-action (beatstep-grid)
  (let ((file (#_QFileDialog::getOpenFileName
               beatstep-grid "Load"
               ;; (namestring luftstrom-display::*bs-presets-file*)
               "*.lisp")))
    (if (string= file "") (format t "~&canceled.")
        ;; (luftstrom-display::restore-bs-presets file)
        )))

(defun do-save-action (beatstep-grid)
  (declare (ignore beatstep-grid))
  ;; (luftstrom-display::store-bs-presets)
  )

(defun do-saveas-action (beatstep-grid)
  (let ((file (#_QFileDialog::getSaveFileName
               beatstep-grid "Save As"
               ;; (namestring luftstrom-display::*bs-presets-file*)
               "*.lisp")))
    (if (string= file "") (format t "~&canceled.")
        ;; (luftstrom-display::store-bs-presets file)
        )))



(defmethod close-event ((instance beatstep-grid) ev)
  (declare (ignore ev))
  (remove-gui (id instance))
  (stop-overriding))

(defmethod key-press-event ((instance custom-spinbox) ev)
;;;  (format t "~%~a, ~a~%" (#_key ev) (#_modifiers ev))
  (cond ;; Signal Ctl-Space pressed.
    ((= (#_key ev) 16777220)
     (case (#_modifiers ev)
       (0 (emit-signal instance "returnPressed(int)" 0))
       (100663296 (emit-signal instance "returnPressed(int)" 1))))
    ;; Delegate standard.
    (T
     (call-next-qmethod))))

(defgeneric set-fader (obj idx val))

(defmethod set-fader ((instance beatstep-grid) idx val)
  (cuda-gui::gui-funcall
   (#_setValue (text-box (aref (param-boxes instance) idx)) val)))

(defgeneric inc-fader (obj idx val))

(defmethod inc-fader ((instance beatstep-grid) idx inc)
  (cuda-gui::gui-funcall
   (#_setValue (text-box (aref (param-boxes instance) idx))
               (+ (#_value (text-box (aref (param-boxes instance) idx))) inc))))

(defgeneric set-callback (obj idx fn))

(defmethod set-callback ((instance beatstep-grid) idx fn)
  (setf (callback (aref (param-boxes instance) idx)) fn))

(defun beatstep-gui (&key (id :bs1))
  (create-tl-widget 'beatstep-grid id))

#|
(setf (callback (aref (param-boxes (find-gui :pv1)) 9))
      (lambda (x) (format t "val: ~a" x)))

(funcall (callback (aref (param-boxes (find-gui :pv1)) 9))
         13)

(beatstep-gui :id :bs2)                                      ;



(#_setValue (aref (param-boxes (find-gui :pv1)) 9) 10)


(#_setValue (text-box (aref (param-boxes (find-gui :pv1)) 9)) 11)

(incudine::add-receiver cm::*midi-in)


(emit-signal (aref (param-boxes (find-gui :pv1)) 9) "setValue(int)" 25)

(emit-signal (text-box (aref (param-boxes (find-gui :pv1)) 9)) "setValue(int)" 25)



(create-tl-widget 'beatstep-grid :pv1)

(set-fader (find-gui :pv1) 1 72)

(inc-fader (find-gui :pv1) 1 -1)

(close-all-guis)
|#
