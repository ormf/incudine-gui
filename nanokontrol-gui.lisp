;;; 
;;; nanokontrol-gui.lisp
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
;;; zugewiesen. Das passiert dann in nanokontrol-grid.

(defparameter *nanokontrol-box-style*
  "border: 1px solid #838383; 
background-color: #dddddd;
selection-color: black;
cursor-color: red;
border-radius: 2px;
text-align: right;
selection-background-color: white")


(defparameter *nanokontrol-pushbutton-style*
"
QPushButton {
         border: 2px solid #838383; 
         border-style: outset;
         background-color: #dddddd;
         cursor-color: white;
         border-radius: 2px;
         min-width: 40px;
     }
")

(defparameter *nanokontrol-pushbutton-style*
"
QPushButton {
         background-color: #dddddd;
         min-width: 40px;
     }
")

(defun empty-fn (&rest args) (declare (ignore args)) (values))

(defclass nanokontrol-grid (cudagui-tl-mixin)
  ((rows :initform 2 :initarg :rows :accessor rows)
   (cols :initform 8 :initarg :cols :accessor cols)
   (cleanup-fn :initform #'empty-fn :initarg :cleanup-fn :accessor cleanup-fn)
   (popup-menu :initform (make-instance 'pushbutton) :accessor popup-menu)

   (load-action :accessor load-action) ;;; the action accessors for the menu
   (save-action :accessor save-action)
   (saveas-action :accessor saveas-action)
   (param-boxes :initform (make-array 16) :accessor param-boxes)
   (buttons :initform (make-array 16) :accessor buttons)
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

(defmethod initialize-instance :after ((instance nanokontrol-grid) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((*background-color* "background-color: #999999;"))
    (cudagui-tl-initializer instance))
  (with-slots (param-boxes buttons popup-menu rows cols audio-args midi-cc-fns midi-note-fns
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
      (#_setStyleSheet popup-menu *nanokontrol-box-style*)
      (#_setStyleSheet audio-args *nanokontrol-box-style*)
      (#_setStyleSheet midi-cc-fns *nanokontrol-box-style*)
      (#_setStyleSheet midi-note-fns *nanokontrol-box-style*)
      (loop for row below rows
         do (loop for column below (* 2 cols) by 2
                  do (let* ((idx (+ (/ column 2) (* 8 row)))
                            (new-lsbox (make-instance 'label-spinbox :label (format nil "~d" (1+ idx))
                                                                      :text "--"
                                                                      :id (1+ idx))))
                       (#_setRange (text-box new-lsbox) 0 127)
                       (setf (aref param-boxes idx) new-lsbox)
                       (let ((lsboxlayout (#_new QHBoxLayout)))
                      (#_addWidget grid (label-box new-lsbox) (1+ row)  column)
                      (#_addWidget lsboxlayout (text-box new-lsbox))
                      (#_addStretch lsboxlayout)
                      (#_addLayout grid lsboxlayout (1+ row) (1+ column))))))
      (loop for row below rows
         do (loop for column below (* 2 cols) by 2
                  do (let* ((idx (+ (/ column 2) (* 8 row)))
                            (new-button (make-instance 'toggle :state 0 :id (1+ idx))))
                       (setf (aref buttons idx) new-button)
                       (#_setFocusPolicy new-button (#_StrongFocus "Qt"))
                       (#_setFixedHeight new-button 25)
                    (let ((buttonlayout (#_new QHBoxLayout)))
                      (#_addWidget buttonlayout new-button)
                      (#_addStretch buttonlayout)
                      (#_addLayout grid buttonlayout (+ 3 row) (1+ column))))))
      (#_addLayout main grid))
    (connect load-action "triggered()" instance "loadAction()")
    (connect save-action "triggered()" instance "saveAction()")
    (connect saveas-action "triggered()" instance "saveasAction()")))

;;; popup Menu actions (not implemented in Nanokontrol)

(defun do-load-action (nanokontrol-grid)
  (let ((file (#_QFileDialog::getOpenFileName
               nanokontrol-grid "Load"
               ;; (namestring luftstrom-display::*bs-presets-file*)
               "*.lisp")))
    (if (string= file "") (format t "~&canceled.")
        ;; (luftstrom-display::restore-bs-presets file)
        )))

(defun do-save-action (nanokontrol-grid)
  (declare (ignore nanokontrol-grid))
  ;; (luftstrom-display::store-bs-presets)
  )

(defun do-saveas-action (nanokontrol-grid)
  (let ((file (#_QFileDialog::getSaveFileName
               nanokontrol-grid "Save As"
               ;; (namestring luftstrom-display::*bs-presets-file*)
               "*.lisp")))
    (if (string= file "") (format t "~&canceled.")
        ;; (luftstrom-display::store-bs-presets file)
        )))

(defmethod close-event ((instance nanokontrol-grid) ev)
  (declare (ignore ev))
  (remove-gui (id instance))
  (funcall (cleanup-fn instance))
  (stop-overriding))

(defmethod set-fader ((instance nanokontrol-grid) idx val)
  (emit-signal (aref (param-boxes instance) idx) "setValue(int)" val))

(defgeneric set-encoder-callback (obj idx fn))

(defmethod set-encoder-callback ((instance nanokontrol-grid) idx fn)
  (setf (callback (aref (param-boxes instance) idx)) fn))

(defgeneric set-pushbutton-callback (obj idx fn))

(defmethod set-pushbutton-callback ((instance nanokontrol-grid) idx fn)
  (setf (callback (aref (buttons instance) idx)) fn))

(defun nanokontrol-gui (&key (id :nk2))
  (if (find-gui id) (progn (close-gui id) (sleep 1)))
  (create-tl-widget 'nanokontrol-grid id))