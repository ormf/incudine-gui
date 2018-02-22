;;;; gui-registry.asd
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

;;; central registry for all guis. Entries have to be added in the gui
;;; instantiation code and removed in their removal code (reacting to
;;; a close signal). Keys are the id (name) of the gui, values are the
;;; qt instances. In case of a toplevel-gui the id is also used as
;;; window-title.

(defvar *guis* (make-hash-table :test #'equal))

;; (define-condition )

(defun add-gui (id gui &key (db *guis*))
  (if (gethash id db)
      (error "gui ~s already exists!" id)
      (setf (gethash id db) gui)))

(defun remove-gui (id &key (db *guis*))
  (unless (remhash id db) ;;; the warning should never happen...
    (warn "couldn't remove gui, ~s not found in database!" id)))

(defun find-gui (id &key (db *guis*))
  (gethash id db))

#|

(add-gui "test" 'Hallo)
(add-gui :test 'Hallo)
(find-gui :TEST)
(find-gui "test")

(remove-gui :test)
(remove-gui "test")

|#
