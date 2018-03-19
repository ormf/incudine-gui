;;;; incudine-setup.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)
(export 'setup-io :scratch)
(export 'node-free-unprotected :scratch)
 
(defmacro foreach-input-channel (&body body)
  (with-gensyms (i)
    `(dochannels (,i *number-of-input-bus-channels*)
       (let ((current-channel ,i))
         (declare (type channel-number current-channel)
                  (ignorable current-channel))
         ,@body))))




(dsp! cp-input-buses ((first-in-bus channel-number))
  (:defaults 0)
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (setf (bus (+ current-channel first-in-bus)) (audio-in current-channel))))
)


(dsp! cp-output-buses ((first-out-bus channel-number))
  (:defaults 8)
  (foreach-frame
    (foreach-channel
      (setf (bus (+ current-channel first-out-bus)) (audio-out current-channel)))))

(defun setup-io ()
  (make-group 100)
  (make-group 200 :after 100)
  (make-group 300 :after 200)
  (make-group 400 :after 300)
  (cp-input-buses :head 100)
  (cp-output-buses :head 300))

(defun node-free-unprotected ()
 (dogroup (n (node 200))
   (free n)))

;;; (setup-io)
;;; (block-size)
;;; (set-rt-block-size 256)
;;; (rt-start)

