
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
      (setf (bus (+ current-channel first-in-bus)) (audio-in current-channel)))))

(dsp! cp-output-buses ((first-out-bus channel-number))
  (:defaults 8)
  (foreach-frame
    (setf (bus (+ 0 first-out-bus)) (audio-out current-channel))
    (setf (bus (+ 1 first-out-bus)) (audio-out current-channel))))

(dsp! mix-to-output ((startidx channel-number) (numchannels channel-number))
  (:defaults 16 8)
  (foreach-frame
    (dochannels (current-channel numchannels)
      (incf (audio-out current-channel) (bus (+ current-channel startidx))))))

(dsp! clear-buses ((startidx channel-number) (numchannels channel-number))
  (:defaults 16 8)
  (foreach-frame
    (dochannels (current-channel numchannels)
      (setf (bus (+ current-channel startidx)) +sample-zero+))))

(defun setup-io ()
  (free 0)
  (sleep 0.1)
  (make-group 100)
  (make-group 200 :after 100)
  (make-group 300 :after 200)
  (make-group 400 :after 300)
  (clear-buses 16 8 :head 100)
  (cp-input-buses :tail 100)
  (mix-to-output :startidx 16 :head 300)
  (cp-output-buses :tail 300))

(defun node-free-unprotected ()
 (dogroup (n (node 200))
   (free n)))

;;; (setup-io)
;;; (dump (node 0))
;;; (block-size)
;;; (set-rt-block-size 256)
;;; (rt-start)

