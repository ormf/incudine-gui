;;;; levelmeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)

(export 'meters :scratch)

(declaim (inline round-sample))

(defun round-sample (x)
  (declare (type (sample
                  #.(coerce (ash most-negative-fixnum -1) 'sample)
                  #.(coerce (ash most-positive-fixnum -1) 'sample)) x))
  (multiple-value-bind (result rem) (round x)
    (declare (ignore rem))
    result))

#|

(defvar *gui-bus-pointer*
  (incudine.external:foreign-alloc-sample *number-of-gui-bus-channels*))
(declaim (type cffi:foreign-pointer *gui-bus-pointer*))

(declaim (inline gui-bus))
(defun gui-bus (num)
  (declare (type bus-number num))
  (smp-ref *gui-bus-pointer* num))

(declaim (inline set-gui-bus))
(defun set-gui-bus (num value)
  (declare (type bus-number num))
  (setf (smp-ref *gui-bus-pointer* num) (sample value)))

(defsetf gui-bus set-gui-bus)


Some ideas about the buses:

    ;; The last 200 buses are reserved for GUI.
    (define-constant +gui-bus-offset+ (- *number-of-bus-channels* 200))

    (declaim (inline gui-bus))
    (defun gui-bus (num)
      (bus (+ +gui-bus-offset+ num)))

    (declaim (inline set-gui-bus))
    (defun set-gui-bus (num value)
      (declare (type bus-number num))
      (setf (bus (+ +gui-bus-offset+ num)) value))

    (defsetf gui-bus set-gui-bus)

    (gui-bus 0)             ; => 0.0d0
    (setf (gui-bus 0) 123)  ; => 123.0d0

The alternative is a foreign *GUI-BUS-POINTER*:

    (defvar *gui-bus-pointer*
      (cffi:inc-pointer incudine::*bus-pointer*
                        (* (cffi:foreign-type-size 'sample)
                           (- *number-of-bus-channels* 200))))
    (declaim (type cffi:foreign-pointer *gui-bus-pointer*))

    (declaim (inline gui-bus))
    (defun gui-bus (num)
      (declare (type bus-number num))
      (smp-ref *gui-bus-pointer* num))

    (declaim (inline set-gui-bus))
    (defun set-gui-bus (num value)
      (declare (type bus-number num))
      (setf (smp-ref *gui-bus-pointer* num) (sample value)))

    (defsetf gui-bus set-gui-bus)

    (gui-bus 0)             ; => 123.0d0
    (setf (gui-bus 0) 321)  ; => 321.0d0

(define-vug levelmeter (in freq (idx channel-number))
  (with ((sum 0)
         (count 0)
         (max (round-sample (/ *sample-rate* freq))))
    (declare (sample sum) (fixnum count max))
    (prog1 count
      (incf count)
      (incf sum (* in in))
      (when (>= count max)
        (nrt-funcall
          (lambda ()
            (cuda-gui:change-level
              (svref (cuda-gui:meters cuda-gui:*test*) idx)
              (round-sample
                (+ 100 (lin->db
                         (sqrt (the non-negative-sample (/ sum count)))))))))
        (setf sum +sample-zero+)
        (setf count 0)))))
|#

(define-vug levelmeter (in freq (idx channel-number))
  (with ((sum 0)
         (count 0)
         (max (round-sample (/ *sample-rate* freq)))
         (value 0))
    (declare (sample sum) (fixnum count max value))
    (prog1 count
      (incf count)
      (incf sum (* in in))
      (when (>= count max)
        (setf value
              (round-sample
                (+ 100 (lin->db
                         (sqrt (the non-negative-sample (/ sum count)))))))
        (nrt-funcall
          (lambda ()
            (cuda-gui:change-level
              (svref (cuda-gui:meters cuda-gui:*test*) idx)
              value)))
        (setf sum +sample-zero+)
        (setf count 0)))))


(dsp! stereometer (freq (chan channel-number))
  (:defaults 10 0)
  (levelmeter (audio-in chan) freq chan)
  (levelmeter (audio-in (1+ chan)) freq (1+ chan)))

(defun meters (&key (num 2) (window-title "Meters") (freq 5))
  (let ((node-id (next-node-id)))
    (cuda-gui:meter-gui :num num :node-id node-id :window-title window-title)
    (stereometer freq 0 :id node-id)))
