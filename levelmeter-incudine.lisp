;;;; levelmeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)
(export 'meters :scratch)

(declaim (inline round-sample))

(defun round-sample (x)
  (declare (type (sample
                  #.(coerce (ash most-negative-fixnum -1) 'sample)
                  #.(coerce (ash most-positive-fixnum -1) 'sample))
                 x))
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

#|
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

(dotimes (i hop-size)
  (incf (aref sums i) (* in in (buffer-value hanning (aref bufidx i))))
  (if (> (incf (aref bufidx i)) size)
      (prog1
          (output-value (sqrt (aref sums i)))
        (setf (aref bufidx i) 0)
        (setf (aref sums i) +sample-zero+))))
|#

(defun hanning ()
  (lambda (c-array size)
    (declare (type foreign-pointer c-array) (type non-negative-fixnum size))
    (with-foreign-array (tmp 'sample)
      (with-samples (value abs-value (max 0.0))
        (dotimes (i size)
          (setf (smp-ref c-array i)
                (* 0.5 (- 1 (cos (* 2 pi (/ i (1- size))))))))
        (values c-array nil nil)))))

(defun hanning-rms ()
  "(/ size) weighted version of (* 2 hanning)"
  (lambda (c-array size)
    (declare (type foreign-pointer c-array) (type non-negative-fixnum size))
    (with-foreign-array (tmp 'sample)
      (with-samples (value abs-value (max 0.0))
        (dotimes (i size)
          (setf (smp-ref c-array i)
                (/ (- 1 (cos (* 2 pi (/ i (1- size))))) size)))
        (values c-array nil nil)))))

(define-vug levelmeter (in freq (meter incudine-gui::levelmeter))
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
            (cuda-gui:change-level meter value)))
        (setf sum +sample-zero+)
        (setf count 0)))))

(define-vug env-levelmeter (in (freq fixnum) (meter incudine-gui::levelmeter)
                            (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-local-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (bufidx (make-array periods :element-type 'channel-number))
         (max +sample-zero+)
         (value 0)
         (last-value #.most-positive-fixnum))
    (declare  (fixnum size value last-value) (sample max))
    (initialize
      (dotimes (i periods)
        (setf (aref bufidx i)
              (reduce-warnings (floor (* i (round (/ size periods))))))))
    (dotimes (i periods)
      (incf (smp-ref sums i) (* in in (buffer-value hanning (aref bufidx i))))
      (when (>= (incf (aref bufidx i)) size)
        (progn
          (setf value
                (round-sample
                  (+ 100 (lin->db
                          (sqrt (the non-negative-sample (smp-ref sums i)))))))
          (if (/= last-value value)
              (progn
                (nrt-funcall
                 (lambda ()
                   (cuda-gui:change-level meter value)))
                (setf last-value value)))
          (setf (smp-ref sums i) +sample-zero+)
          (setf (aref bufidx i) 0))))))

(dsp! env-monometer ((freq fixnum) (gui incudine-gui::levelmeter) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
   (foreach-frame (env-levelmeter (bus chan) freq gui hop-size)))
 
(defun meters (&key (group 300) (num *number-of-input-bus-channels*) (id "Meters") (freq 5) (hop-size 2) (audio-bus 0))
  (let* ((gui (cuda-gui:meter-gui :num num :dsp-node-ids '() :id id)))
    (dotimes (idx num)
      (env-monometer freq (svref (incudine-gui::meters gui) idx)
                     (+ audio-bus idx) hop-size
                     :action (lambda (n)
                               (push (node-id n)
                                     (cuda-gui::dsp-node-ids gui)))
                     :tail group))))

#|

(meters :id "meters03")
(dsp! monometer (freq (gui incudine-gui::levelmeter) (chan channel-number) (gui-idx channel-number))
   (:defaults 10 nil 0 0)
   (levelmeter (audio-in chan) freq gui))

(defun meters (&key (num 2) (id "Meters") (freq 10) (audio-bus 0))
  (let* ((gui (cuda-gui:meter-gui :num num :dsp-node-ids '() :id id)))
    (loop
       for idx below num
       do (progn
            (monometer freq (svref (incudine-gui::meters gui) idx) (+ audio-bus idx) idx)
            :action (lambda (n)
                      (push (node-id n)
                            (cuda-gui:node-ids gui)))))))

(env-meters :num 8 :id "meters01" :freq 10 :hop-size 1)
(env-meters :num 8 :id "meters02" :freq 10 :hop-size 2)
(env-meters :num 16 :id "meters03" :freq 10 :hop-size 4)
(env-meters :num 16 :id "meters04" :freq 20 :hop-size 4)

|#

