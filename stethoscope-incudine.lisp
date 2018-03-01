;;;; stethoscope-incudine.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)

(define-vug env-levelmeter (in (freq fixnum) (meter incudine-gui::levelmeter)
                            (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-local-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (bufidx (make-array periods :element-type 'channel-number))
         (max +sample-zero+)
         (value 0))
    (declare  (fixnum size value) (sample max))
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
          (nrt-funcall
            (lambda ()
              (cuda-gui:change-level meter value)))
          (setf (smp-ref sums i) +sample-zero+)
          (setf (aref bufidx i) 0))))))

(define-vug scope-vug (in (size fixnum)
                          (chan-idx channel-number)
                          (gui-array (simple-array buffer *))
                          (maxsize fixnum))
  (with ((buf-a (make-local-buffer (1+ maxsize)))
         (buf-b (make-local-buffer (1+ maxsize)))
         (idx 0)
         (curr 0))
    (declare (fixnum idx curr))
    (initialize
     (setf (aref gui-array chan-idx) buf-b))
    (if (zerop curr)
        (setf (buffer-value buf-b idx) in)
        (setf (buffer-value buf-a idx) in))
    (incf idx)
    (if (>= idx (min size maxsize))
        (progn
          (setf idx 0)
          (setf curr (lognot curr))
          (if (zerop curr)
              (setf (aref gui-array chan-idx) buf-b)
              (setf (aref gui-array chan-idx) buf-a))))))

(define-vug scope-vug (in (size fixnum)
                          (chan-idx channel-number)
                          (gui-array (simple-array buffer *))
                          (maxsize fixnum))
  (with ((buf-a (make-local-buffer (1+ maxsize)))
         (buf-b (make-local-buffer (1+ maxsize)))
         (idx 0)
         (curr 0))
    (declare (fixnum idx curr))
    (initialize
     (setf (aref gui-array chan-idx) buf-b))
    (if (zerop curr)
        (setf (buffer-value buf-b idx) in)
        (setf (buffer-value buf-a idx) in))
    (incf idx)
    (if (>= idx (min size maxsize))
        (progn
          (setf idx 0)
          (setf curr (lognot curr))
          (if (zerop curr)
              (setf (aref gui-array chan-idx) buf-b)
              (setf (aref gui-array chan-idx) buf-a))))))

(define-vug scope-vug ((in channel-number)
                       (size fixnum)
                       (numchans channel-number)
                       (gui incudine-gui::stethoscope)
                       (maxsize fixnum))
  (with ((sample-idx 0)
         (curr 0)
         (bufs-a (make-array numchans :element-type 'buffer :initial-element (make-buffer 1)))
         (bufs-b (make-array numchans :element-type 'buffer :initial-element (make-buffer 1)))
         (curr-buf (make-array numchans :element-type 'buffer :initial-element (make-buffer 1))))
    (declare (fixnum sample-idx curr))
    (initialize
     (dotimes (idx numchans)
       (setf (aref bufs-a idx) (make-local-buffer (1+ maxsize)))
       (setf (aref bufs-b idx) (make-local-buffer (1+ maxsize)))) 
     (setf (incudine-gui::bufs gui) bufs-b)
     (setf curr-buf bufs-a))
    (dotimes (idx numchans)
      (setf (buffer-value (aref curr-buf idx) sample-idx) (audio-in (+ in idx))))
    (incf sample-idx)
    (if (>= sample-idx (min size maxsize))
        (progn
          (setf sample-idx 0)
          (setf curr (lognot curr))
          (if (zerop curr)
              (progn
                (setf (incudine-gui::bufs gui) bufs-b)
                (setf curr-buf bufs-a))
              (progn
                (setf (incudine-gui::bufs gui) bufs-a)
                (setf curr-buf bufs-b)))))))

(dsp! scope-dsp ((numchans channel-number) (bus-num channel-number) (gui incudine-gui::stethoscope))
  (scope-vug bus-num 1024 numchans gui 8192))
