;;;; stethoscope-incudine.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)

(deftype uint () '(integer 0 #.most-positive-fixnum))

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

(define-vug scope-vug ((numchans channel-number)
                       (in channel-number)
                       (gui incudine-gui::stethoscope)
                       (size uint)
                       (maxsize uint))
  (with ((sample-idx 0)
         (curr 0)
         (bufs-a (make-array numchans :element-type 'buffer :initial-element (make-buffer 1)))
         (bufs-b (make-array numchans :element-type 'buffer :initial-element (make-buffer 1)))
         (curr-bufs (make-array numchans :element-type 'buffer :initial-element (make-buffer 1))))
    (declare (fixnum curr) (uint sample-idx))
    (initialize
     (dotimes (idx numchans)
       (setf (aref bufs-a idx) (make-local-buffer (the uint (1+ maxsize))))
       (setf (aref bufs-b idx) (make-local-buffer (the uint (1+ maxsize))))) 
     (setf (incudine-gui::bufs gui) bufs-b)
     (setf curr-bufs bufs-a)
     (nrt-funcall (lambda () (format t "~&bufs-a: ~a~%bufs-b: ~a~%curr-bufs: ~a~%"
                                bufs-a bufs-b curr-bufs))))
    (progn
      (dotimes (idx numchans)
        (setf (buffer-value (aref curr-bufs idx) sample-idx) (audio-in (+ in idx))))
      (incf sample-idx)
      (if (>= sample-idx (min size maxsize))
          (progn
            (reduce-warnings
              (nrt-funcall (lambda () (cuda-gui::signal! gui (cuda-gui::repaint-view)))))
            (setf sample-idx 0)
            (setf curr (lognot curr))
            (if (zerop curr)
                (progn
                  (setf (incudine-gui::bufs gui) bufs-b)
                  (setf curr-bufs bufs-a))
                (progn
                  (setf (incudine-gui::bufs gui) bufs-a)
                  (setf curr-bufs bufs-b)))
            )))))

;;;          (nrt-funcall (lambda () (cuda-gui::gui-funcall (q+:repaint gui))))

;;; (dotimes (idx 3) (print idx) (print idx))

(define-vug sine (freq amp phase)
  (sin (+ (* +twopi+ (phasor freq 0)) phase))
  (* amp ))

(dsp! scope-dsp ((numchans channel-number) (bus-num channel-number) (gui incudine-gui::stethoscope)
                 (bufsize uint) (bufmaxsize uint))
  (:defaults 2 0 nil 1024 8192)
  (scope-vug numchans bus-num gui bufsize bufmaxsize))
