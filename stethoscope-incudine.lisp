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

(define-vug scope-vug ((numchans channel-number)
                       (in channel-number)
                       (gui incudine-gui::stethoscope)
                       (size uint)
                       (maxsize uint))
  (with ((sample-idx 0)
         (sample-max-idx (min size maxsize))
         (curr-p t)
         (bufs-a (make-array numchans))
         (bufs-b (make-array numchans))
         (curr-bufs (make-array numchans)))
    (declare (uint sample-idx sample-max-idx) (boolean curr-p))
    (initialize
      (dochannels (idx numchans)
        (setf (svref bufs-a idx) (make-local-buffer (the uint (1+ maxsize))))
        (setf (svref bufs-b idx) (make-local-buffer (the uint (1+ maxsize)))))
      (setf (incudine-gui::bufs gui) bufs-b)
      (setf curr-bufs bufs-a))
    (dochannels (idx numchans)
      (setf (buffer-value (svref curr-bufs idx) sample-idx)
            (audio-in (+ in idx)))
;;;   (nrt-funcall
;;;       (lambda () (format t "current-channel: ~a" idx))
      )
    (incf sample-idx)
    (when (>= sample-idx sample-max-idx)
      (reduce-warnings
        (nrt-funcall
          (lambda () (cuda-gui::signal! gui (cuda-gui::repaint-view)))))
      (setf sample-idx 0)
      (setf curr-p (not curr-p))
      (cond (curr-p ;;; swap display and audio buffer
             (setf (incudine-gui::bufs gui) bufs-b)
             (setf curr-bufs bufs-a))
            (t
             (setf (incudine-gui::bufs gui) bufs-a)
             (setf curr-bufs bufs-b))))))

;;;          (nrt-funcall (lambda () (cuda-gui::gui-funcall (q+:repaint gui))))

;;; (dotimes (idx 3) (print idx) (print idx))

(dsp! scope-dsp ((numchans channel-number) (bus-num channel-number) (gui incudine-gui::stethoscope)
                 (bufsize uint) (bufmaxsize uint))
  (:defaults 2 8 nil 1024 8192)
  (foreach-frame (scope-vug numchans bus-num gui bufsize bufmaxsize)))
