1;;;; stethoscope-incudine.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)

(deftype uint () '(integer 0 #.most-positive-fixnum))

(define-vug audio-out-scope-vug ((numchans channel-number)
                       (in channel-number)
                       (gui incudine-gui::stethoscope)
                       (size uint)
                       (maxsize uint))
  (with ((sample-idx 0)
         (tick-idx 0)
         (screen-mintick (sample->fixnum (/ *sample-rate* cuda-gui::*screen-refresh-freq*)) )
         (sample-max-idx (min size maxsize))
         (curr-p t)
         (bufs-a (cuda-gui::bufs-a gui))
         (bufs-b (cuda-gui::bufs-b gui))
         (curr-bufs (make-array numchans)))
    (declare (uint sample-idx sample-max-idx tick-idx screen-mintick) (boolean curr-p))
    (initialize
     (setf (incudine-gui::curr-bufs gui) bufs-b)
     (setf curr-bufs bufs-a))
    (dochannels (idx numchans)
      (setf (buffer-value (svref curr-bufs idx) sample-idx)
            (audio-out (+ in idx))))
    (incf sample-idx)
    (incf tick-idx)
    (when (>= sample-idx sample-max-idx)
      (setf sample-idx 0)
      (when (> tick-idx screen-mintick)
        (setf tick-idx 0)
        ;; (reduce-warnings
        ;;   (nrt-funcall
        ;;    (lambda () (cuda-gui::emit-signal gui "repaintViewEvent()"))))
        (setf curr-p (not curr-p))
        (cond (curr-p ;;; swap display and audio buffer
               (setf (incudine-gui::curr-bufs gui) bufs-b)
               (setf curr-bufs bufs-a))
              (t
               (setf (incudine-gui::curr-bufs gui) bufs-a)
               (setf curr-bufs bufs-b)))))))

(define-vug audio-in-scope-vug ((numchans channel-number)
                       (in channel-number)
                       (gui incudine-gui::stethoscope)
                       (size uint)
                       (maxsize uint))
  (with ((sample-idx 0)
         (sample-max-idx (min size maxsize))
         (curr-p t)
         (bufs-a (cuda-gui::bufs-a gui))
         (bufs-b (cuda-gui::bufs-b gui))
         (curr-bufs (make-array numchans)))
    (declare (uint sample-idx sample-max-idx) (boolean curr-p))
    (initialize
     (setf (incudine-gui::curr-bufs gui) bufs-b)
     (setf curr-bufs bufs-a))
    (dochannels (idx numchans)
      (setf (buffer-value (svref curr-bufs idx) sample-idx)
            (audio-in (+ in idx))))
    (incf sample-idx)
    (when (>= sample-idx sample-max-idx)
      (reduce-warnings
        (nrt-funcall
          (lambda () (cuda-gui::emit-signal gui "repaintViewEvent()")))
        )
      (setf sample-idx 0)
      (setf curr-p (not curr-p))
      (cond (curr-p ;;; swap display and audio buffer
             (setf (incudine-gui::curr-bufs gui) bufs-b)
             (setf curr-bufs bufs-a))
            (t
             (setf (incudine-gui::curr-bufs gui) bufs-a)
             (setf curr-bufs bufs-b))))))

(define-vug bus-scope-vug ((numchans channel-number)
                       (in channel-number)
                       (gui incudine-gui::stethoscope)
                       (size uint)
                       (maxsize uint))
  (with ((sample-idx 0)
         (sample-max-idx (min size maxsize))
         (curr-p t)
         (bufs-a (cuda-gui::bufs-a gui))
         (bufs-b (cuda-gui::bufs-b gui))
         (curr-bufs (make-array numchans)))
    (declare (uint sample-idx sample-max-idx) (boolean curr-p))
    (initialize
     (setf (incudine-gui::curr-bufs gui) bufs-b)
     (setf curr-bufs bufs-a))
    (dochannels (idx numchans)
      (setf (buffer-value (svref curr-bufs idx) sample-idx)
            (input-bus (+ in idx))))
    (incf sample-idx)
    (when (>= sample-idx sample-max-idx)
      (reduce-warnings
        (nrt-funcall
          (lambda () (cuda-gui::emit-signal gui "repaintViewEvent()"))))
      (setf sample-idx 0)
      (setf curr-p (not curr-p))
      (cond (curr-p ;;; swap display and audio buffer
             (setf (incudine-gui::curr-bufs gui) bufs-b)
             (setf curr-bufs bufs-a))
            (t
             (setf (incudine-gui::curr-bufs gui) bufs-a)
             (setf curr-bufs bufs-b))))))

;;;          (nrt-funcall (lambda () (cuda-gui::gui-funcall (q+:repaint gui))))

;;; (dotimes (idx 3) (print idx) (print idx))

(dsp! audio-in-scope-dsp ((numchans channel-number) (bus-num channel-number) (gui incudine-gui::stethoscope)
                 (bufsize uint) (bufmaxsize uint) (process? boolean))
  (:defaults 2 8 nil 1024 8192 t)
  (if process?
      (foreach-frame (audio-in-scope-vug numchans bus-num gui bufsize bufmaxsize))))

(dsp! audio-out-scope-dsp ((numchans channel-number) (bus-num channel-number) (gui incudine-gui::stethoscope)
                 (bufsize uint) (bufmaxsize uint) (process? boolean))
  (:defaults 2 8 nil 1024 8192 t)
  (if process?
      (foreach-frame (audio-out-scope-vug numchans bus-num gui bufsize bufmaxsize))))

(dsp! bus-scope-dsp ((numchans channel-number) (bus-num channel-number) (gui incudine-gui::stethoscope)
                 (bufsize uint) (bufmaxsize uint) (process? boolean))
  (:defaults 2 8 nil 1024 8192 t)
  (if process?
      (foreach-frame (bus-scope-vug numchans bus-num gui bufsize bufmaxsize))))
#x02c6
