;;;; levlemeter-gui.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)

(defun rmstodb (rms)
  (* 20 (log rms 10)))

(define-vug levelmeter (in freq idx)
  (with-samples ((count 0)
                 (sum 0)
                 (max (coerce (round (/ *sample-rate* freq)) 'sample) ))
    (prog1 count
      (incf count)
      (incf sum (* in in))
      (cond ((>= count max)
             (progn
               (cuda-gui:change-level
                (aref (cuda-gui:meters cuda-gui:*test*) (round idx))
                (round (+ 100 (rmstodb (sqrt (/ sum count))))))
               (setf sum 0.0d0)
               (setf count 0.0d0)))))))


(dsp! stereometer (freq chan)
  (levelmeter (audio-in (round chan)) freq (round chan))
  (levelmeter (audio-in (round (1+ chan))) freq (round (1+ chan))))

(dsp! stereometer (freq chan)
  (levelmeter (audio-in (round chan)) freq 0)
  (levelmeter (audio-in (round (1+ chan))) freq 1))

(defun meteor (&key (num 2))
  (let ((node-id (next-node-id)))
    (cuda-gui:meter-gui :num num :node-id node-id)
    (stereometer 5 0 :id node-id)))
