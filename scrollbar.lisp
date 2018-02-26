;;;; scrollbar.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine-gui)
(in-readtable :qtools)

(deftype orientation () '(member :horizontal :vertical))

(define-widget scrollbar (QScrollBar)
  ((orientation :initform :horizontal
                :type orientation :initarg :orientation :accessor orientation)))

(define-initializer (scrollbar setup)
  (case orientation
    (:vertical
     (progn
       (q+:set-orientation scrollbar (#_Vertical "Qt"))
       (q+:set-fixed-width scrollbar 15)))
    (t
     (progn
       (q+:set-orientation scrollbar (#_Horizontal "Qt"))
       (q+:set-fixed-height scrollbar 15))))
  (q+:set-style-sheet scrollbar
                      "border: 1px solid #333333; cursor-color: white; border-radius: 5px; selection-background-color: white")
  (q+:set-minimum scrollbar 0)
  (q+:set-maximum scrollbar 10000))

(define-override (scrollbar paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter scrollbar)))
    (let* ((width (q+:width scrollbar))
           (height (q+:height scrollbar))
           (max (q+:maximum scrollbar))
           (min (q+:minimum scrollbar))
           (prop (float (/ (- (q+:value scrollbar) min)
                           (- max min)))))
      (q+:erase-rect painter (q+:rect scrollbar))
      (let ((bg-path (q+:make-QPainterPath)))
        (q+:add-Rounded-Rect bg-path (q+:make-qrectf (q+:rect scrollbar)) 5 5)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51 255))
        (q+:fill-path painter bg-path (q+:make-qbrush (q+:make-qcolor 128 128 128 255)))
        (q+:draw-path painter bg-path))
      (let ((thumb-path (q+:make-QPainterPath)))
        (case (orientation scrollbar)
          (:vertical
           (let ((val-pos (+ 2 (* prop (- height 4)))))
             (q+:add-Rounded-Rect thumb-path (q+:make-qrectf
                                              (q+:make-qrect
                                               1 (round
                                                  (cond
                                                    ((< val-pos 9) 1)
                                                    ((> val-pos (- height 9)) (- height 19))
                                                    (t (+ val-pos -9))))
                                               13 18))
                                  2 2)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51))
             (q+:fill-path painter thumb-path (q+:make-qbrush (q+:make-qcolor 255 255 255 255)))
             (q+:draw-path painter thumb-path)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 0 0 0 255))
             (q+:set-width (q+:pen painter) 2)
             (q+:draw-line painter 3 (round val-pos) 13 (round val-pos))))
          (t
           (let ((val-pos (+ 2 (* prop (- width 4)))))
             (q+:add-Rounded-Rect thumb-path (q+:make-qrectf
                                              (q+:make-qrect
                                               (round
                                                  (cond
                                                    ((< val-pos 9) 1)
                                                    ((> val-pos (- width 9)) (- width 19))
                                                    (t (+ val-pos -9))))
                                               1
                                               18 13))
                                  2 2)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51))
             (q+:fill-path painter thumb-path (q+:make-qbrush (q+:make-qcolor 255 255 255 255)))
             (q+:draw-path painter thumb-path)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 0 0 0 255))
             (q+:set-width (q+:pen painter) 2)
             (q+:draw-line painter (round val-pos) 3 (round val-pos) 13))))))))
#|
(define-override (scrollbar paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter scrollbar)))
    (let ((width (q+:width scrollbar))
          (height (q+:height scrollbar)))
      (q+:erase-rect painter (q+:rect scrollbar))
      (let ((bg-path (q+:make-QPainterPath)))
        (q+:add-Rounded-Rect bg-path (q+:make-qrectf (q+:rect scrollbar)) 5 5)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51 255))
        (q+:fill-path painter bg-path (q+:make-qbrush (q+:make-qcolor 128 128 128 255)))
        (q+:draw-path painter bg-path))
      (let ((thumb-path (q+:make-QPainterPath))
            (value (q+:value scrollbar)))
        (format t "~&~a" value)
        (q+:add-Rounded-Rect thumb-path (q+:make-qrectf
                                         (case (orientation scrollbar)
                                           (:vertical (q+:make-qrect 1 (round (* 0.01 (q+:value scrollbar) height)) 13 18))
                                           (t (q+:make-qrect (round (* 0.01 (q+:value scrollbar) width)) 1 18 13))))
                             2 2)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51))
        (q+:fill-path painter thumb-path (q+:make-qbrush (q+:make-qcolor 200 200 200 255)))
        (q+:draw-path painter thumb-path))
;;      (q+:set-color (q+:pen painter) (q+:make-qcolor 0 0 0 255))
;;      (q+:draw-line painter 0 0 width height)
      )))

(define-override (scrollbar paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter scrollbar)))
    (let* ((width (q+:width scrollbar))
           (height (q+:height scrollbar))
           (max (q+:maximum scrollbar))
           (min (q+:minimum scrollbar))
           (prop (/ (- (q+:value scrollbar ) min)
                    (- max min))))
      (q+:erase-rect painter (q+:rect scrollbar))
      (let ((bg-path (q+:make-QPainterPath)))
        (q+:add-Rounded-Rect bg-path (q+:make-qrectf (q+:rect scrollbar)) 5 5)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51 255))
        (q+:fill-path painter bg-path (q+:make-qbrush (q+:make-qcolor 128 128 128 255)))
        (q+:draw-path painter bg-path))
      (let ((thumb-path (q+:make-QPainterPath)))
        ;;        (format t "~&~a" value)
        (case (orientation scrollbar)
          (:vertical
           (let ((val-pos (max 1 (min (1- height) (* prop height)))))
             (q+:move-to thumb-path 1 (round val-pos))
             (q+:line-to thumb-path 13 (round (+ val-pos (* prop -18))))
             (q+:line-to thumb-path 13 (round (+ val-pos (* (- 1 prop) 18))))
             (q+:close-subpath thumb-path)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51))
             (q+:fill-path painter thumb-path (q+:make-qbrush (q+:make-qcolor 200 200 200 255)))
             (q+:draw-path painter thumb-path)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 0 0 0 255))
             (q+:set-width (q+:pen painter) 2)
             (q+:draw-line painter 2 (round val-pos) 12 (round val-pos))))
          (t
           (let ((val-pos (max 1 (min (1- width) (* prop width)))))
             (q+:move-to thumb-path (round val-pos) 1)
             (q+:line-to thumb-path (round (+ val-pos (* prop -18))) 13)
             (q+:line-to thumb-path (round (+ val-pos (* (- 1 prop) 18))) 13)
             (q+:close-subpath thumb-path)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51))
             (q+:fill-path painter thumb-path (q+:make-qbrush (q+:make-qcolor 200 200 200 255)))
             (q+:draw-path painter thumb-path)
             (q+:set-color (q+:pen painter) (q+:make-qcolor 0 0 0 255))
             (q+:set-width (q+:pen painter) 2)
             (q+:draw-line painter (round val-pos) 2 (round val-pos) 12))))))))

|#

(define-override (scrollbar mouse-press-event) (ev)
  (case (orientation scrollbar)
    (:vertical (q+:set-value
                scrollbar
                (round (+ (q+:minimum scrollbar)
                          (* (- (q+:maximum scrollbar)
                                (q+:minimum scrollbar))
                             (/ (q+:y ev) (q+:height scrollbar)))))))
    (t (q+:set-value
                scrollbar
                (round (+ (q+:minimum scrollbar)
                          (* (- (q+:maximum scrollbar)
                                (q+:minimum scrollbar))
                             (/ (q+:x ev) (q+:width scrollbar)))))))))

(define-override (scrollbar mouse-move-event) (ev)
  (case (orientation scrollbar)
    (:vertical (q+:set-value
                scrollbar
                (round (+ (q+:minimum scrollbar)
                          (* (- (q+:maximum scrollbar)
                                (q+:minimum scrollbar))
                             (/ (q+:y ev) (q+:height scrollbar)))))))
    (t (q+:set-value
                scrollbar
                (round (+ (q+:minimum scrollbar)
                          (* (- (q+:maximum scrollbar)
                                (q+:minimum scrollbar))
                             (/ (q+:x ev) (q+:width scrollbar)))))))))





#|
(define-override (numbox mouse-release-event) (ev)
  (setf mouse-pressed nil)
  ;;(q+:set-cursor numbox (#_DragLinkCursor "Qt"))
  )

(let* ((painter (q+:make-qpainter))
       (path (q+:make-QPainterPath)))
        (q+:add-Rounded-Rect path (q+:make-qrectf (q+:make-qrect 0 0 10 10)) 10 10)
        (q+:set-color (q+:pen painter) (q+:make-qcolor 51 51 51 255))
        (q+:fill-path painter path (q+:make-qbrush (q+:make-qcolor 10 10 10 255)))
        (q+:draw-path painter path))

(format t "~x" #x33)

(q+:set-Render-Hint)(QPainter::Antialiasing)             ;
                     
 ;
QPen pen(Qt::black, 10);
p.setPen(pen);
p.fillPath(path, Qt::red);
p.drawPath(path);



(defparameter *scroll-bar-style* "border: 1px solid #333333; 
background-color: #444444;
cursor-color: white;
border-radius: 5px;
selection-background-color: white")


QScrollBar::add-line:vertical {
      margin: 0 0 0 15;
      border: none;
      background: none;
}
|#