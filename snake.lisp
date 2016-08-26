(ql:quickload "ltk")
(defpackage :snake
  (:use :cl :ltk)
  (:export snake)
  )

(in-package :snake)

(defvar *tick* 200)
(defvar *scale* 10)

(defstruct (segment (:conc-name sg-))
  (x 0 :type integer)
  (y 0 :type integer)
  box)

(defparameter *food* ())
(defparameter *snake* ())
(defparameter *direction* :none)

(defun scale-d (d) (* d *scale*))

(defun move-snake (canvas s x y)
  (let* ((head (first s))
         (tail (rest s)))
    (if (null head)
        ()
        (progn (set-coords canvas
                           (sg-box head)
                           (list (scale-d x)
                                 (scale-d y)
                                 (scale-d (1+ x))
                                 (scale-d (1+ y))))
               (cons (make-segment :x x :y y :box (sg-box head))
                     (move-snake canvas tail (sg-x head) (sg-y head))))
        )
    ))

(defun tick (canvas)
  (let* ((head (car *snake*))
         (old-x (sg-x head))
         (old-y (sg-y head))
         (new-x (cond
                  ((eq *direction* :right) (1+ old-x))
                  ((eq *direction* :left) (1- old-x))
                  (t old-x)))
         (new-y (cond
                  ((eq *direction* :down) (1+ old-y))
                  ((eq *direction* :up) (1- old-y))
                  (t old-y))))
    (if (or (= new-x -1)
            (= new-x 50)
            (= new-y -1)
            (= new-y 50))
        (print "BOOOM"))
    (setq *snake* (move-snake canvas *snake* new-x new-y))
    (after *tick* (lambda() (tick canvas)))
    ))

(defun make-snake (canvas)
  (let* ((x (random 50))
         (y (random 50))
         (box (create-rectangle canvas
                                (scale-d x)
                                (scale-d y)
                                (scale-d (1+ x))
                                (scale-d (1+ y)))))
    (setq *snake* (list (make-segment :x x :y y :box box)))
    ))

(defun snake ()
  (with-ltk ()
    (let* ((canvas (make-instance 'canvas :width 500 :height 500)))
      (pack canvas)
      (make-snake canvas)
      (bind *tk* "<Key>"
            (lambda (evt)
              (let* ((k (symbol-name (event-char evt))))
                ; this sucks - needs a better way
                (setq *direction* (cond
                                    ((equal k "UP") :up)
                                    ((equal k "DOWN") :down)
                                    ((equal k "LEFT") :left)
                                    ((equal k "RIGHT") :right)
                                    (t *direction*)
                                    )))))
      (after *tick* (lambda () (tick canvas)))
      )))
