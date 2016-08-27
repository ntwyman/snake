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
(defun scaled-box (x y)
  (map 'list #'scale-d (list x y (1+ x) (1+ y))))

(defun move-snake (canvas s x y)
  (let* ((head (first s))
         (tail (rest s)))
    (unless (null head)
      (let* ((old-x (sg-x head))
             (old-y (sg-y head)))
        (set-coords canvas
                    (sg-box head)
                    (scaled-box x y))
        (setf (sg-x head) x)
        (setf (sg-y head) y)
        (move-snake canvas tail old-x old-y))
      )))

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
    ; Fell off the edge of the world
    (if (or (= new-x -1)
            (= new-x 50)
            (= new-y -1)
            (= new-y 50))
        (print "BOOOM"))

    (move-snake canvas *snake* new-x new-y)
    (after *tick* (lambda() (tick canvas)))
    ))

(defun make-snake (canvas)
  (let* ((x (random 50))
         (y (random 50))
         (food-x (random 50))
         (food-y (random 50))
         (box (apply #'create-rectangle (cons canvas (scaled-box x y))))
         (food (apply #'create-oval (cons canvas (scaled-box food-x food-y)))))
    (setq *snake* (list (make-segment :x x :y y :box box)))
    (setq *food* (make-segment :x food-x :y food-y :box food))
    ))

(defun snake ()
  (with-ltk ()
    (let* ((canvas (make-instance 'canvas :width 500 :height 500)))
      (pack canvas)
      (make-snake canvas)
      (bind *tk* "<Key>"
            (lambda (evt)
              (let* ((k (symbol-name (event-char evt))))
                (format t "Type ~S, K - ~S~%"
                        (type-of k) k)
                ; This sucks - need a better way
                (setq *direction* (cond
                                    ((equal k "UP") :up)
                                    ((equal k "DOWN") :down)
                                    ((equal k "LEFT") :left)
                                    ((equal k "RIGHT") :right)
                                    (t *direction*)
                                    )))))
      (after *tick* (lambda () (tick canvas)))
      )))
