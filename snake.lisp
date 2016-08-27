(ql:quickload "ltk")

(defpackage :snake
  (:use :cl :ltk)
  (:export snake))

(in-package :snake)

; How often to move snake - 200ms
(defvar *tick* 200)
; Size of playing area
(defvar *x-size* 50)
(defvar *y-size* 50)
; Graphical scale
(defvar *scale* 10)

(defstruct (coords (:conc-name co-))
  (x 0 :type integer)
  (y 0 :type integer))

; A shape on the screen, co-ords and id on the canvas
(defstruct (shape (:conc-name sh-))
  (x 0 :type integer)
  (y 0 :type integer)
  id)

(defparameter *snake* nil)
(defparameter *food* nil)
(defparameter *direction* :none)

(defun scale-d (d)
  (* d *scale*))

(defun scaled-box (x y)
  (map 'list #'scale-d (list x y (1+ x) (1+ y))))

(defun rnd-coords()
  (make-coords :x (random *x-size*) :y (random *y-size*)))

(defun create-shape (canvas create-fn x y)
  (make-shape :x x :y y :id (apply create-fn (cons canvas (scaled-box x y)))))

(defun rnd-shape (canvas create-fn)
  (let* ((pos (rnd-coords)))
    (create-shape canvas create-fn (co-x pos) (co-y pos))))

(defun move-shape (canvas shape x y)
  (set-coords canvas (sh-id shape) (scaled-box x y))
  (setf (sh-x shape) x)
  (setf (sh-y shape) y))

(defun move-snake (canvas s x y)
  (let* ((head (first s))
         (tail (rest s)))
    (unless (null head)
      (let* ((old-x (sh-x head))
             (old-y (sh-y head)))
        (move-shape canvas head x y)
        (move-snake canvas tail old-x old-y)))))

(defun snake-contains (snake x y)
  (let* ((head (first snake))
         (tail (rest snake)))
    (if (null head)
        nil
        (if (and (= (sh-x head) x)
                 (= (sh-y head) y))
            t
            (snake-contains tail x y)))))

(defun move-food (canvas)
  (let* ((pos (rnd-coords)))
    (move-shape canvas *food* (co-x pos) (co-y pos))))

(defun tick (canvas)
  (let* ((head (first *snake*))
         (old-x (sh-x head))
         (old-y (sh-y head))
         (new-x (cond
                  ((eq *direction* :right) (1+ old-x))
                  ((eq *direction* :left) (1- old-x))
                  (t old-x)))
         (new-y (cond
                  ((eq *direction* :down) (1+ old-y))
                  ((eq *direction* :up) (1- old-y))
                  (t old-y))))

    ; Fell off the edge of the world
    (if (and (or (not (= new-x old-x))
                 (not (= new-y old-y)))
             (or (= new-x -1)
                 (= new-x 50)
                 (= new-y -1)
                 (= new-y 50)
                 (snake-contains *snake* new-x new-y)))
        (print "BOOOM"))

    ; nom-nom-nom
    (if (and (= new-x (sh-x *food*))
             (= new-y (sh-y *food*)))
        (progn ; Grow snake and move food
          (setq *snake* (cons (create-shape canvas #'create-rectangle new-x new-y) *snake*) )
          (move-food canvas)
          )
        ; otherwise move the snake
        (move-snake canvas *snake* new-x new-y)
        )

    (after *tick* (lambda() (tick canvas)))))

(defun snake ()
  (with-ltk ()
    (let* ((canvas (make-instance 'canvas :width (scale-d *x-size*) :height (scale-d *y-size*))))
      (pack canvas)
      (setq *snake* (list (rnd-shape canvas #'make-rectangle)))
      (setq *food* (rnd-shape canvas #'create-oval))
      (bind *tk* "<Key>"
            (lambda (evt)
              (let* ((k (symbol-name (event-char evt))))
                ; This sucks - need a better way
                (setq *direction* (cond
                                    ((equal k "UP") :up)
                                    ((equal k "DOWN") :down)
                                    ((equal k "LEFT") :left)
                                    ((equal k "RIGHT") :right)
                                    (t *direction*))))))
      (after *tick* (lambda () (tick canvas))))))
