;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-rect1
         world-rect2
         world-paused?
         new-rectangle
         rect-x
         rect-y
         rect-vx
         rect-vy)


;Constants definition
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define RECTANGLE-WIDTH 60)
(define RECTANGLE-HEIGHT 50)

(define RIGHT-MOST-POS 370)
(define LEFT-MOST-POS 30)
(define UPPER-MOST-POS 25)
(define LOWER-MOST-POS 275)

(define DRAW-RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue"))
(define DRAW-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define PAUSE " ")

;Data definition
;world: (make-world rectangle rectangle boolean)
;INTERPRETATION: (make rec1 rec2 ifpaused), in which
; rec1 is a rectangle object inside the world
; rec2 is another rectangle object inside the world
; ifpaused is a boolean value indicating if the world is paused

(define-struct world (rect1 rect2 paused?))

;Template
(define (world-fn w)
  (... (world-rect1 w) (world-rect2 w) (world-paused? w)))

;rect-info: (make-rect-info coordinate coordinate velocity-x-dir velocity-y-dir)
;INTERPRETATION: (make-rect-info x y vx vy select), in which
;x is a NonNegInteger representing the x coordinate of the rectangle
;y is a NonNegInteger representing the y coordinate of the rectangle
;vx is is an Integer representing the velocity on the x direction of the rectangle
;vy is is an Integer representing the velocity on the y direction of the rectangle

(define-struct rect-info (x y vx vy))

;Template
(define (rect-info-fn rect)
  (... (rect-info-x rect) (rect-info-y rect) (rect-info-vx rect) (rect-info-vy rect)))




;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
;; Example: (screensaver 0.5)
;; Strategy: Using template for big-bang, also using simpler functions
(define (screensaver n)
  (big-bang (initial-world 1)
            (on-tick world-after-tick n)
            (on-draw draw-furnished-world)
            (on-key world-after-key-event)))

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; Example: (initial-world 3) -> (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) false)
;; Strategy: using template method of rectangle struct
(define (initial-world rand)
  (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) #true))

(begin-for-test
  (check-equal? (initial-world 1) (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) true)))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; Example: (world-after-tick (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) false))
;;        -> (make-world (make-rect-info 188 120 -12 20) (make-rect-info 223 186 23 -14) false)
;;Strategy: dividing into cases; using template of world struct and rectangle struct; using simpler functions
(define (world-after-tick w)
  (cond
    [(world-paused? w) w]
    [else (make-world (rect-after-tick (world-rect1 w)) (rect-after-tick (world-rect2 w))
                      (world-paused? w))]))

(begin-for-test
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) true))
                (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) true)  "should return same world")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) false))
                (make-world (make-rect-info 188 120 -12 20) (make-rect-info 223 186 23 -14) false) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) true))
                (make-world (make-rect-info 200 100 -12 20) (make-rect-info 200 200 23 -14) true) "Wrong coordinate update"))

;rect-after-tick: rectangle -> rectangle
;GIVEN: a rectangle object
;RETURNS: a rectangle object that follows the given one after the tick
;Example: (rect-after-tick (make-rect-info 100 120 -10 15)) -> (make-rect-info 90 135 -10 15)
;Strategy: dividing into cases; using template of rectangle struct; combining simpler functions
(define (rect-after-tick r)
  (cond
    [(and (>= (rect-info-x r) RIGHT-MOST-POS) (>= (rect-info-y r) LOWER-MOST-POS)) (perfect-bounce r)]
    [(and (>= (rect-info-x r) RIGHT-MOST-POS) (<= (rect-info-y r) UPPER-MOST-POS)) (perfect-bounce r)]
    [(and (<= (rect-info-x r) LEFT-MOST-POS) (>= (rect-info-y r) LOWER-MOST-POS)) (perfect-bounce r)]
    [(and (<= (rect-info-x r) LEFT-MOST-POS) (<= (rect-info-y r) UPPER-MOST-POS)) (perfect-bounce r)]
    [(or (<= (rect-info-x r) LEFT-MOST-POS) (>= (rect-info-x r) RIGHT-MOST-POS)) (reverse-x r)]
    [(or (<= (rect-info-y r) UPPER-MOST-POS) (>= (rect-info-y r) LOWER-MOST-POS)) (reverse-y r)]
    [(check-hit-wall-both r) (move-less-both r)]
    [(check-hit-wall-x r) (move-less-x r)]
    [(check-hit-wall-y r) (move-less-y r)]
    [else (normal-move r)]))

(begin-for-test
  (check-equal? (rect-after-tick (make-rect-info 100 120 -10 15)) (make-rect-info 90 135 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info LEFT-MOST-POS UPPER-MOST-POS -10 -15)) (make-rect-info 40 40 10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info LEFT-MOST-POS LOWER-MOST-POS -10 15)) (make-rect-info 40 260 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info RIGHT-MOST-POS UPPER-MOST-POS 10 -15)) (make-rect-info 360 40 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS 10 15)) (make-rect-info 360 260 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info LEFT-MOST-POS 225 -10 -15)) (make-rect-info 40 210 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info RIGHT-MOST-POS 225 10 -15)) (make-rect-info 360 210 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 320 UPPER-MOST-POS -10 -15)) (make-rect-info 310 40 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 320 LOWER-MOST-POS -10 15)) (make-rect-info 310 260 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 31 26 -10 -15)) (make-rect-info LEFT-MOST-POS UPPER-MOST-POS -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 31 274 -10 15)) (make-rect-info LEFT-MOST-POS LOWER-MOST-POS -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 369 26 10 -15)) (make-rect-info RIGHT-MOST-POS UPPER-MOST-POS 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 369 274 10 15)) (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS 10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 31 225 -10 -15)) (make-rect-info LEFT-MOST-POS 210 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 369 225 10 -15)) (make-rect-info RIGHT-MOST-POS 210 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 300 26 -10 -15)) (make-rect-info 290 UPPER-MOST-POS -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 300 274 -10 15)) (make-rect-info 290 LOWER-MOST-POS -10 15) "Wrong rect movement"))

;check-hit-wall-both: rectangle -> boolean
;INTERP: this function checks the condition for perfect bounce
;GIVEN: a rectangle
;RETURNS: a boolean value representing whether at the current velocity, the rectangle would hit 2 of the 4 walls concurrently
;         in the next tick.
;Example: (check-hit-wall-both (make-rect-info 368 274 10 11)) -> true
;         (check-hit-wall-both (make-rect-info 368 270 1 2)) -> false
(define (check-hit-wall-both r)
  (and (check-hit-wall-x r) (check-hit-wall-y r)))

;check-hit-wall-x: rectangle -> boolean
;GIVEN: a rectangle
;RETURNS: boolean value representing if the rectangle's x-position is out of bounds.
;Example: (check-hit-wall-x (make-rect-info 22 144 12 13) -> true
;Strategy: dividing into cases; using template of rectangle
(define (check-hit-wall-x r)
  (cond
    [(and (< (rect-info-x r) RIGHT-MOST-POS) (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS)) true]
    [(and (> (rect-info-x r) LEFT-MOST-POS) (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS)) true]
    [else false]))

;check-hit-wall-y: rectangle -> boolean
;GIVEN: a rectangle
;RETURNS: boolean value representing if the rectangle's y-position is out of bounds.
;Example: (check-hit-wall-y (make-rect-info 222 143 12 13) -> false
;Strategy: dividing into cases; using template of rectangle
(define (check-hit-wall-y r)
  (cond
    [(and (< (rect-info-y r) LOWER-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS)) true]
    [(and (> (rect-info-y r) UPPER-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS)) true]
    [else false]))

;move-less-both: rectangle -> rectangle
;INTERP: this function handles the situation, where in next tick the rectangle would cross two walls under
;        its current velocity, by setting the rectangle tangential to two walls instead of out of bounds.
;GIVEN: a rectangle that under its current velocity would cross two walls in the next tick.
;RETURNS: a rectangle that follows the given one after the tick, tangential to two walls.
;Example: (move-less-both (make-rect-info 365 270 15 15)) -> (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS 15 15)
;Strategy: dividing into cases
(define (move-less-both r)
  (cond
    [(and (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS))
     (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r))]
    [(and (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS))
     (make-rect-info RIGHT-MOST-POS UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r))]
    [(and (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS))
     (make-rect-info LEFT-MOST-POS LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r))]
    [(and (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS))
     (make-rect-info LEFT-MOST-POS UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r))]))


;move-less-x: rectangle -> rectangle
;INTERP: this function handles the situation, where in next tick the rectangle's side would cross 
;        left or right wall under its current velocity, by setting the rectangle tangential to
;        wall instead of hitting it.
;GIVEN: a rectangle that under its current velocity would cross left/right wall in the next tick.
;RETURNS: a rectangle that follows the given one after the tick, tangential to the wall.
;Example: (move-less-x (make-rect-info 365 270 15 2)) -> (make-rect-info RIGHT-MOST-POS 272 15 2)
;Strategy: dividing into cases; using template of rectangle
(define (move-less-x r)
  (if (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS)
      (make-rect-info RIGHT-MOST-POS (+ (rect-info-y r) (rect-info-vy r)) (rect-info-vx r) (rect-info-vy r))
      (make-rect-info LEFT-MOST-POS (+ (rect-info-y r) (rect-info-vy r)) (rect-info-vx r) (rect-info-vy r))))

;move-less-y: rectangle -> rectangle
;INTERP: this function handles the situation, where in next tick the rectangle's side would cross 
;        upper or lower wall under its current velocity, by setting the rectangle tangential to the
;        wall instead of hitting it.
;GIVEN: a rectangle that under its current velocity would cross upper/lower wall in the next tick.
;RETURNS: a rectangle that follows the given one after the tick, tangential to the wall.
;Example: (move-less-y (make-rect-info 365 270 3 11)) -> (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS 3 11)
;Strategy: dividing into cases; using template of rectangle
(define (move-less-y r)
  (if (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS)
      (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r))
      (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r))))

;perfect-bounce rectangle -> rectangle
;GIVEN: a rectangle tangential to two walls
;RETURNS: a rectangle that follows the given one after the tick, with its x-velocity and y-velocity reversed,
;         and x-position and y-position bounced back by a distance equal to its corresponding velocity
;Example: (perfect-bounce (make-rect-info LEFT-MOST-POS UPPER-MOST-POS -10 -15)) -> (make-rect-info 40 40 10 15)
;Strategy: using template of rectangle; 
(define (perfect-bounce r)
  (make-rect-info (- (rect-info-x r) (rect-info-vx r)) (- (rect-info-y r) (rect-info-vy r))
                  (- 0 (rect-info-vx r)) (- 0 (rect-info-vy r))))

;reverse-x: rectangle -> rectangle
;GIVEN: a rectangle tangential to left/right wall
;RETURNS: a rectangle that follows the given one after the tick, with its x-velocity reversed,
;         and x-position bounced back by a distance equal to the velocity
;Example: (reverse-x (make-rect-info LEFT-MOST-POS UPPER-MOST-POS0 -10 15)) -> (make-rect-info 40 265 10 15)
;Strategy: dividing into cases; using template of rectangle; 
(define (reverse-x r)
  (make-rect-info (- (rect-info-x r) (rect-info-vx r)) (+ (rect-info-y r) (rect-info-vy r))
                  (- 0 (rect-info-vx r)) (rect-info-vy r)))

;reverse-y: rectangle -> rectangle
;GIVEN: a rectangle tangential to lower/upper wall
;RETURNS: a rectangle that follows the given one after the tick, with its y-velocity reversed,
;         and y-position bounced back by a distance equal to the velocity
;Example: (reverse-y (make-rect-info 35 LOWER-MOST-POS 10 15)) -> (make-rect-info 40 260 10 -15)
;Strategy: dividing into cases; using template of rectangle; 
(define (reverse-y r)
  (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) (- (rect-info-y r) (rect-info-vy r))
                  (rect-info-vx r) (- 0 (rect-info-vy r))))

;normal-move: rectangle -> rectangle
;GIVEN: a rectangle within the canvas
;RETURNS: a rectangle that follows the given rectangle after the tick, moving a distance in both directions
;         indicated by its velocity
;Example: (normal-move (make-rect-info 35 40 10 10) -> (make-rect-info 45 50 10 10)
;Strategy: using template of rectangle; structural decomposition
(define (normal-move r)
  (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) (+ (rect-info-y r) (rect-info-vy r))
                  (rect-info-vx r) (rect-info-vy r)))

;draw-world: world -> world
;GIVEN: a world state
;RETURNS: an image with blue rectangles drawn on the canvas
;Example: (draw-world (make-world (make-rect-info LEFT-MOST-POS 40 10 10) (make-rect-info 40 50 15 11)
;                                 false) -> image
;Strategy: using template of rectangle; structural decomposition
(define (draw-world w)
  (place-image DRAW-RECTANGLE (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image DRAW-RECTANGLE (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w)) DRAW-CANVAS))) 

(begin-for-test
  (check-equal? (draw-world (make-world (make-rect-info LEFT-MOST-POS 40 10 10) (make-rect-info 40 50 15 11) false))
                           (place-image DRAW-RECTANGLE LEFT-MOST-POS 40
                           (place-image DRAW-RECTANGLE 40 50 DRAW-CANVAS)) "Inconsistent images"))

;draw-rect1-location: world -> image
;INTERP: Draw the location information inside the rectangle 1 
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 1, located at the center of rectangle 1
;Example: (draw-rect1-location (make-world (make-rect-info 50 50 10 10)
;                                          (make-rect-info 100 100 20 20) false)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-rect1-location w)
  (text (string-append "(" (number->string (rect-info-vx (world-rect1 w))) ", "
                       (number->string (rect-info-vy (world-rect1 w))) ")")
        12 "blue"))

(begin-for-test
  (check-equal? (draw-rect1-location (make-world (make-rect-info 50 50 10 10) (make-rect-info 100 100 20 20) false))
                (text (string-append "(" (number->string 10) ", "
                       (number->string 10) ")") 12 "blue") "Inconsistent images"))

;draw-rect2-location: world -> image
;INTERP: Draw the location information inside the rectangle 2
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 1, located at the center of rectangle 2
;Example: (draw-rect2-location (make-world (make-rect-info 50 50 10 10)
;                                          (make-rect-info 100 100 20 20) false)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-rect2-location w)
  (text (string-append "(" (number->string (rect-info-vx (world-rect2 w))) ", "
                       (number->string (rect-info-vy (world-rect2 w))) ")")
        12 "blue"))

(begin-for-test
  (check-equal? (draw-rect2-location (make-world (make-rect-info 50 50 10 10) (make-rect-info 100 100 20 20) false))
                (text (string-append "(" (number->string 20) ", "
                       (number->string 20) ")") 12 "blue") "Inconsistent images"))


;draw-unfurnished-world: world -> image  
;INTERP: Draw the world with location info printed inside the rectangles
;GIVEN: a world state
;RETURNS: an image with blue rectangles drawn on the canvas; each rectangle's location info are drawn inside each rectangle
;Example: (draw-unfurnished-world (make-world (make-rect-info 150 150 -10 10)
;                                          (make-rect-info 110 120 20 -20) false)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-furnished-world w)
  (place-image (draw-rect1-location w) (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image (draw-rect2-location w) (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w))
                            (draw-world w))))

(begin-for-test
  (check-equal? (draw-furnished-world (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false))
                (place-image (draw-rect1-location (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false))
                             (rect-info-x (world-rect1 (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false)))
                             (rect-info-y (world-rect1 (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false)))
                             (place-image (draw-rect2-location (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false))
                                          (rect-info-x (world-rect2 (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false)))
                                          (rect-info-y (world-rect2 (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false)))
                                          (draw-world (make-world (make-rect-info 150 150 -10 10) (make-rect-info 110 120 20 -20) false))))
                "Inconsistent image"))


;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world state
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; Example: (world-after-key-event (make-world (make-rect-info 100 200 1 2) (make-rect-info 105 201 2 3)
;                                              false) " ")) -> (make-world (make-rect-info 100 200 1 2) (make-rect-info 105 201 2 3)
;                                                                             true)
;  Strategy: dividing into cases; usng template of world
(define (world-after-key-event w ke)
  (if (key=? ke PAUSE)
      (make-world (world-rect1 w) (world-rect2 w) (not (world-paused? w)))
      w))

(begin-for-test
  (check-equal? (world-after-key-event (make-world (make-rect-info 100 200 1 2) (make-rect-info 105 201 2 3) false) " ")
                (make-world (make-rect-info 100 200 1 2) (make-rect-info 105 201 2 3) true) "world should pause")
  (check-equal? (world-after-key-event (make-world (make-rect-info 100 200 1 2) (make-rect-info 105 201 2 3) false) "a")
                (make-world (make-rect-info 100 200 1 2) (make-rect-info 105 201 2 3) false) "world should pause"))

;; world-rect1 : WorldState -> Rectangle
;; world-rect2 : WorldState -> Rectangle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Example: (new-rectangle 100 200 10 20) -> (new-rectangle 100 200 10 20)
(define (new-rectangle n1 n2 i1 i2)
  (make-rect-info n1 n2 i1 i2))

(begin-for-test
  (check-equal? (new-rectangle 100 200 10 20) (make-rect-info 100 200 10 20)))

;; rect-x : Rectangle -> NonNegInt
;; rect-y : Rectangle -> NonNegInt
;; rect-vx : Rectangle -> Int
;; rect-vy : Rectangle -> Int
;; RETURNS: the coordinates of the center of the rectangle and its
;; velocity in the x- and y- directions.
;; Example: (rect-x (make-rect-info 100 200 10 20)) -> 100
(define (rect-x rectangle)
  (rect-info-x rectangle))

(define (rect-y rectangle)
  (rect-info-y rectangle))

(define (rect-vx rectangle)
  (rect-info-vx rectangle))

(define (rect-vy rectangle)
  (rect-info-vy rectangle))

(begin-for-test
  (check-equal? (rect-x (make-rect-info 100 200 10 20)) 100)
  (check-equal? (rect-y (make-rect-info 100 200 10 20)) 200)
  (check-equal? (rect-vx (make-rect-info 100 200 10 20)) 10)
  (check-equal? (rect-vy (make-rect-info 100 200 10 20)) 20))