;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
         rect-x
         rect-y
         rect-vx
         rect-vy)

(provide world-after-mouse-event
         rect-after-mouse-event
         rect-selected?
         new-rectangle)


;Constants definition
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

(define RECTANGLE-WIDTH 60)
(define RECTANGLE-HEIGHT 50)

(define RIGHT-MOST-POS 370)
(define LEFT-MOST-POS 30)
(define UPPER-MOST-POS 25)
(define LOWER-MOST-POS 275)
(define HALF-WIDTH LEFT-MOST-POS)
(define HALF-LEN UPPER-MOST-POS)

(define DRAW-RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue"))
(define DRAW-RED-RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "red"))
(define DRAW-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define PAUSE " ")

;Data definition
;world: (make-world rectangle rectangle boolean coordinate coordinate)
;INTERPRETATION: (make rec1 rec2 ifpaused x-coord y-coord), in which
; rec1 is a rectangle object inside the world
; rec2 is another rectangle object inside the world
; ifpaused is a boolean value indicating if the world is paused
; x-coord is an NonNegInteger representing the x coordinate of the mouse
; y-coord is an NonNegInteger representing the y coordinate of the mouse
(define-struct world (rect1 rect2 paused? mousex mousey))

;Template
(define (world-fn w)
  (... (world-rect1 w) (world-rect2 w) (world-paused? w) (world-mx w) (world-my w)))


;rect-info: (make-rect-info coordinate coordinate velocity-x-dir velocity-y-dir boolean)
;INTERPRETATION: (make-rect-info x y vx vy select), in which
;x is a NonNegInteger representing the x coordinate of the rectangle
;y is a NonNegInteger representing the y coordinate of the rectangle
;vx is is an Integer representing the velocity on the x direction of the rectangle
;vy is is an Integer representing the velocity on the y direction of the rectangle
;select is a boolean value representing if the rectangle is selected by mouse
(define-struct rect-info (x y vx vy selected? rx ry))

;Template
(define (rect-info-fn rect)
  (... (rect-info-x rect) (rect-info-y rect) (rect-info-vx rect) (rect-info-vy rect)
       (rect-info-selected? rect) (rect-info-rx rect) (rect-info-ry rect)))



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
            (on-mouse world-after-mouse-event)
            (on-draw draw-real-world)
            (on-key world-after-key-event)))

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; Example: (initial-world 3) -> (make-world (make-rect-info 200 100 -12 20 false) (make-rect-info 200 200 23 -14 false) false 0 0)
;; Strategy: using template method of rectangle struct
(define (initial-world rand)
  (make-world (make-rect-info 200 100 -12 20 false 10 20)
              (make-rect-info 200 200 23 -14 false 10 20)
              true 0 0))

(begin-for-test
 (check-equal? (initial-world 1) (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 false 10 20) true 0 0)))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; Example: (world-after-tick (make-world (make-rect-info 200 100 -12 20 false) (make-rect-info 200 200 23 -14 false) false 0 0))
;;        -> (make-world (make-rect-info 188 120 -12 20 false) (make-rect-info 223 186 23 -14 false) false 0 0)
;;Strategy: dividing into cases; using template of world struct and rectangle struct; using simpler functions
(define (world-after-tick w)
  (cond
    [(world-paused? w) w]
    [(if-both-selected (world-rect1 w) (world-rect2 w)) (make-world (world-rect1 w) (world-rect2 w)
                                                                    (world-paused? w) (world-mousex w) (world-mousey w))]
    [(if-selected (world-rect1 w)) (make-world (world-rect1 w) (rect-after-tick (world-rect2 w))
                                               (world-paused? w) (world-mousex w) (world-mousey w))]
    [(if-selected (world-rect2 w)) (make-world (rect-after-tick (world-rect1 w)) (world-rect2 w)
                                               (world-paused? w) (world-mousex w) (world-mousey w))]
    [else (make-world (rect-after-tick (world-rect1 w)) (rect-after-tick (world-rect2 w))
                      (world-paused? w) (world-mousex w) (world-mousey w))]))

(begin-for-test
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 false 10 20) true 0 0))
                (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 false 10 20) true 0 0)  "should return same world")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 false 10 20) false 0 0))
                (make-world (make-rect-info 188 120 -12 20 false 10 20) (make-rect-info 223 186 23 -14 false 10 20) false 0 0) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20 true 10 20) (make-rect-info 200 200 23 -14 true 10 20) false 0 0))
                (make-world (make-rect-info 200 100 -12 20 true 10 20)  (make-rect-info 200 200 23 -14 true 10 20) false 0 0) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20 true 10 20) (make-rect-info 200 200 23 -14 false 10 20) false 0 0))
                (make-world (make-rect-info 200 100 -12 20 true 10 20) (make-rect-info 223 186 23 -14 false 10 20) false 0 0) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 true 10 20) false 0 0))
                (make-world (make-rect-info 188 120 -12 20 false 10 20) (make-rect-info 200 200 23 -14 true 10 20)false 0 0) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 true 10 20) true 0 0))
                (make-world (make-rect-info 200 100 -12 20 false 10 20) (make-rect-info 200 200 23 -14 true 10 20) true 0 0) "Wrong coordinate update"))
                

;rect-after-tick: rectangle -> rectangle
;GIVEN: a rectangle object
;RETURNS: a rectangle object that follows the given one after the tick
;Example: (rect-after-tick (make-rect-info 100 120 -10 15 false)) -> (make-rect-info 90 135 -10 15 false)
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
  (check-equal? (rect-after-tick (make-rect-info 100 120 -10 15 false 10 20)) (make-rect-info 90 135 -10 15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 30 25 -10 -15 false 10 20)) (make-rect-info 40 40 10 15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 30 275 -10 15 false 10 20)) (make-rect-info 40 260 10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 370 25 10 -15 false 10 20)) (make-rect-info 360 40 -10 15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 370 275 10 15 false 10 20)) (make-rect-info 360 260 -10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 30 225 -10 -15 false 10 20)) (make-rect-info 40 210 10 -15 false 10 20)"Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 370 225 10 -15 false 10 20)) (make-rect-info 360 210 -10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 320 25 -10 -15 false 10 20)) (make-rect-info 310 40 -10 15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 320 275 -10 15 false 10 20)) (make-rect-info 310 260 -10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 31 26 -10 -15 false 10 20)) (make-rect-info 30 25 -10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 31 274 -10 15 false 10 20)) (make-rect-info 30 275 -10 15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 369 26 10 -15 false 10 20)) (make-rect-info 370 25 10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 369 274 10 15 false 10 20)) (make-rect-info 370 275 10 15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 31 225 -10 -15 false 10 20)) (make-rect-info 30 210 -10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 369 225 10 -15 false 10 20)) (make-rect-info 370 210 10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 300 26 -10 -15 false 10 20)) (make-rect-info 290 25 -10 -15 false 10 20) "Wrong rect movement")
  (check-equal? (rect-after-tick (make-rect-info 300 274 -10 15 false 10 20)) (make-rect-info 290 275 -10 15 false 10 20) "Wrong rect movement"))

;check-hit-wall-both: rectangle -> boolean
;INTERP: this function checks the condition for perfect bounce
;GIVEN: a rectangle
;RETURNS: a boolean value representing whether at the current velocity, the rectangle would hit 2 of the 4 walls concurrently
;         in the next tick.
;Example: (check-hit-wall-both (make-rect-info 368 274 10 11 false)) -> true
;         (check-hit-wall-both (make-rect-info 368 270 1 2 false)) -> false
;Strategy: combining simpler functions
(define (check-hit-wall-both r)
  (and (check-hit-wall-x r) (check-hit-wall-y r)))

;move-less-both: rectangle -> rectangle
;INTERP: this function handles the situation, where in next tick the rectangle would cross two walls under
;        its current velocity, by setting the rectangle tangential to two walls instead of out of bounds.
;GIVEN: a rectangle that under its current velocity would cross two walls in the next tick.
;RETURNS: a rectangle that follows the given one after the tick, tangential to two walls.
;Example: (move-less-both (make-rect-info 365 270 15 15 false)) -> (make-rect-info 370 275 15 15 false)
;Strategy: dividing into cases
(define (move-less-both r)
  (cond
    [(and (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS))
     (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))]
    [(and (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS))
     (make-rect-info RIGHT-MOST-POS UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))]
    [(and (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS))
     (make-rect-info LEFT-MOST-POS LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))]
    [(and (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS))
     (make-rect-info LEFT-MOST-POS UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))]))

;check-hit-wall-x: rectangle -> boolean
;GIVEN: a rectangle
;RETURNS: boolean value representing if the rectangle's x-position is out of bounds.
;Example: (check-hit-wall-x (make-rect-info 22 144 12 13 false) -> true
;Strategy: dividing into cases; using template of rectangle
(define (check-hit-wall-x r)
  (cond
    [(and (< (rect-info-x r) RIGHT-MOST-POS) (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS)) true]
    [(and (> (rect-info-x r) LEFT-MOST-POS) (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS)) true]
    [else false]))

;check-hit-wall-y: rectangle -> boolean
;GIVEN: a rectangle
;RETURNS: boolean value representing if the rectangle's y-position is out of bounds.
;Example: (check-hit-wall-y (make-rect-info 222 143 12 13 true) -> false
;Strategy: dividing into cases; using template of rectangle
(define (check-hit-wall-y r)
  (cond
    [(and (< (rect-info-y r) LOWER-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS)) true]
    [(and (> (rect-info-y r) UPPER-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS)) true]
    [else false]))

;move-less-x: rectangle -> rectangle
;INTERP: this function handles the situation, where in next tick the rectangle's side would cross 
;        left or right wall under its current velocity, by setting the rectangle tangential to
;        wall instead of hitting it.
;GIVEN: a rectangle that under its current velocity would cross left/right wall in the next tick.
;RETURNS: a rectangle that follows the given one after the tick, tangential to the wall.
;Example: (move-less-x (make-rect-info 365 270 15 2 false)) -> (make-rect-info 370 272 15 2 false)
;Strategy: dividing into cases; using template of rectangle
(define (move-less-x r)
  (if (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS)
      (make-rect-info RIGHT-MOST-POS (+ (rect-info-y r) (rect-info-vy r)) (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))
      (make-rect-info LEFT-MOST-POS (+ (rect-info-y r) (rect-info-vy r)) (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))))

;move-less-y: rectangle -> rectangle
;INTERP: this function handles the situation, where in next tick the rectangle's side would cross 
;        upper or lower wall under its current velocity, by setting the rectangle tangential to the
;        wall instead of hitting it.
;GIVEN: a rectangle that under its current velocity would cross upper/lower wall in the next tick.
;RETURNS: a rectangle that follows the given one after the tick, tangential to the wall.
;Example: (move-less-y (make-rect-info 365 270 3 11 false)) -> (make-rect-info 370 275 3 11 false)
;Strategy: dividing into cases; using template of rectangle
(define (move-less-y r)
  (if (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS)
      (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))
      (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r))))

;perfect-bounce rectangle -> rectangle
;GIVEN: a rectangle tangential to two walls
;RETURNS: a rectangle that follows the given one after the tick, with its x-velocity and y-velocity reversed,
;         and x-position and y-position bounced back by a distance equal to its corresponding velocity
;Example: (perfect-bounce (make-rect-info 30 25 -10 -15 false)) -> (make-rect-info 40 40 10 15 false)
;Strategy: using template of rectangle; 
(define (perfect-bounce r)
  (make-rect-info (- (rect-info-x r) (rect-info-vx r)) (- (rect-info-y r) (rect-info-vy r))
                  (- 0 (rect-info-vx r)) (- 0 (rect-info-vy r)) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r)))

;reverse-x: rectangle -> rectangle
;GIVEN: a rectangle tangential to left/right wall
;RETURNS: a rectangle that follows the given one after the tick, with its x-velocity reversed,
;         and x-position bounced back by a distance equal to the velocity
;Example: (reverse-x (make-rect-info 30 250 -10 15 false)) -> (make-rect-info 40 265 10 15 false)
;Strategy: dividing into cases; using template of rectangle; 
(define (reverse-x r)
  (make-rect-info (- (rect-info-x r) (rect-info-vx r)) (+ (rect-info-y r) (rect-info-vy r))
                  (- 0 (rect-info-vx r)) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r)))

;reverse-y: rectangle -> rectangle
;GIVEN: a rectangle tangential to lower/upper wall
;RETURNS: a rectangle that follows the given one after the tick, with its y-velocity reversed,
;         and y-position bounced back by a distance equal to the velocity
;Example: (reverse-y (make-rect-info 35 275 10 15 false)) -> (make-rect-info 40 260 10 -15 false)
;Strategy: dividing into cases; using template of rectangle; 
(define (reverse-y r)
  (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) (- (rect-info-y r) (rect-info-vy r))
                          (rect-info-vx r) (- 0 (rect-info-vy r)) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r)))

;normal-move: rectangle -> rectangle
;GIVEN: a rectangle within the canvas
;RETURNS: a rectangle that follows the given rectangle after the tick, moving a distance in both directions
;         indicated by its velocity
;Example: (normal-move (make-rect-info 35 40 10 10 false) -> (make-rect-info 45 50 10 10 false)
;Strategy: using template of rectangle; structural decomposition
(define (normal-move r)
  (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) (+ (rect-info-y r) (rect-info-vy r))
                  (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r)))

;draw-world: world -> world
;GIVEN: a world state
;RETURNS: an image with blue rectangles drawn on the canvas
;Example: (draw-world (make-world (make-rect-info 30 40 10 10 false) (make-rect-info 40 50 15 11 false)
;                                 false 300 250) -> image
;Strategy: using template of rectangle; structural decomposition
(define (draw-world w)
  (place-image DRAW-RECTANGLE (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image DRAW-RECTANGLE (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w)) DRAW-CANVAS))) 

(begin-for-test
  (check-equal? (draw-world (make-world (make-rect-info 30 40 10 10 false 10 20) (make-rect-info 40 50 15 11 false 10 20) false 300 250))
                           (place-image DRAW-RECTANGLE 30 40
                           (place-image DRAW-RECTANGLE 40 50 DRAW-CANVAS)) "Inconsistent images"))

;draw-rect1-location: world -> image
;INTERP: Draw the location information inside the rectangle 1 
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 1, located at the center of rectangle 1
;Example: (draw-rect1-location (make-world (make-rect-info 50 50 10 10 false)
;                                          (make-rect-info 100 100 20 20 false) false 200 200)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-rect1-location w)
  (text (string-append "(" (number->string (rect-info-vx (world-rect1 w))) ", "
                       (number->string (rect-info-vy (world-rect1 w))) ")")
        12 "blue"))

(begin-for-test
  (check-equal? (draw-rect1-location (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200))
                (text (string-append "(" (number->string 10) ", "
                       (number->string 10) ")") 12 "blue") "Inconsistent images"))

;draw-rect2-location: world -> image
;INTERP: Draw the location information inside the rectangle 2
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 1, located at the center of rectangle 2
;Example: (draw-rect2-location (make-world (make-rect-info 50 50 10 10 false)
;                                          (make-rect-info 100 100 20 20 false) false 200 200)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-rect2-location w)
  (text (string-append "(" (number->string (rect-info-vx (world-rect2 w))) ", "
                       (number->string (rect-info-vy (world-rect2 w))) ")")
        12 "blue"))

(begin-for-test
  (check-equal? (draw-rect2-location (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200))
                (text (string-append "(" (number->string 20) ", "
                       (number->string 20) ")") 12 "blue") "Inconsistent images"))

;draw-unfurnished-world: world -> image  
;INTERP: Draw the world with location info printed inside the rectangles
;GIVEN: a world state
;RETURNS: an image with blue rectangles drawn on the canvas; each rectangle's location info are drawn inside each rectangle
;Example: (draw-unfurnished-world (make-world (make-rect-info 150 150 -10 10 false)
;                                          (make-rect-info 110 120 20 -20 false) false 200 200)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-unfurnished-world w)
  (place-image (draw-rect1-location w) (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image (draw-rect2-location w) (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w))
                            (draw-world w))))

(begin-for-test
  (check-equal? (draw-unfurnished-world (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200))
                (place-image (draw-rect1-location (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200))
                             (rect-info-x (world-rect1 (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200)))
                             (rect-info-y (world-rect1 (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200)))
                             (place-image (draw-rect2-location (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200))
                                          (rect-info-x (world-rect2 (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200)))
                                          (rect-info-y (world-rect2 (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200)))
                                          (draw-world (make-world (make-rect-info 150 150 -10 10 false 10 20) (make-rect-info 110 120 20 -20 false 10 20) false 200 200))))
                "Inconsistent image"))

;draw-rect1-location-r: world -> image
;INTERP: Draw the location information in red color inside the rectangle 1, located at the center of rectangle 1
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 1
;Example: (draw-rect1-location-r (make-world (make-rect-info 50 50 10 10 false)
;                                          (make-rect-info 100 100 20 20 false) false 200 200)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-rect1-location-r w)
  (text (string-append "(" (number->string (rect-info-vx (world-rect1 w))) ", "
                       (number->string (rect-info-vy (world-rect1 w))) ")")
        12 "red"))

(begin-for-test
  (check-equal? (draw-rect1-location-r (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200))
                (text (string-append "(" (number->string (rect-info-vx (world-rect1 (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200)))) ", "
                       (number->string (rect-info-vy (world-rect1 (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200)))) ")")
        12 "red")
                "Inconsistent image"))

;draw-rect2-location-r: world -> image
;INTERP: Draw the location information in red color inside the rectangle 2, located at the center of rectangle 2
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 2
;Example: (draw-rect1-location (make-world (make-rect-info 50 50 10 10 false)
;                                          (make-rect-info 100 100 20 20 false) false 200 200)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-rect2-location-r w)
  (text (string-append "(" (number->string (rect-info-vx (world-rect2 w))) ", "
                       (number->string (rect-info-vy (world-rect2 w))) ")")
        12 "red"))

(begin-for-test
  (check-equal? (draw-rect2-location-r (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200))
                (text (string-append "(" (number->string (rect-info-vx (world-rect2 (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200)))) ", "
                       (number->string (rect-info-vy (world-rect2 (make-world (make-rect-info 50 50 10 10 false 10 20) (make-rect-info 100 100 20 20 false 10 20) false 200 200)))) ")")
        12 "red")
                "Inconsistent image"))

;draw-red-rect: world -> image
;draw-red-blue-rect: world -> image
;draw-blue-red-rect: world -> image
;INTERP: all three functions given a world, draw two rectangles in color represented by its name:
;        draw-red-rect draws two red rectangles; draw-red-blue-rect draws first rectangles as red
;        and second rectangle as blue; draw-blue-red-rect draws first rectangle as blue and second
;        rectangle as red
;GIVEN: world state
;RETURNS: an image with two rectangles in either red or blue color within the canvas, located according
;        to their x and y positions
;Example: (draw-red-rect (make-world (make-rect-info 150 120 10 20 false)
;                                    (make-rect-info 150 120 10 20 false) 200 250 false)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (draw-red-rect w)
  (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w)) DRAW-CANVAS)))

(define (draw-red-blue-rect w)
  (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image DRAW-RECTANGLE (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w)) DRAW-CANVAS)))

(define (draw-blue-red-rect w)
  (place-image DRAW-RECTANGLE (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w)) DRAW-CANVAS)))

(begin-for-test
  (check-equal? (draw-red-rect (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))
                (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect2 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false)))
                             (rect-info-y (world-rect2 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) DRAW-CANVAS)
                "inconsistent image")
  (check-equal? (draw-red-blue-rect (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))
                (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect1 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) (rect-info-y (world-rect1 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false)))
               (place-image DRAW-RECTANGLE (rect-info-x (world-rect2 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) (rect-info-y (world-rect2 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) DRAW-CANVAS))
                "inconsistent image")
  (check-equal? (draw-blue-red-rect (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))
              (place-image DRAW-RECTANGLE (rect-info-x (world-rect1 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) (rect-info-y (world-rect1 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false)))
               (place-image DRAW-RED-RECTANGLE (rect-info-x (world-rect2 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) (rect-info-y (world-rect2 (make-world (make-rect-info 150 120 10 20 false 10 20) (make-rect-info 150 120 10 20 false 10 20) 200 250 false))) DRAW-CANVAS))
              "inconsistent image"))

;draw-red-world: world -> image
;draw-red-blue-world: world -> image
;draw-blue-red-world: world -> image
;INTERP: all three functions draw location information with corresponding colors inside rectangles,
;        which are also drawn on the canvas
;GIVEN: world state
;RETURNS: image containing elements including location info (with color), rectangles (with color), and canvas.
;Exmaple: (draw-red-world (make-world (make-rect-info 120 200 30 -15 false)
;                                     (make-rect-info 130 190 30 -15 false) 130 190 false)) -> image
;Strategy: combining simpler functions; using template of world and rectangle;
(define (draw-red-world w)
  (place-image (draw-rect1-location-r w) (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image (draw-rect2-location-r w) (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w))
                            (draw-red-rect w))))

(define (draw-red-blue-world w)
  (place-image (draw-rect1-location-r w) (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image (draw-rect2-location w) (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w))
                            (draw-red-blue-rect w))))

(define (draw-blue-red-world w)
  (place-image (draw-rect1-location w) (rect-info-x (world-rect1 w)) (rect-info-y (world-rect1 w))
               (place-image (draw-rect2-location-r w) (rect-info-x (world-rect2 w)) (rect-info-y (world-rect2 w))
                            (draw-blue-red-rect w))))

(begin-for-test
  (check-equal? (draw-red-world (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))
                (place-image (draw-rect1-location-r (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)) (rect-info-x (world-rect1 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))) (rect-info-y (world-rect1 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)))
                             (place-image (draw-rect2-location-r (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)) (rect-info-x (world-rect2 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))) (rect-info-y (world-rect2 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)))
                                          (draw-red-rect (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))))
                "Inconsistent image")
  (check-equal? (draw-red-blue-world (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))
                (place-image (draw-rect1-location-r (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)) (rect-info-x (world-rect1 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))) (rect-info-y (world-rect1 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)))
               (place-image (draw-rect2-location (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)) (rect-info-x (world-rect2 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))) (rect-info-y (world-rect2 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)))
                            (draw-red-blue-rect (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))))
                "Inconsistent image")
  (check-equal? (draw-blue-red-world (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))
                (place-image (draw-rect1-location (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)) (rect-info-x (world-rect1 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))) (rect-info-y (world-rect1 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)))
               (place-image (draw-rect2-location-r (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)) (rect-info-x (world-rect2 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))) (rect-info-y (world-rect2 (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false)))
                            (draw-blue-red-rect (make-world (make-rect-info 120 200 30 -15 false 10 20) (make-rect-info 130 190 30 -15 false 10 20) 130 190 false))))
                "Inconsistent image"))

  
;draw-cursor-on-red-world: world -> image
;draw-cursor-on-red-blue-world: world -> image
;draw-cursor-on-blue-red-world: world -> image
;INTERP: all three functions draw a red circle inside the rectangle(s) that has been selected by mouse
;GIVEN: a world state
;RETURN: an image in which a red circle has been drawn inside rectangle(s) which is drawn on the canvas
;Example: (draw-cursor-on-red-world (make-world (make-rect-info 200 230 19 18 true) (make-rect-info 40 230 19 20 false)
;                                               false 200 220)) -> image
;Strategy: combining simpler functions; using template of world
(define (draw-cursor-on-red-world w)
  (place-image (circle 5 "outline" "red") (world-mousex w) (world-mousey w)
               (draw-red-world w)))

(define (draw-cursor-on-red-blue-world w)
  (place-image (circle 5 "outline" "red") (world-mousex w) (world-mousey w)
               (draw-red-blue-world w)))

(define (draw-cursor-on-blue-red-world w)
  (place-image (circle 5 "outline" "red") (world-mousex w) (world-mousey w)
               (draw-blue-red-world w)))

(begin-for-test
  (check-equal? (draw-cursor-on-red-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
               (place-image (circle 5 "outline" "red") (world-mousex (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220)) (world-mousey (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                            (draw-red-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220)))
               "Inconsistent image")
  (check-equal? (draw-cursor-on-red-blue-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
               (place-image (circle 5 "outline" "red") (world-mousex (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220)) (world-mousey (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                            (draw-red-blue-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220)))
               "Inconsistent image")
  (check-equal? (draw-cursor-on-blue-red-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
               (place-image (circle 5 "outline" "red") (world-mousex (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220)) (world-mousey (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                            (draw-blue-red-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220)))
               "Inconsistent image"))

;draw-real-world: World -> image
;GIVEN: world state
;RETURNS: an image containing every object inside, including red circle, rectangles, location info, canvas.
;Example: (draw-real-world (make-world (make-rect-info 200 230 19 18 true) (make-rect-info 40 230 19 20 false)
;                                               false 200 220)) -> image
;Strategy: dividing into cases; combining simpler functions
(define (draw-real-world w)
  (cond
    [(if-both-selected (world-rect1 w) (world-rect2 w)) (draw-cursor-on-red-world w)]
    [(if-selected (world-rect1 w)) (draw-cursor-on-red-blue-world w)]
    [(if-selected (world-rect2 w)) (draw-cursor-on-blue-red-world w)]
    [else (draw-unfurnished-world w)]))

(begin-for-test
  (check-equal? (draw-real-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 true 10 20) false 200 220))
                (draw-cursor-on-red-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 true 10 20) false 200 220))
                "inconsistent image")
  (check-equal? (draw-real-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                (draw-cursor-on-red-blue-world (make-world (make-rect-info 200 230 19 18 true 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                "inconsistent image")
  (check-equal? (draw-real-world (make-world (make-rect-info 200 230 19 18 false 10 20) (make-rect-info 40 230 19 20 true 10 20) false 200 220))
                (draw-cursor-on-blue-red-world (make-world (make-rect-info 200 230 19 18 false 10 20) (make-rect-info 40 230 19 20 true 10 20) false 200 220))
                "inconsistent image")
  (check-equal? (draw-real-world (make-world (make-rect-info 200 230 19 18 false 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                (draw-unfurnished-world (make-world (make-rect-info 200 230 19 18 false 10 20) (make-rect-info 40 230 19 20 false 10 20) false 200 220))
                "inconsistent image"))


;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world state
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; Example: (world-after-key-event (make-world (make-rect-info 100 200 1 2 false) (make-rect-info 105 201 2 3 false)
;                                               false 50 100) " ")) -> (make-world (make-rect-info 100 200 1 2 false) (make-rect-info 105 201 2 3 false)
;                                                                             true 50 100)
;  Strategy: dividing into cases; usng template of world
(define (world-after-key-event w ke)
  (if (key=? ke PAUSE)
      (make-world (world-rect1 w) (world-rect2 w) (not (world-paused? w)) (world-mousex w) (world-mousey w))
      w))

(begin-for-test
  (check-equal? (world-after-key-event (make-world (make-rect-info 100 200 1 2 false 10 20) (make-rect-info 105 201 2 3 false 10 20) false 50 100) " ")
                (make-world (make-rect-info 100 200 1 2 false 10 20) (make-rect-info 105 201 2 3 false 10 20) true 50 100) "world should pause")
  (check-equal? (world-after-key-event (make-world (make-rect-info 100 200 1 2 false 10 20) (make-rect-info 105 201 2 3 false 10 20) false 50 100) "a")
                (make-world (make-rect-info 100 200 1 2 false 10 20) (make-rect-info 105 201 2 3 false 10 20) false 50 100) "world should pause"))

;; world-rect1 : WorldState -> Rectangle
;; world-rect2 : WorldState -> Rectangle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; provided by default define-struct

;; rect-x : Rectangle -> NonNegInt
;; rect-y : Rectangle -> NonNegInt
;; rect-vx : Rectangle -> Int
;; rect-vy : Rectangle -> Int
;; RETURNS: the coordinates of the center of the rectangle and its
;; velocity in the x- and y- directions.
;; Example: (rect-x (make-rect-info 100 200 10 20 false)) -> 100
;; Strategy: using template of rectangle
(define (rect-x rectangle)
  (rect-info-x rectangle))

(define (rect-y rectangle)
  (rect-info-y rectangle))

(define (rect-vx rectangle)
  (rect-info-vx rectangle))

(define (rect-vy rectangle)
  (rect-info-vy rectangle))

(begin-for-test
  (check-equal? (rect-x (make-rect-info 100 200 10 20 false 10 20)) 100)
  (check-equal? (rect-y (make-rect-info 100 200 10 20 false 10 20)) 200)
  (check-equal? (rect-vx (make-rect-info 100 200 10 20 false 10 20)) 10)
  (check-equal? (rect-vy (make-rect-info 100 200 10 20 false 10 20)) 20))

;; world-after-mouse-event: WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; Example: (world-after-mouse-event (make-world (make-rect-info 25 20 -3 3 true)
;                                                (make-rect-info 10 10 -5 -1 false) 10 20 false) 30 30 "button-up"))
;          -> (make-world (make-rect-info 25 20 -3 3 false)
;                                                (make-rect-info 10 10 -5 -1 false) 30 30 false)
;  Strategy: dividing into cases; using template of world; combining simpler functions
(define (world-after-mouse-event world mx my me)
  (cond
    [(mouse=? "button-up" me) (mouse-up world mx my)]
    [(mouse=? "button-down" me) (mouse-down world mx my)]
    [(mouse=? "drag" me) (mouse-drag world mx my)]
    [else world]))

(begin-for-test
  (check-equal? (world-after-mouse-event (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30 "button-up")
                (mouse-up (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30))
  (check-equal? (world-after-mouse-event (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30 "button-down")
                (mouse-down (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30))
  (check-equal? (world-after-mouse-event (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30 "drag")
                (mouse-drag (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30))
  (check-equal? (world-after-mouse-event (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20) 30 30 "enter")
                (make-world (make-rect-info 25 20 -3 3 true 10 20) (make-rect-info 10 10 -5 -1 false 10 20) false 10 20))
  )
  
;mouse-up: World NonNegInt NonNegInt-> World
;INTERP: given a world and mouse position, this function returns a world in which the rectangles' attributes are updated according
;        to the mouse event, in this case the "button down" scenario
;GIVEN: a world state, x coordinate of mouse, y coordinate of mouse
;RETURNS: a world state that follows the given one, with mouse positions updated and rectangle's
;         attributes updated
;Example: (mouse-up (make-world (make-rect-info 100 200 10 20 false) (make-rect-info 100 200 10 20 false)
;                               200 250 false) 200 300) -> (make-world (make-rect-info 100 200 10 20 false) (make-rect-info 100 200 10 20 false)
;                               200 250 false)
;Strategy: dividing into cases; using template of world; combining simpler functions
(define (mouse-up world mx my)
  (cond
    [(if-both-selected (world-rect1 world) (world-rect1 world))
     (make-world (uncheck-rect (world-rect1 world)) (uncheck-rect (world-rect2 world))
                 (world-paused? world) mx my)]
    [(if-selected (world-rect1 world))
     (make-world (uncheck-rect (world-rect1 world)) (world-rect2 world)
                 (world-paused? world) mx my)]
    [(if-selected (world-rect2 world))
     (make-world (world-rect1 world) (uncheck-rect (world-rect2 world))
                 (world-paused? world) mx my)]
    [else (make-world (world-rect1 world) (world-rect2 world)
                 (world-paused? world) mx my)]))

(begin-for-test
  (check-equal? (mouse-up (make-world (make-rect-info 100 200 10 20 false 10 20) (make-rect-info 100 200 10 20 false 10 20)
                               false 200 250) 200 300)
                (make-world (make-rect-info 100 200 10 20 false 10 20) (make-rect-info 100 200 10 20 false 10 20) false 200 300))
  (check-equal? (mouse-up (make-world (make-rect-info 200 250 10 20 true 10 20) (make-rect-info 100 200 10 20 false 10 20)
                               false 200 250) 200 250) 
                (make-world (uncheck-rect (make-rect-info 200 250 10 20 true 10 20)) (make-rect-info 100 200 10 20 false 10 20) false 200 250))
  (check-equal? (mouse-up (make-world (make-rect-info 100 200 10 20 false 10 20) (make-rect-info 100 200 10 20 true 10 20)
                               false 200 250) 200 300)
                (make-world (make-rect-info 100 200 10 20 false 10 20) (make-rect-info 100 200 10 20 false 10 20) false 200 300))
  (check-equal? (mouse-up (make-world (make-rect-info 100 200 10 20 true 10 20) (make-rect-info 100 200 10 20 true 10 20)
                                      false 200 250) 200 300)
                (make-world (make-rect-info 100 200 10 20 false 10 20) (make-rect-info 100 200 10 20 false 10 20) false 200 300))
  (check-equal? (mouse-up (make-world (make-rect-info 200 250 10 20 true 10 20) (make-rect-info 100 200 10 20 false 10 20)
                               false 200 250) 150 200) 
                (make-world (uncheck-rect (make-rect-info 200 250 10 20 true 10 20)) (make-rect-info 100 200 10 20 false 10 20) false 150 200)))

;mouse-down: World NonNegInt NonNegInt-> World
;INTERP: given a world and mouse position, this function returns a world in which the rectangles' attributes are updated according
;        to the mouse event, in this case the "button down" scenario
;GIVEN: a world state, x coordinate of mouse, y coordinate of mouse
;RETURNS: a world state that follows the given one, with mouse positions updated and rectangle's
;         attributes updated
;Example: (mouse-down (make-world (make-rect-info 100 200 10 20 false) (make-rect-info 100 200 10 20 false)
;                               200 250 false) 100 210) -> (make-world (make-rect-info 100 200 10 20 true) (make-rect-info 100 200 10 20 false)
;                               100 210 true)
;Strategy: dividing into cases; using template of world; combining simpler functions
(define (mouse-down world mx my)
  (cond
    [(if-inside-both (world-rect1 world) (world-rect2 world) mx my)
                     (select-both world mx my)]
    [(if-inside (world-rect1 world) mx my) (select-rect-1 world mx my)]
    [(if-inside (world-rect2 world) mx my) (select-rect-2 world mx my)]
    [else world]))

(begin-for-test
  (check-equal? (mouse-down (make-world (make-rect-info 100 200 10 20 false 10 20) (make-rect-info 100 200 10 20 false 10 20) false 200 250) 100 210)
                (make-world (make-rect-info 100 200 10 20 true 0 -10) (make-rect-info 100 200 10 20 true 0 -10) false 100 210))
  (check-equal? (mouse-down (make-world (make-rect-info 150 250 10 20 false 10 20) (make-rect-info 31 40 10 20 false 10 20) false 15 25) 150 250)
                (make-world (make-rect-info 150 250 10 20 true 0 0) (make-rect-info 31 40 10 20 false 10 20) false 150 250))

  (check-equal? (mouse-down (make-world (make-rect-info 31 40 10 20 false 10 20) (make-rect-info 150 250 10 20 false 10 20) false 15 25) 150 250)
                (make-world (make-rect-info 31 40 10 20 false 10 20) (make-rect-info 150 250 10 20 true 0 0) false 150 250))

  (check-equal? (mouse-down (make-world (make-rect-info 120 130 10 20 false 10 20) (make-rect-info 150 250 10 20 false 10 20) false 15 25) 0 0)
                (make-world (make-rect-info 120 130 10 20 false 10 20) (make-rect-info 150 250 10 20 false 10 20) false 15 25)))

;mouse-drag: World NonNegInt NonNegInt-> World
;INTERP: given a world and mouse position, this function returns a world in which the rectangles' attributes are updated according
;        to the mouse event, in this case the "drag" scenario
;GIVEN: a world state, x coordinate of mouse, y coordinate of mouse
;RETURNS: a world state that follows the given one, with mouse positions updated and rectangle's
;         attributes updated
;Example: (mouse-up (make-world (make-rect-info 100 200 10 20 true) (make-rect-info 100 200 10 20 true)
;                               190 200 false) 100 210) -> (make-world (make-rect-info 110 210 10 20 true) (make-rect-info 110 210 10 20 true)
;                               100 210 true)
;Strategy: dividing into cases; using template of world; combining simpler functions
(define (mouse-drag w mx my)
  (cond
    [(if-both-selected (world-rect1 w) (world-rect2 w)) (drag-both w mx my)]
    [(if-selected (world-rect1 w)) (drag-one w mx my)]
    [(if-selected (world-rect2 w)) (drag-two w mx my)]
    [else w]))

(begin-for-test
  (check-equal? (mouse-drag (make-world (make-rect-info 100 200 10 20 true 10 20) (make-rect-info 100 200 10 20 true 10 20) false 200 250) 100 210)
                (drag-both (make-world (make-rect-info 100 200 10 20 true 10 20) (make-rect-info 100 200 10 20 true 10 20) false 200 250) 100 210))
  (check-equal? (mouse-drag (make-world (make-rect-info 150 250 10 20 true 10 20) (make-rect-info 31 40 10 20 false 10 20) false 15 25) 150 250)
                (drag-one (make-world (make-rect-info 150 250 10 20 true 10 20) (make-rect-info 31 40 10 20 false 10 20) false 15 25) 150 250))
  (check-equal? (mouse-drag (make-world (make-rect-info 150 250 10 20 false 10 20) (make-rect-info 31 40 10 20 true 10 20) false 15 25) 150 250)
                (drag-two (make-world (make-rect-info 150 250 10 20 false 10 20) (make-rect-info 31 40 10 20 true 10 20) false 15 25) 150 250))
  (check-equal? (mouse-drag (make-world (make-rect-info 120 130 10 20 false 10 20) (make-rect-info 150 250 10 20 false 10 20) false 15 25) 0 0)
                (make-world (make-rect-info 120 130 10 20 false 10 20) (make-rect-info 150 250 10 20 false 10 20) false 15 25)))

;drag-both: World NonNegInt NonNegInt -> world 
;drag-one: World NonNegInt NonNegInt -> world 
;drag-two: World NonNegInt NonNegInt -> world
;GIVEN: a world state and position of mouse
;RETURNS: a world state in which the rectangles' positions are updated according to mouse's movement
;Example: (drag-both (make-world (make-rect-info 100 200 10 20 true) (make-rect-info 100 200 10 20 true)
;                               190 200 false) 100 210)
;          -> (make-world (make-rect-info 110 210 10 20 true) (make-rect-info 110 210 10 20 true)
;                               100 210 true)
;Strategy: combining simpler functions; using template of world
(define (drag-both w mx my)
  (make-world (move-rect-one w mx my) (move-rect-two w mx my) (world-paused? w) mx my))

(define (drag-one w mx my)
  (make-world (move-rect-one w mx my) (world-rect2 w) (world-paused? w) mx my))

(define (drag-two w mx my)
  (make-world (world-rect1 w) (move-rect-two w mx my) (world-paused? w) mx my))

;move-rect-one: World NonNegInt NonNegInt -> World
;move-rect-two: World NonNegInt NonNegInt -> World
;GIVEN: a world state, x coordinate of mouse, y coordinate of mouse
;RETURNS:a rectangle withs its coordinate updated according to the mouse movement
;Example: (move-rect-one (make-world (make-rect-info 100 200 10 20 true) (make-rect-info 100 200 10 20 true)
;                               190 200 false) 100 210) -> (make-rect-info 110 210 10 20 true)
;Strategy: using template of world and rectangle
(define (move-rect-one w mx my)
  (make-rect-info (+ (rect-info-x (world-rect1 w)) (- mx (world-mousex w)))
                  (+ (rect-info-y (world-rect1 w)) (- my (world-mousey w)))
                  (rect-info-vx (world-rect1 w)) (rect-info-vy (world-rect1 w))
                  (rect-info-selected? (world-rect1 w)) (rect-info-rx (world-rect1 w))
                  (rect-info-ry (world-rect1 w))))

(define (move-rect-two w mx my)
  (make-rect-info (+ (rect-info-x (world-rect2 w)) (- mx (world-mousex w)))
                  (+ (rect-info-y (world-rect2 w)) (- my (world-mousey w)))
                  (rect-info-vx (world-rect2 w)) (rect-info-vy (world-rect2 w))
                  (rect-info-selected? (world-rect2 w)) (rect-info-rx (world-rect2 w))
                  (rect-info-ry (world-rect2 w))))


;if-inside: rectangle NonNegInt NonNegInt -> boolean
;if-inside-both: rectangle NonNegInt NonNegInt -> boolean
;GIVEN: rectangle, x coordinate of mouse, y coordinate of mouse
;RETURNS: boolean value representing if mouse is positioned inside rectangle(s)
;Example: (if-inside (make-rect-info 100 200 10 10 false) 110 220) -> true
;Strategy: using template of rectangle
(define (if-inside r mousex mousey) 
  (and (and (< mousex (+ (rect-info-x r) HALF-WIDTH)) (> mousex (- (rect-info-x r) HALF-WIDTH)))
       (and (< mousey (+ (rect-info-y r) HALF-LEN)) (> mousey (- (rect-info-y r) HALF-LEN)))))

(define (if-inside-both r1 r2 mousex mousey)
  (and (if-inside r1 mousex mousey) (if-inside r2 mousex mousey)))


;select-both: World NonNegInt NonNegInt -> World
;select-rect-1 World NonNegInt NonNegInt -> World
;select-rect-2 World NonNegInt NonNegInt -> World
;GIVEN: world state, x coordinate of mouse, y coordinate of mouse
;RETURNS: a World that follows the given one, with rectangle(s) selected and mouse position updated
;Example: (select-rect-1 (make-world (make-rect-info 100 200 10 20 false) (make-rect-info 100 200 10 20 true)
;                               190 200 false) 10 21) 110 220)
;         -> (make-world (make-rect-info 100 200 10 20 true) (make-rect-info 100 200 10 20 true)
;                               190 200 false) 10 21) 110 220)
;Strategy: combining simpler functions; using template of world
(define (select-both w mx my)
  (make-world (update-relative-pos (check-rect (world-rect1 w)) mx my)
              (update-relative-pos (check-rect (world-rect2 w)) mx my)
              (world-paused? w) mx my))

(define (select-rect-1 w mx my)
  (make-world (update-relative-pos (check-rect (world-rect1 w)) mx my)
              (world-rect2 w) (world-paused? w) mx my))

(define (select-rect-2 w mx my)
  (make-world (world-rect1 w)
              (update-relative-pos (check-rect (world-rect2 w)) mx my)
              (world-paused? w) mx my))



  ;update-relative-pos: rect-info NonNegInt NonNegInt -> rect-info
  ;GIVEN: a rectangle, x-coordinate of mouse, y-coordinate of mouse
  ;RETURNS: a new rectangle in which the relative position is updated
  ;Example: (update-relative-position (make-rect-info 100 200 10 20 false 50 -30) 300 250)
  ;         -> (make-rect-info 100 200 10 20 false 100 50)
  ;Strategy: using template of rectangle
  (define (update-relative-pos rect mx my)
    (make-rect-info (rect-info-x rect) (rect-info-y rect)
                    (rect-info-vx rect) (rect-info-vy rect) (rect-info-selected? rect)
                    (- (rect-info-x rect) mx) (- (rect-info-y rect) my)))
  
;if-both-selected: rectangle rectangle -> boolean
;if-selected: rectangle -> boolean
;GIVEN: one or two rectangle
;RETURNS: a boolean representing if one or both rectangles are selected
;Example: (if-both-selected (make-rect-info 100 200 1 2 true) -> true
;Strategy: using template of rectangle
(define (if-both-selected r1 r2)
  (and (rect-info-selected? r1) (rect-info-selected? r2)))

(define (if-selected r)
  (rect-info-selected? r))

;check-rect: rectangle -> rectangle
;uncheck-rect: rectangle -> rectangle
;GIVEN: a rectangle
;RETURNS: a same rectangle with selected? checked or unchecked
;Example: (check-rect (make-rect-info 100 200 1 2 false)) -> (make-rect-info 10 20 1 2 true)
;Strategy: using template of rectangle
(define (check-rect r)
  (make-rect-info (rect-info-x r) (rect-info-y r) (rect-info-vx r) (rect-info-vy r) true
                  (rect-info-rx r) (rect-info-ry r)))

(define (uncheck-rect r)
  (make-rect-info (rect-info-x r) (rect-info-y r) (rect-info-vx r) (rect-info-vy r) false
                  (rect-info-rx r) (rect-info-ry r)))

;; rect-after-mouse-event :  Rectangle Int Int MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event
;; Example: (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20) 100 200 "button-down")
;           -> (make-rect-info 100 200 -10 -20 true 0 0)
;  Strategy: dividing into cases; combining simpler functions
(define (rect-after-mouse-event r mx my me)
  (cond
    [(mouse=? "button-up" me) (case-up r mx my)]
    [(mouse=? "button-down" me) (case-down r mx my)]
    [(mouse=? "drag" me) (case-drag r mx my)]
    [else r]))

(begin-for-test
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20) 100 200 "button-down")
                (make-rect-info 100 200 -10 -20 true 0 0))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 true 10 20) 1 2 "button-down")
                (make-rect-info 100 200 -10 -20 true 10 20))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 true 10 20) 100 200 "button-up")
                (make-rect-info 100 200 -10 -20 false 10 20))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20) 100 200 "button-up")
                (make-rect-info 100 200 -10 -20 false 10 20))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20) 100 200 "drag")
                (make-rect-info 100 200 -10 -20 false 10 20))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 true 10 20) 100 200 "drag")
                (make-rect-info 110 220 -10 -20 true 10 20))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20) 100 200 "enter")
                (make-rect-info 100 200 -10 -20 false 10 20)))

;case-up: rectangle NonNegInt NonNegInt -> rectangle
;case-down: rectangle NonNegInt NonNegInt -> rectangle
;GIVEN: a rectangle, x coordinate of mouse, y coordinate of mouse
;RETURNS: a rectangle following the given one, with its selected? attribute
;         updated according to mouse position
;Example: (case-up (make-rect-info 100 200 10 20 true) 35 45) -> (make-rect-info 100 200 10 20 false)
(define (case-up r mx my)
  (if (if-selected r) (uncheck-rect r) r))

(define (case-down r mx my)
  (if (if-inside r mx my) (update-relative-pos (check-rect r) mx my)
      r))

(define (case-drag r mx my)
  (cond
    [(if-selected r)
     (make-rect-info (+ (rect-info-x r) (rect-movement-x r mx))
                     (+ (rect-info-y r) (rect-movement-y r my))
                     (rect-info-vx r) (rect-info-vy r)
                     (rect-info-selected? r)
                     (rect-info-rx r) (rect-info-ry r))]
    [else r]))


;rect-movement-x: rect-info NonNegInt -> Int
;;rect-movement-y: rect-info NonNegInt -> Int
;GIVEN: a rectangle, x coordinate of mouse
;RETURNS: distance the rectangle need to move along x/y direction
;Example: (rect-movement-x (make-rect-info 100 200 10 20 true 20 10) 50) -> 30
(define (rect-movement-x r mx)
  (- (rect-info-rx r) (- (rect-info-x r) mx)))

(define (rect-movement-y r my)
  (- (rect-info-ry r) (- (rect-info-y r) my)))

;; rect-selected? : Rectangle -> Boolean
;; GIVEN: rectangle
;; RETURNS: true iff the given rectangle is selected.
;; Example: (rect-selected? (make-rect-info 100 200 10 20 false)) -> false
(define (rect-selected? r)
  (rect-info-selected? r))

(begin-for-test
  (check-equal? (rect-selected? (make-rect-info 100 200 10 20 false 10 20)) false)
  (check-equal? (rect-selected? (make-rect-info 100 200 10 20 true 10 20)) true))

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Example: (new-rectangle 100 200 10 20) -> (new-rectangle 100 200 10 20 false)
(define (new-rectangle n1 n2 i1 i2)
  (make-rect-info n1 n2 i1 i2 false 0 0))

(begin-for-test
  (check-equal? (new-rectangle 100 200 10 20) (make-rect-info 100 200 10 20 false 0 0)))