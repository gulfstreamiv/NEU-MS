;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-rects
         world-paused?
         rect-x
         rect-y
         rect-vx
         rect-vy)

(provide world-after-mouse-event
         rect-after-mouse-event
         rect-after-key-event
         rect-selected?
         new-rectangle
         rect-pen-down?)


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

(define IMAGE-RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue"))
(define IMAGE-RED-RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "red"))
(define DRAW-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define IMAGE-DOT (circle 1 "solid" "black"))

(define PAUSE " ")

;Data definition
;world: (make-world Rect-info Rect-info Boolean NonNegInt NonNegInt List)
;INTERPRETATION: (make rec1 rec2 ifpaused x-coord y-coord listofdots), in which
; rec1 is a rectangle object inside the world
; rec2 is another rectangle object inside the world
; ifpaused is a boolean value indicating if the world is paused
; x-coord is an NonNegInteger representing the x coordinate of the mouse
; y-coord is an NonNegInteger representing the y coordinate of the mouse
; listofdots is a list containing all the dots object 
(define-struct world (rects paused? mousex mousey dots))

;Template
(define (world-fn w)
  (... (world-rects w) (world-paused? w) (world-mx w) (world-my w)
       (world-dots w)))


;rect-info: (make-rect-info NonNegInt NonNegInt Int Int Boolean Boolean)
;INTERPRETATION: (make-rect-info x y vx vy select dot?), in which
;x is a NonNegInteger representing the x coordinate of the rectangle
;y is a NonNegInteger representing the y coordinate of the rectangle
;vx is is an Integer representing the velocity on the x direction of the rectangle
;vy is is an Integer representing the velocity on the y direction of the rectangle
;select is a boolean value representing if the rectangle is selected by mouse
;dot? is a boolean value representing if the rectangle is drawing dots
(define-struct rect-info (x y vx vy selected? rx ry dot?))

;Template
(define (rect-info-fn rect)
  (... (rect-info-x rect) (rect-info-y rect) (rect-info-vx rect) (rect-info-vy rect)
       (rect-info-selected? rect) (rect-info-rx rect) (rect-info-ry rect)))

;;dot: (make-dot NonNegInt NonNegInt)
;;INTERP: (make-dot x y), in which
;;        x is the x coordinate of the dot
;;        y is the y coordinate of dot
(define-struct dot (x y))

;;template
(define (dot-fn d)
  (... (dot-x d) (dot-y d)))

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
  (make-world (list) true 0 0 (list)))

(begin-for-test
 (check-equal? (initial-world 1)
               (make-world (list) true 0 0 (list))))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; Example: (world-after-tick (make-world (make-rect-info 200 100 -12 20 false) (make-rect-info 200 200 23 -14 false) false 0 0))
;;        -> (make-world (make-rect-info 188 120 -12 20 false) (make-rect-info 223 186 23 -14 false) false 0 0)
;;Strategy: dividing into cases; using template of world struct and rectangle struct; using simpler functions
(define (world-after-tick w)
  (cond
    [(or (world-paused? w) (empty? (world-rects w))) w]
    [else (make-world (rects-after-tick-recursive (world-rects w))
                      (world-paused? w) (world-mousex w) (world-mousey w) (add-dots w (world-rects w)))]))

;;rect-after-tick-recursive
;;GIVEN: a list of rectangles
;;RETURNS: a list of rectangles, in which each rectangle's state is updated
;;Strategy: dividing into cases; recursion
(define (rects-after-tick-recursive rects)
  (cond
    [(empty? rects) rects]
    [(if-selected (first rects)) (cons (first rects) (rects-after-tick-recursive (rest rects)))]
    [else (cons (rect-after-tick (first rects)) (rects-after-tick-recursive (rest rects)))]))

(begin-for-test
  (check-equal? (world-after-tick (make-world (list (make-rect-info 200 100 -12 20 false 10 20 false)
                                                    (make-rect-info 200 200 23 -14 false 10 20 false)) true 0 0 (list)))
                (make-world (list (make-rect-info 200 100 -12 20 false 10 20 false)
                                  (make-rect-info 200 200 23 -14 false 10 20 false)) true 0 0 (list))  "should return same world")
  (check-equal? (world-after-tick (make-world (list (make-rect-info 200 100 -12 20 false 10 20 false)
                                                    (make-rect-info 200 200 23 -14 false 10 20 false)) false 0 0 (list)))
                (make-world (list (make-rect-info 188 120 -12 20 false 10 20 false)
                                  (make-rect-info 223 186 23 -14 false 10 20 false)) false 0 0 (list)) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (list (make-rect-info 200 100 -12 20 true 10 20 false)
                                                    (make-rect-info 200 200 23 -14 true 10 20 false)) false 0 0 (list)))
                (make-world (list (make-rect-info 200 100 -12 20 true 10 20 false)
                                  (make-rect-info 200 200 23 -14 true 10 20 false)) false 0 0 (list)) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (list (make-rect-info 200 100 -12 20 true 10 20 false)
                                                    (make-rect-info 200 200 23 -14 false 10 20 false)) false 0 0 (list)))
                (make-world (list (make-rect-info 200 100 -12 20 true 10 20 false)
                                  (make-rect-info 223 186 23 -14 false 10 20 false)) false 0 0 (list)) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (list (make-rect-info 200 100 -12 20 false 10 20 false)
                                                    (make-rect-info 200 200 23 -14 true 10 20 false)) false 0 0 (list)))
                (make-world (list (make-rect-info 188 120 -12 20 false 10 20 false)
                                  (make-rect-info 200 200 23 -14 true 10 20 false)) false 0 0 (list)) "Wrong coordinate update")
  (check-equal? (world-after-tick (make-world (list (make-rect-info 200 100 -12 20 false 10 20 false)
                                                    (make-rect-info 200 200 23 -14 true 10 20 false)) true 0 0 (list)))
                (make-world (list (make-rect-info 200 100 -12 20 false 10 20 false)
                                  (make-rect-info 200 200 23 -14 true 10 20 false)) true 0 0 (list)) "Wrong coordinate update"))                



;;adding-dots: World ListofRect -> ListofDots
;;GIVEN: World and List of Dots
;;RETURNS: a list of dots, in which new dots are added
;;Strategy: dividing into cases
(define (add-dots w rects)
  (cond
    [(empty? rects) (world-dots w)]
    [(and (if-dot-enabled (first rects)) (not (if-selected (first rects))))
     (cons (create-dot (first rects)) (add-dots w (rest rects)))]
    [else (add-dots w (rest rects))]))

(begin-for-test
  (check-equal? (add-dots WORLD-4 (world-rects WORLD-4)) (add-dots WORLD-4 (world-rects WORLD-4)))
)

;;if-dot-enabled: Rect-info -> Boolean
;;GIVEN: rectangle
;;RETURNS: value of dot? attribute
(define (if-dot-enabled r)
  (rect-info-dot? r))

;;create-dot: Rect-info -> dot
;;GIVEN: rectangle
;;RETURNS: a dot object located at the center of the rectangle
(define (create-dot r)
  (make-dot (rect-info-x r) (rect-info-y r)))               

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

;;For testing 
(define (MAKE_RECT x y vx vy)
  (rect-after-tick (make-rect-info x y vx vy false 10 20 false)))

(begin-for-test
  (check-equal? (rect-after-tick (MAKE_RECT 100 120 -10 15)) (MAKE_RECT 90 135 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 30 25 -10 -15)) (MAKE_RECT 40 40 10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 30 275 -10 15)) (MAKE_RECT 40 260 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 370 25 10 -15)) (MAKE_RECT 360 40 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 370 275 10 15)) (MAKE_RECT 360 260 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 30 225 -10 -15)) (MAKE_RECT 40 210 10 -15)"Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 370 225 10 -15)) (MAKE_RECT 360 210 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 320 25 -10 -15)) (MAKE_RECT 310 40 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 320 275 -10 15)) (MAKE_RECT 310 260 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 31 26 -10 -15)) (MAKE_RECT 30 25 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 31 274 -10 15)) (MAKE_RECT 30 275 -10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 369 26 10 -15)) (MAKE_RECT 370 25 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 369 274 10 15)) (MAKE_RECT 370 275 10 15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 31 225 -10 -15)) (MAKE_RECT 30 210 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 369 225 10 -15)) (MAKE_RECT 370 210 10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 300 26 -10 -15)) (MAKE_RECT 290 25 -10 -15) "Wrong rect movement")
  (check-equal? (rect-after-tick (MAKE_RECT 300 274 -10 15)) (MAKE_RECT 290 275 -10 15) "Wrong rect movement"))


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
     (make-rect-info RIGHT-MOST-POS LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r)
                     (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))]
    [(and (> (+ (rect-info-x r) (rect-info-vx r)) RIGHT-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS))
     (make-rect-info RIGHT-MOST-POS UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r)
                     (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))]
    [(and (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS) (> (+ (rect-info-y r) (rect-info-vy r)) LOWER-MOST-POS))
     (make-rect-info LEFT-MOST-POS LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r)
                     (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))]
    [(and (< (+ (rect-info-x r) (rect-info-vx r)) LEFT-MOST-POS) (< (+ (rect-info-y r) (rect-info-vy r)) UPPER-MOST-POS))
     (make-rect-info LEFT-MOST-POS UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r)
                     (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))]))

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
      (make-rect-info RIGHT-MOST-POS (+ (rect-info-y r) (rect-info-vy r)) (rect-info-vx r) (rect-info-vy r)
                      (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))
      (make-rect-info LEFT-MOST-POS (+ (rect-info-y r) (rect-info-vy r)) (rect-info-vx r) (rect-info-vy r)
                      (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))))

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
      (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) LOWER-MOST-POS (rect-info-vx r) (rect-info-vy r)
                      (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))
      (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) UPPER-MOST-POS (rect-info-vx r) (rect-info-vy r)
                      (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))))

;perfect-bounce rectangle -> rectangle
;GIVEN: a rectangle tangential to two walls
;RETURNS: a rectangle that follows the given one after the tick, with its x-velocity and y-velocity reversed,
;         and x-position and y-position bounced back by a distance equal to its corresponding velocity
;Example: (perfect-bounce (make-rect-info 30 25 -10 -15 false)) -> (make-rect-info 40 40 10 15 false)
;Strategy: using template of rectangle; 
(define (perfect-bounce r)
  (make-rect-info (- (rect-info-x r) (rect-info-vx r)) (- (rect-info-y r) (rect-info-vy r))
                  (- 0 (rect-info-vx r)) (- 0 (rect-info-vy r)) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))

;reverse-x: rectangle -> rectangle
;GIVEN: a rectangle tangential to left/right wall
;RETURNS: a rectangle that follows the given one after the tick, with its x-velocity reversed,
;         and x-position bounced back by a distance equal to the velocity
;Example: (reverse-x (make-rect-info 30 250 -10 15 false)) -> (make-rect-info 40 265 10 15 false)
;Strategy: dividing into cases; using template of rectangle; 
(define (reverse-x r)
  (make-rect-info (- (rect-info-x r) (rect-info-vx r)) (+ (rect-info-y r) (rect-info-vy r))
                  (- 0 (rect-info-vx r)) (rect-info-vy r) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))

;reverse-y: rectangle -> rectangle
;GIVEN: a rectangle tangential to lower/upper wall
;RETURNS: a rectangle that follows the given one after the tick, with its y-velocity reversed,
;         and y-position bounced back by a distance equal to the velocity
;Example: (reverse-y (make-rect-info 35 275 10 15 false)) -> (make-rect-info 40 260 10 -15 false)
;Strategy: dividing into cases; using template of rectangle; 
(define (reverse-y r)
  (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) (- (rect-info-y r) (rect-info-vy r))
                          (rect-info-vx r) (- 0 (rect-info-vy r))
                          (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))

;normal-move: rectangle -> rectangle
;GIVEN: a rectangle within the canvas
;RETURNS: a rectangle that follows the given rectangle after the tick, moving a distance in both directions
;         indicated by its velocity
;Example: (normal-move (make-rect-info 35 40 10 10 false) -> (make-rect-info 45 50 10 10 false)
;Strategy: using template of rectangle; structural decomposition
(define (normal-move r)
  (make-rect-info (+ (rect-info-x r) (rect-info-vx r)) (+ (rect-info-y r) (rect-info-vy r))
                  (rect-info-vx r) (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r) (rect-info-ry r)
                  (rect-info-dot? r)))



;rect-location-red-image: Rect-info -> image
;rect-location-blue-image: Rect-info -> image
;INTERP: Draw the location information in red/blue color inside the rectangle 1, located at the center of rectangle 1
;GIVEN: a world state
;RETURNS: an text image representing the location info of rectangle 1
;Example: (rect-location-red-image (make-rect-info 100 100 20 20 false 10 20 false)) -> image
;Strategy: combining simpler functions; using template of rectangle
(define (rect-location-red-image r)
  (text (string-append "(" (number->string (rect-info-vx  r)) ", "
                       (number->string (rect-info-vy r)) ")")
        12 "red"))

(define (rect-location-blue-image r)
  (text (string-append "(" (number->string (rect-info-vx  r)) ", "
                       (number->string (rect-info-vy r)) ")")
        12 "blue"))

;;testcase
(define RECT-FOR-TEST (make-rect-info 50 50 10 10 false 10 20 false))
(begin-for-test
  (check-equal? (rect-location-red-image RECT-FOR-TEST)
                (text (string-append "(" (number->string (rect-info-vx  RECT-FOR-TEST)) ", "
                                     (number->string (rect-info-vy RECT-FOR-TEST)) ")")
                      12 "red"))
  (check-equal? (rect-location-blue-image RECT-FOR-TEST)
                (text (string-append "(" (number->string (rect-info-vx  RECT-FOR-TEST)) ", "
                                     (number->string (rect-info-vy RECT-FOR-TEST)) ")")
                      12 "blue")))

;draw-single-red-rect: Rect-info -> image
;draw-single-blue-rect: Rect-info -> image
;INTERP: draw a colored rectangle on canvas
;GIVEN: rectangle
;RETURNS: image containing rectangle (with color), and canvas.
;Exmaple: (draw-single-red-rect (make-rect-info 120 200 30 -15 false 10 20 false)) -> image
;Strategy: using template of rectangle;

(define (draw-single-red-rect w r)
  (place-image IMAGE-RED-RECTANGLE (rect-info-x r) (rect-info-y r) (draw-dots (world-dots w))))

(define (draw-single-blue-rect w r)
  (place-image IMAGE-RECTANGLE (rect-info-x r) (rect-info-y r) (draw-dots (world-dots w))))

;;testcases
(define RECT-FOR-TEST-2 (make-rect-info 120 200 30 -15 false 0 0 false))
(define THIS-WORLD (make-world (list RECT-FOR-TEST-2) false  100 200 (list)))
(define THAT-WORLD (make-world (list RECT-FOR-TEST-2) false  100 200
                               (list (make-dot 200 240))))
(define DOTS-LIST (list (make-dot 200 100) (make-dot 230 120) (make-dot 200 150)))

(begin-for-test
  (check-equal? (draw-single-red-rect THIS-WORLD RECT-FOR-TEST-2)
                (place-image IMAGE-RED-RECTANGLE
                             (rect-info-x RECT-FOR-TEST-2) (rect-info-y RECT-FOR-TEST-2)
                             DRAW-CANVAS))
  (check-equal? (draw-single-blue-rect THIS-WORLD RECT-FOR-TEST-2)
                (place-image IMAGE-RECTANGLE
                             (rect-info-x RECT-FOR-TEST-2) (rect-info-y RECT-FOR-TEST-2)
                             DRAW-CANVAS))
  (check-equal? (draw-dots (list))
                DRAW-CANVAS)
  (check-equal? (draw-dots DOTS-LIST)
                (place-image IMAGE-DOT (dot-x (first DOTS-LIST)) (dot-y (first DOTS-LIST))
                             (draw-dots (rest DOTS-LIST)))))


;;draw-dots: ListofDots -> image
;;GIVEN: List of dots
;;RETURNS: image containing dots drawn on canvas
;;Strategy: dividing into cases; combining simpler functions
(define (draw-dots list-of-dots)
  (cond
    [(empty? list-of-dots) DRAW-CANVAS]
    [else (place-image IMAGE-DOT (dot-x (first list-of-dots)) (dot-y (first list-of-dots))
                       (draw-dots (rest list-of-dots)))]))


;;recursive draw: ListofRectangle -> image
;;GIVEN: a list of rectangles in the world
;;RETURNS: an image in which rectangles and their corresponding location info
;;         are drawn inside the canvas
;;Strategy: dividing into cases
(define (recursive-draw-world w rects)
  (cond
    [(and (= (length rects) 1) (if-selected (first rects)))
     (place-image (rect-location-red-image (first rects)) (rect-info-x (first rects)) (rect-info-y (first rects))
                  (draw-single-red-rect w (first rects)))]

    [(and (= (length rects) 1) (not (if-selected (first rects))))
     (place-image (rect-location-blue-image (first rects)) (rect-info-x (first rects)) (rect-info-y (first rects))
                  (draw-single-blue-rect w (first rects)))]    

    [(and (> (length rects) 1) (if-selected (first rects)))
     (place-image (rect-location-red-image (first rects)) (rect-info-x (first rects)) (rect-info-y (first rects))
                  (place-image IMAGE-RED-RECTANGLE (rect-info-x (first rects)) (rect-info-y (first rects))
                               (recursive-draw-world w (rest rects))))]

    [(and (> (length rects) 1) (not (if-selected (first rects))))
     (place-image (rect-location-blue-image (first rects)) (rect-info-x (first rects)) (rect-info-y (first rects))
                  (place-image IMAGE-RECTANGLE (rect-info-x (first rects)) (rect-info-y (first rects))
                               (recursive-draw-world w (rest rects))))] 

    [else (draw-dots (world-dots w))])) 

  
;draw-cursor-on-world: world -> image
;INTERP: all three functions draw a red circle inside the rectangle(s) that has been selected by mouse
;GIVEN: a world state
;RETURN: an image in which a red circle has been drawn inside rectangle(s) which is drawn on the canvas
;Example: (draw-cursor-on-world (make-world (make-rect-info 200 230 true 19 18 false)
;                                           (make-rect-info 40 230 false 19 20 false)
;                                               false 200 220 (list))) -> image
;Strategy: using template of world
(define (draw-cursor-on-world w)
  (place-image (circle 5 "outline" "red") (world-mousex w) (world-mousey w)
               (recursive-draw-world w (world-rects w))))


;draw-real-world: World -> image
;GIVEN: world state
;RETURNS: an image containing every object inside, including red circle, rectangles, location info, canvas.
;Example: (draw-real-world (make-world (list (make-rect-info 200 230 true 19 18 false) (make-rect-info 40 230 false 19 20 false))
;                                               false 200 220 (list))) -> image
;Strategy: dividing into cases; combining simpler functions
(define (draw-real-world w)
  (cond
    [(if-one-selected (world-rects w)) (draw-cursor-on-world w)]
    [else (recursive-draw-world w (world-rects w))]))

;;if-one-selected: ListofRects -> Boolean
;;GIVEN: list of rectangles
;;RETURNS: true if at least one rectangle is selected, false otherwise
;;Strategy: divide into cases
(define (if-one-selected rects)
  (cond
    [(empty? rects) false]
    [else (or (if-selected (first rects)) (if-one-selected (rest rects)))]))

(define WORLD-1 (make-world (list (make-rect-info 200 230 19 18 true 10 20 false) (make-rect-info 40 230 19 20 true 10 20 false)) false 200 220
                            (list (make-dot 100 200))))
(define WORLD-2 (make-world (list (make-rect-info 200 230 19 18 true 10 20 true) (make-rect-info 40 230 19 20 false 10 20 false)
                                  (make-rect-info 290 230 19 18 false 10 20 true)) false 200 220 (list)))
(define WORLD-3 (make-world (list) false 200 220 (list)))
(define WORLD-4 (make-world (list (make-rect-info 200 230 19 18 false 10 20 true) (make-rect-info 40 230 19 20 false 10 20 false)) false 200 220
                            (list (make-dot 100 200))))

(begin-for-test
  (check-equal? (draw-real-world WORLD-1)
                (draw-cursor-on-world WORLD-1)
                "inconsistent image")
  (check-equal? (draw-real-world WORLD-2)
                (draw-cursor-on-world WORLD-2)
                "inconsistent image")
  (check-equal? (draw-real-world WORLD-3)
                (recursive-draw-world WORLD-3 (world-rects WORLD-3))
                "inconsistent image")
  (check-equal? (draw-real-world WORLD-4)
                (recursive-draw-world WORLD-4 (world-rects WORLD-4))
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
  (cond
    [(key=? ke PAUSE) (toggle-pause w)]
    [(key=? ke "n") (add-rect w)]
    [(if-valid-rect-key-event ke) (handle-key-event w ke)]
    [else w]))

;;testcases
(define WORLD-04 (make-world (list (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)) false 50 100 (list)))
(define WORLD-05 (make-world (list (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)) true 50 100 (list)))
(define WORLD-6 (make-world (list (make-rect-info 200 150 0 0 false 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false )
                                  ) false 50 100 (list)))
(define WORLD-7 (make-world (list (make-rect-info 200 150 0 0 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)
                                  ) false 50 100 (list)))
(define WORLD-8 (make-world (list (make-rect-info 200 150 0 -2 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)
                                  ) false 50 100 (list)))
(define WORLD-9 (make-world (list (make-rect-info 200 150 0 2 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)
                                  ) false 50 100 (list)))
(define WORLD-10 (make-world (list (make-rect-info 200 150 2 0 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)
                                  ) false 50 100 (list)))
(define WORLD-11 (make-world (list (make-rect-info 200 150 -2 0 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false) (make-rect-info 105 201 2 3 false 10 20 false)
                                  ) false 50 100 (list)))
(define WORLD-12 (make-world (list (make-rect-info 34 28 -2 0 false 0 0 false) (make-rect-info 200 150 -2 0 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false)
                                   (make-rect-info 105 201 2 3 false 10 20 false)) false 50 100 (list)))
(define WORLD-13 (make-world (list (make-rect-info 34 28 -2 0 false 0 0 false) (make-rect-info 200 150 -2 2 true 0 0 false) (make-rect-info 100 200 1 2 false 10 20 false)
                                   (make-rect-info 105 201 2 3 false 10 20 false)) false 50 100 (list)))

(begin-for-test
  (check-equal? (world-after-key-event WORLD-04 " ")
                WORLD-05 "world should pause")
  (check-equal? (world-after-key-event WORLD-04 "a")
                WORLD-04 "world should not change")
  (check-equal? (world-after-key-event WORLD-04 "n")
                WORLD-6 "world should not change")
  (check-equal? (world-after-key-event WORLD-7 "up")
                WORLD-8 "world should not change")
  (check-equal? (world-after-key-event WORLD-7 "down")
                WORLD-9 "world should not change")
  (check-equal? (world-after-key-event WORLD-7 "right")
                WORLD-10 "world should not change")
  (check-equal? (world-after-key-event WORLD-7 "left")
                WORLD-11 "world should not change")
  (check-equal? (world-after-key-event WORLD-12 "down")
                WORLD-13 "world should not change"))

;;toggle-pause: World -> World
;;GIVEN: a world
;;RETURNS: a world in which the paused? attribute is reversed
;;Strategy: using template of world
(define (toggle-pause w)
  (make-world (world-rects w) (not (world-paused? w)) (world-mousex w) (world-mousey w) (world-dots w)))

;;if-speed-key: Keyevent -> Boolean
;;GIVEN: Keyevent
;;RETURNS: true if keyevent is one of add-speed keys
;;Strategy: combining simpler functions
(define (if-valid-rect-key-event ke)
  (or (key=? ke "up") (key=? ke "left") (key=? ke "right") (key=? ke "down") (key=? ke "d") (key=? ke "u")))

;;add-rect: World -> World
;;GIVEN: a world
;;RETURNS: a world, with one rectangle added into rectangle list
;;Strategy: using template of world
(define (add-rect w)
  (make-world (add-rect-into-list (world-rects w)) (world-paused? w)
              (world-mousex w) (world-mousey w) (world-dots w)))

;;add-rect-into-list: ListofRectangle -> ListofRectangle
;;GIVEN: a list of rectangles
;;RETURNS: a list, with one rectangle added into rectangle list
(define (add-rect-into-list rects)
  (cons (make-rect-info 200 150 0 0 false 0 0 false) rects))

;;handle-key-event World Keyevent -> World
;;GIVEN: a world and keyevent
;;RETURNS: a new world in which all objects' states are updated,
;;         some new object (dots) may also be created
;;Strategy: using template of world
(define (handle-key-event w ke)
  (make-world (iterate-rects (world-rects w) ke) (world-paused? w)
              (world-mousex w) (world-mousey w) (world-dots w)))

;;iterate-rects: ListofRects Keyevent -> ListofRects
;;GIVEN: a list of rectangles, a keyevent
;;RETURNS: a list of rectangles, in which each rectangle's status is updated
;;Strategy: using helper function
(define (iterate-rects rects ke)
  (cond
    [(empty? rects) rects]
    [else (cons (rect-after-key-event (first rects) ke) (iterate-rects (rest rects) ke))] ))


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
  (check-equal? (rect-x (make-rect-info 100 200 10 20 false 10 20 false)) 100)
  (check-equal? (rect-y (make-rect-info 100 200 10 20 false 10 20 false)) 200)
  (check-equal? (rect-vx (make-rect-info 100 200 10 20 false 10 20 false)) 10)
  (check-equal? (rect-vy (make-rect-info 100 200 10 20 false 10 20 false)) 20))

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

;;testcases
(define WORLD-15 (make-world (list (make-rect-info 25 20 -3 3 true 10 20 false) (make-rect-info 10 10 -5 -1 false 10 20 false)) false 10 20 (list)))
(define WORLD-16 (make-world (list (make-rect-info 225 220 -6 6 false 10 20 false) (make-rect-info 25 20 -3 3 true 10 20 false)
                                   (make-rect-info 10 10 -5 -1 false 10 20 false)) false 10 20 (list)))

(begin-for-test
  (check-equal? (world-after-mouse-event WORLD-15 30 30 "button-up")
                (mouse-up WORLD-15 30 30))
  (check-equal? (world-after-mouse-event WORLD-15 30 30 "button-down")
                (mouse-down WORLD-15 30 30))
  (check-equal? (world-after-mouse-event WORLD-15 30 30 "drag")
                (mouse-drag WORLD-15 30 30))
  (check-equal? (world-after-mouse-event WORLD-16 30 30 "button-down")
                (mouse-down WORLD-16 30 30))
  (check-equal? (world-after-mouse-event WORLD-15 30 30 "enter")
                WORLD-15))
  
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
  (make-world (unselect-rects-recursive (world-rects world)) (world-paused? world) mx my (world-dots world)))


;;unselect-rects-recursive: ListofRects -> ListofRects
;;GIVEN: a list of rectangles
;;RETURNS: a list of rectangles, in which each rectangle is unchecked
;;Strategy: dividing into cases
(define (unselect-rects-recursive rects)
  (cond
    [(empty? rects) rects]
    [else (cons (uncheck-rect (first rects)) (unselect-rects-recursive (rest rects)))]))


;mouse-down: World NonNegInt NonNegInt-> World
;INTERP: given a world and mouse position, this function returns a world in which the rectangles' attributes are updated according
;        to the mouse event, in this case the "button down" scenario
;GIVEN: a world state, x coordinate of mouse, y coordinate of mouse
;RETURNS: a world state that follows the given one, with mouse positions updated and rectangle's
;         attributes updated
;Example: (mouse-down (make-world (make-rect-info 100 200 false 10 20 false) (make-rect-info 100 200 false 10 20 false)
;                               200 250 false) 100 210 (list)) -> (make-world (make-rect-info 100 200 true 10 20 false)
;                                                         (make-rect-info 100 200 false 10 20 false)
;                               100 210 true (list))
;Strategy: dividing into cases; using template of world; combining simpler functions
(define (mouse-down world mx my)
  (make-world (select-rects-recursive (world-rects world) mx my)
              (world-paused? world) mx my (world-dots world)))


;;select-rects-recursive: ListofRects -> ListofRects
;;GIVEN: a list of rectangles
;;RETURNS: a list of rectangles, in which each rectangle is unchecked
;;Strategy: dividing into cases
(define (select-rects-recursive rects mx my)
  (cond
    [(empty? rects) rects]
    [(if-inside (first rects) mx my) (cons (case-down (first rects) mx my) (select-rects-recursive (rest rects) mx my))]
    [else (cons (first rects) (select-rects-recursive (rest rects) mx my))]))


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
  (make-world (mouse-drag-recursive (world-rects w) mx my) (world-paused? w) mx my (world-dots w))) 

;mouse-drag-recursive: ListofRect -> ListofRect
;GIVEN: a list of rectangles
;RETURNS: a list of rectangles, in which each rectangle is dragged accordingly
;Strategy: dividing into cases
(define (mouse-drag-recursive rects mx my)
  (cond
    [(empty? rects) rects]
    [else (cons (case-drag (first rects) mx my) (mouse-drag-recursive (rest rects) mx my))]))


;if-inside: rectangle NonNegInt NonNegInt -> boolean
;GIVEN: rectangle, x coordinate of mouse, y coordinate of mouse
;RETURNS: boolean value representing if mouse is positioned inside rectangle(s)
;Example: (if-inside (make-rect-info 100 200 false 10 10 false) 110 220) -> true
;Strategy: using template of rectangle
(define (if-inside r mousex mousey) 
  (and (and (< mousex (+ (rect-info-x r) HALF-WIDTH)) (> mousex (- (rect-info-x r) HALF-WIDTH)))
       (and (< mousey (+ (rect-info-y r) HALF-LEN)) (> mousey (- (rect-info-y r) HALF-LEN)))))


;update-relative-pos: rect-info NonNegInt NonNegInt -> rect-info
;GIVEN: a rectangle, x-coordinate of mouse, y-coordinate of mouse
;RETURNS: a new rectangle in which the relative position is updated
;Example: (update-relative-position (make-rect-info 100 200 10 20 false 50 -30) 300 250)
;         -> (make-rect-info 100 200 10 20 false 100 50)
;Strategy: using template of rectangle
(define (update-relative-pos rect mx my)
  (make-rect-info (rect-info-x rect) (rect-info-y rect)
                  (rect-info-vx rect) (rect-info-vy rect) (rect-info-selected? rect)
                  (- (rect-info-x rect) mx) (- (rect-info-y rect) my) (rect-info-dot? rect)))
  

;if-selected: rectangle -> boolean
;GIVEN: one or two rectangle
;RETURNS: a boolean representing if one or both rectangles are selected
;Example: (if-selected (make-rect-info 100 200 1 2 true) -> true
;Strategy: using template of rectangle
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
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))

(define (uncheck-rect r)
  (make-rect-info (rect-info-x r) (rect-info-y r) (rect-info-vx r) (rect-info-vy r) false
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))

;; rect-after-key-event : Rect-info KeyEvent -> Rect-info
;; GIVEN: Rectangle and keyevent
;; RETURNS: the state of the rectangle that should follow the given
;; rectangle after the given key event
;; Strategy: dividing into cases
(define (rect-after-key-event r ke)
  (cond
    [(and (key=? "up" ke) (rect-info-selected? r)) (add-speed-up r)]
    [(and (key=? "down" ke) (rect-info-selected? r)) (add-speed-down r)]
    [(and (key=? "left" ke) (rect-info-selected? r)) (add-speed-left r)]
    [(and (key=? "right" ke) (rect-info-selected? r)) (add-speed-right r)]
    [(and (key=? "d" ke) (rect-info-selected? r)) (enable-dot r)]
    [(and (key=? "u" ke) (rect-info-selected? r)) (cancel-dot r)]
    [else r]))

(begin-for-test
  (check-equal? (rect-after-key-event (make-rect-info 10 10 100 200 true 0 0 false) "d")
                (make-rect-info 10 10 100 200 true 0 0 true))
  (check-equal? (rect-after-key-event (make-rect-info 10 10 100 200 true 0 0 true) "u")
                (make-rect-info 10 10 100 200 true 0 0 false)))                                      

;add-speed-up: Rect-info -> Rect-info
;add-speed-down: Rect-info -> Rect-info
;add-speed-left: Rect-info -> Rect-info
;add-speed-right: Rect-info -> Rect-info
;GIVEN: a rectangle
;RETURNS: a rectangle with speed added in corresponsing direction
;Strategy: using template of rectangle
(define (add-speed-up r)
  (make-rect-info (rect-info-x r) (rect-info-y r)
                  (rect-info-vx r) (- (rect-info-vy r) 2) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))
(define (add-speed-down r)
  (make-rect-info (rect-info-x r) (rect-info-y r)
                  (rect-info-vx r) (+ (rect-info-vy r) 2) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))
(define (add-speed-left r)
  (make-rect-info (rect-info-x r) (rect-info-y r)
                  (- (rect-info-vx r) 2) (rect-info-vy r) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))
(define (add-speed-right r)
  (make-rect-info (rect-info-x r) (rect-info-y r)
                  (+ (rect-info-vx r) 2) (rect-info-vy r) (rect-info-selected? r)
                  (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r)))

;enable-dot: Rect-info -> Rect-info
;cancel-dot: Rect-info -> Rect-info
;GIVEN: a rectangle
;RETURNS: a new rectangle with dot? enabled/disabled
;Strategy: using template of rectangle
(define (enable-dot r)
  (make-rect-info (rect-info-x r) (rect-info-y r) (rect-info-vx r)
                  (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r)
                  (rect-info-ry r) true))

(define (cancel-dot r)
  (make-rect-info (rect-info-x r) (rect-info-y r) (rect-info-vx r)
                  (rect-info-vy r) (rect-info-selected? r) (rect-info-rx r)
                  (rect-info-ry r) false))


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
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20 false) 100 200 "button-down")
                (make-rect-info 100 200 -10 -20 true 0 0 false))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 true 10 20 false) 1 2 "button-down")
                (make-rect-info 100 200 -10 -20 true 10 20 false))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 true 10 20 false) 100 200 "button-up")
                (make-rect-info 100 200 -10 -20 false 10 20 false))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20 false) 100 200 "button-up")
                (make-rect-info 100 200 -10 -20 false 10 20 false))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20 false) 100 200 "drag")
                (make-rect-info 100 200 -10 -20 false 10 20 false)) 
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 true 10 20 false) 100 200 "drag")
                (make-rect-info 110 220 -10 -20 true 10 20 false))
  (check-equal? (rect-after-mouse-event (make-rect-info 100 200 -10 -20 false 10 20 false) 100 200 "enter")
                (make-rect-info 100 200 -10 -20 false 10 20 false)))

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
                     (rect-info-rx r) (rect-info-ry r) (rect-info-dot? r))]
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
  (check-equal? (rect-selected? (make-rect-info 100 200 10 20 false 10 20 false)) false)
  (check-equal? (rect-selected? (make-rect-info 100 200 10 20 true 10 20 false)) true))

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; Example: (new-rectangle 100 200 10 20) -> (new-rectangle 100 200 10 20 false)
(define (new-rectangle n1 n2 i1 i2)
  (make-rect-info n1 n2 i1 i2 false 0 0 false))

(begin-for-test
  (check-equal? (new-rectangle 100 200 10 20) (make-rect-info 100 200 10 20 false 0 0 false)))

;;rect-pen-down?: Rect-info -> Boolean
;;GIVEN: a rectangle
;;RETURNS: true if dot? is enabled; false otherwise
(define (rect-pen-down? r)
  (rect-info-dot? r))

(begin-for-test
  (check-equal? (rect-pen-down? (make-rect-info 100 200 10 20 false 0 0 false))
                false))