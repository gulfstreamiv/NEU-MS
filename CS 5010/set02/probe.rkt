;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

(provide probe-at)
(provide probe-turned-left)
(provide probe-turned-right)
(provide probe-forward)
(provide probe-north?)
(provide probe-south?)
(provide probe-east?)
(provide probe-west?)

;Constants definition
;NORTH, SOUTH, EAST, WEST defines four directions
(define NORTH "north")
(define SOUTH "south")
(define EAST "east")
(define WEST "west")

;WALL_EAST, WALL_WEST define the wall position on the east and west side, with respect to x-coordinate
;WALL_SOUTH, WALL_NORTH define the wall position on the south and north side, with respect to y-coordinate
(define WALL_EAST 173.5)
(define WALL_WEST -173.5)
(define WALL_SOUTH 173.5)
(define WALL_NORTH -173.5)
(define DIAMETER 40)
(define RADIUS 20)

;Struct definition
;probe: a probe object created from (make-probe x-coord y-coord direction)
;INTERPRETATION: in the following definition, x, an integer, means the x-coordiate of the probe;
;y, also an integer, means the y-coordinate of the probe;
;x,y together define the probe's position.
;facing, a string value which is one of NORTH, EAST, SOUTH, WEST, defines the direction of the probe
(define-struct probe (x y facing))

; TEMPLATE
; probe-fn: probe -> ???
(define (probe-fn p)
  (... (probe-x p) (probe-y p) (probe-facing p)))

;probe-at: Integer Integer -> Probe
;GIVEN: an x-coordinate and a y-coordinate
;WHERE: these coordinates leave the robot entirely inside the trap
;RETURNS: a probe with its center at those coordinates, facing north.
;EXAMPLE: a set of coordinates that put the probe in contact with the
;wall is not consistent with the contract.  Note that this means that
;the behavior of probe-at in this situation is unspecified; you don't
;need to check for this.
(define (probe-at x y)
  (make-probe x y NORTH))

;Testcase
(begin-for-test
  (check-equal? (probe-at 10 10) (make-probe 10 10 NORTH) "probe is wrongly positioned"))

;probe-turned-left : Probe -> Probe
;probe-turned-right : Probe -> Probe
;GIVEN: a probe
;RETURNS: a probe like the original, but turned 90 degrees either left
;or right.
(define (probe-turned-left p)
  (cond
    [(string=? (probe-facing p) NORTH) (make-probe (probe-x p) (probe-y p) WEST)]
    [(string=? (probe-facing p) SOUTH) (make-probe (probe-x p) (probe-y p) EAST)]
    [(string=? (probe-facing p) EAST) (make-probe (probe-x p) (probe-y p) NORTH)]
    [(string=? (probe-facing p) WEST) (make-probe (probe-x p) (probe-y p) SOUTH)]))

(define (probe-turned-right p)
  (cond
    [(string=? (probe-facing p) NORTH) (make-probe (probe-x p) (probe-y p) EAST)]
    [(string=? (probe-facing p) SOUTH) (make-probe (probe-x p) (probe-y p) WEST)]
    [(string=? (probe-facing p) EAST) (make-probe (probe-x p) (probe-y p) SOUTH)]
    [(string=? (probe-facing p) WEST) (make-probe (probe-x p) (probe-y p) NORTH)]))

;Testcases
(begin-for-test
  (check-equal? (probe-turned-left (make-probe 0 0 NORTH)) (make-probe 0 0 WEST) "Heading wrong direction")
  (check-equal? (probe-turned-left (make-probe 0 0 SOUTH)) (make-probe 0 0 EAST) "Heading wrong direction")
  (check-equal? (probe-turned-left (make-probe 0 0 EAST)) (make-probe 0 0 NORTH) "Heading wrong direction")
  (check-equal? (probe-turned-left (make-probe 0 0 WEST)) (make-probe 0 0 SOUTH) "Heading wrong direction")
  )

(begin-for-test
  (check-equal? (probe-turned-right (make-probe 0 0 NORTH)) (make-probe 0 0 EAST) "Heading wrong direction")
  (check-equal? (probe-turned-right (make-probe 0 0 SOUTH)) (make-probe 0 0 WEST) "Heading wrong direction")
  (check-equal? (probe-turned-right (make-probe 0 0 EAST)) (make-probe 0 0 SOUTH) "Heading wrong direction")
  (check-equal? (probe-turned-right (make-probe 0 0 WEST)) (make-probe 0 0 NORTH) "Heading wrong direction")
  )


;probe-forward : Probe PosInt -> Probe
;GIVEN: a probe and a distance
;RETURNS: a probe like the given one, but moved forward by the
;specified distance.  If moving forward the specified distance would
;cause the probe to hit any wall of the trap, then the probe should 
;move as far as it can inside the trap, and then stop.
(define (probe-forward p len)
  (cond
    [(string=? (probe-facing p) NORTH) (move-north p len)]
    [(string=? (probe-facing p) SOUTH) (move-south p len)]
    [(string=? (probe-facing p) EAST) (move-east p len)]
    [(string=? (probe-facing p) WEST) (move-west p len)]
)) 

;move-north: probe NonNegInteger -> probe
;GIVEN: a probe and a non-negative integer
;RETURNS: a probe with the latest position after the move
;Example: (move-north (make-probe 0 0 WEST) 10) -> (make-probe -10 0 WEST)
;Following four functions are similar in format and functionality
(define (move-north p len)
  (cond
    [ (> (- (probe-y p) len) (+ WALL_NORTH RADIUS)) (make-probe (probe-x p) (- (probe-y p) len) NORTH)]
    [else (make-probe (probe-x p) (+ -173 RADIUS) NORTH)]))

;move-southern: probe NonNegInteger -> probe
;GIVEN: a probe and a non-negative integer
;RETURNS: a probe with the latest position after the move
(define (move-south p len)
  (cond
    [ (< (+ (probe-y p) len) (- WALL_SOUTH RADIUS)) (make-probe (probe-x p) (+ (probe-y p) len) SOUTH)]
    [else (make-probe (probe-x p) (- 173 RADIUS) SOUTH)]))

;move-east: probe NonNegInteger -> probe
;GIVEN: a probe and a non-negative integer
;RETURNS: a probe with the latest position after the move
(define (move-east p len)
  (cond
    [(< (+ (probe-x p) len) (- WALL_EAST RADIUS)) (make-probe (+ (probe-x p) len) (probe-y p) EAST)]
    [else (make-probe (- 173 RADIUS) (probe-y p) EAST)]))

;move-west: probe NonNegInteger -> probe
;GIVEN: a probe and a non-negative integer
;RETURNS: a probe with the latest position after the move
(define (move-west p len)
  (cond
    [(> (- (probe-x p) len) (+ WALL_WEST RADIUS)) (make-probe (- (probe-x p) len) (probe-y p) WEST)]
    [else (make-probe (+ -173 RADIUS) (probe-y p) WEST)]))

;Testcases
(begin-for-test
  (check-equal? (probe-forward (make-probe 0 0 NORTH) 10) 
                (make-probe 0 -10 NORTH)
                "Wrong position after move")
  (check-equal? (probe-forward (make-probe 0 0 SOUTH) 100) 
                (make-probe 0 100 SOUTH)
                "Wrong position after move")
  (check-equal? (probe-forward (make-probe 0 10 EAST) 50) 
                (make-probe 50 10 EAST)
                "Wrong position after move")
  (check-equal? (probe-forward (make-probe 0 10 WEST) 40) 
                (make-probe -40 10 WEST)
                "Wrong position after move")
  (check-equal? (probe-forward (make-probe 120 10 NORTH) 180) 
                (make-probe 120 -153 NORTH)
                "Probe hits the northern wall")
  (check-equal? (probe-forward (make-probe 120 10 SOUTH) 180) 
                (make-probe 120 153 SOUTH)
                "Probe hits the southern wall")
  (check-equal? (probe-forward (make-probe 120 10 EAST) 50) 
                (make-probe 153 10 EAST)
                "Probe hits the eastern wall")
  (check-equal? (probe-forward (make-probe 20 10 WEST) 180) 
                (make-probe -153 10 WEST)
                "Probe hits the western wall")
  (check-equal? (probe-forward (make-probe 20 10 SOUTH) 280) 
                (make-probe 20 153 SOUTH)
                "Probe cannot get through southern wall")
    (check-equal? (probe-forward (make-probe 20 -22 EAST) 280) 
                (make-probe 153 -22 EAST)
                "Probe cannot get through southern wall"))

;probe-north? : Probe -> Boolean
;probe-south? : Probe -> Boolean
;probe-east? : Probe -> Boolean
;probe-west? : Probe -> Boolean
;GIVEN: a probe
;ANSWERS: whether the probe is facing in the specified direction.
;Example: (probe-north? 
(define (probe-north? p)
  (cond
    [(string=? (probe-facing p) NORTH) #true]
    [else #false]))

;Testcases
(begin-for-test
  (check-equal? (probe-north? (make-probe 100 100 NORTH)) #true "probe not facing north")
  (check-equal? (probe-north? (make-probe 100 100 SOUTH)) #false "probe not facing north"))

(define (probe-south? p)
  (cond
    [(string=? (probe-facing p) SOUTH) #true]
    [else #false]))

(begin-for-test
  (check-equal? (probe-south? (make-probe 100 100 SOUTH)) #true "probe not facing north")
  (check-equal? (probe-south? (make-probe 100 100 NORTH)) #false "probe not facing north"))

(define (probe-east? p)
  (cond
    [(string=? (probe-facing p) EAST) #true]
    [else #false]))

(begin-for-test
  (check-equal? (probe-east? (make-probe 100 100 EAST)) #true "probe not facing north")
  (check-equal? (probe-east? (make-probe 100 100 SOUTH)) #false "probe not facing north"))

(define (probe-west? p)
  (cond
    [(string=? (probe-facing p) WEST) #true]
    [else #false]))

(begin-for-test
  (check-equal? (probe-west? (make-probe 100 100 WEST)) #true "probe not facing north")
  (check-equal? (probe-west? (make-probe 100 100 SOUTH)) #false "probe not facing north"))