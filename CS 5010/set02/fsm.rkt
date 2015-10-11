;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide initial-state)
(provide next-state)
(provide accepting-state?)
(provide error-state?)

;Data definitions
;INTERPRETATION: a state is one of the following variables:
;The meaning of each state is included within the definition
(define INIT "I want any number of mixture of a and b, or one c")
(define EXPECT-ABD "I want any number of mixture of a and b, or one d")
(define FINISH "I'm done, but still could accept any number of mixture of e and f")
(define ERROR "Error! Unexpected format")

;initial-state : Number -> State
;GIVEN: a number
;RETURNS: a representation of the initial state
;of your machine.  The given number is ignored.
(define (initial-state num)
  INIT)

(begin-for-test
  (check-equal? (initial-state 11) INIT "Error: fail to initialize"))

;next-state : State MachineInput -> State
;GIVEN: a state of the machine and a machine input
;RETURNS: the state that should follow the given input.
;Example: (next-state INIT "c") -> EXPECT-ABD
;Strategy: Combine simpler functions
(define (next-state state input)
  (cond
     [(string=? state INIT) (we-need-abc input)]
     [(string=? state EXPECT-ABD) (we-need-abd input)]
     [(string=? state FINISH) (we-need-none-ef input)]))

;Testcases
(begin-for-test
  (check-equal? (next-state INIT "a") INIT "should stay at initial state")
  (check-equal? (next-state INIT "b") INIT "should stay at initial state")
  (check-equal? (next-state INIT "g") ERROR "should pop out error")
  (check-equal? (next-state INIT "c") EXPECT-ABD "should go to EXPECT-ABD state")
  (check-equal? (next-state EXPECT-ABD "a") EXPECT-ABD "should stay at EXPECT-ABD state")
  (check-equal? (next-state EXPECT-ABD "b") EXPECT-ABD "should stay at EXPECT-ABD state")
  (check-equal? (next-state EXPECT-ABD "d") FINISH "should go to FINISH state")
  (check-equal? (next-state EXPECT-ABD "er") ERROR "should pop out error")
  (check-equal? (next-state FINISH "e") FINISH "should stay at finish state")
  (check-equal? (next-state FINISH "f") FINISH "should stay at finish state")
  (check-equal? (next-state FINISH "a") ERROR "should go to error state")
  )

;we-need-abc: State -> State
;This function checks the given input, and directs to the corresponding next state 
;GIVEN: a machine input, like "a", "b", "c", etc.
;RETURNS: a state that corresponds to the given input
;Example: (we-need-abc "a") -> INIT
;Strategy: divide into cases based on the input
(define (we-need-abc input)
  (cond
    [(or (string=? input "a") (string=? input "b")) INIT]
    [(string=? input "c") EXPECT-ABD]
    [else ERROR]))

;we-need-abd State -> State
;This function checks the given input, and directs to the corresponding next state 
;GIVEN: a machine input, like "a", "b", "c", etc.
;RETURNS: a state that corresponds to the given input
;Example: (we-need-abd "d") -> FINISH
;Strategy: divide into cases based on the input
(define (we-need-abd input)
  (cond
    [(or (string=? input "a") (string=? input "b")) EXPECT-ABD]
    [(string=? input "d") FINISH]
    [else ERROR]))

;we-need-none-ef State -> State
;This function checks the given input, and directs to the corresponding next state 
;GIVEN: a machine input, like "a", "b", "c", etc.
;RETURNS: a state that corresponds to the given input
;Example: (we-need-none-ef "g") -> ERROR
;Strategy: divide into cases based on the input
(define (we-need-none-ef input)
  (cond
    [(or (string=? input "e") (string=? input "f")) FINISH]
    [else ERROR]))

;accepting-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff the given state is a final (accepting) state
;Example: (accepting-state? FINISH) -> #true
;Strategy: divide into cases
(define (accepting-state? state)
  (if (string=? state FINISH) #true #false))

;Testcase
(begin-for-test
  (check-equal? (accepting-state? FINISH) #true)
  (check-equal? (accepting-state? ERROR) #false))

;error-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff there is no path (empty or non-empty) from the given
;state to an accepting state
;Example: (error-state? ERROR) -> #true
(define (error-state? state)
  (if (string=? state ERROR) #true #false))

;Testcase
(begin-for-test
  (check-equal? (error-state? ERROR) #true)
  (check-equal? (error-state? EXPECT-ABD) #false))