;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide initial-machine)
(provide machine-next-state)
(provide machine-output)
(provide machine-remaining-coffee)
(provide machine-remaining-chocolate)
(provide machine-bank)

;A CustomerInput is one of
;-- a PosInt          interp: insert the specified amount of money, in cents
;-- "coffee"          interp: request a coffee
;-- "hot chocolate"   interp: request a hot chocolate
;-- "change"          interp: return all the unspent money that the
;                             customer has inserted
;Template:
;cusomterInput-fn: CustomerInput -> ???
; (define (customerInput-fn CustomerInput)
;    (cond
;         [(integer? CustomerInput) ...]
;         [(string=?  CustomerInput "coffee") ...]
;         [(string=?  CustomerInput "hot chocolate") ...]
;         [string=?  CustomerInput "change") ...]))


;A MachineOutput is one of
;-- "coffee"         interp: machine dispenses a cup of coffee
;-- "hot chocolate"  interp: machine dispenses a cup of hot chocolate
;-- "Out of Item"    interp: machine displays "Out of Item"
;-- a PosInt         interp: machine releases the specified amount of
;                           money, in cents
;-- "Nothing"        interp: the machine does nothing
;Template:
; (define (machineOutput-fn MachineOutput)
;    (cond [(string=? MachineOutput "coffee") ...]
;          [(string=? MachineOutput "hot chocolate") ...]
;          [(string=? MachineOutput "Out of Item") ...]
;          [(integer? MachineOutput) ...]))

;Constants definitino
(define COFFEE-PRICE 150)
(define CHOCOLATE-PRICE 60)

;Data definition
;MachineState represents a machine state, which can be created from (make-MachineState NonNegInt NonNegInt NonNegIntNonNegInt)
(define-struct MachineState (coffee chocolate bank curbalance))

;Template:
;MachineState-fn: MachineState -> ???
(define (MachineState-fn ms)
  (... (MachineState-coffee ms) (MachineState-chocolate ms)
       (MachineState-bank ms) (MachineState-curbalance ms)))


;initial-machine : NonNegInt NonNegInt -> MachineState
;GIVEN: a number of cups of coffee and of hot chocolate
;RETURNS: the state of a machine loaded with the given number of cups
;         of coffee and of hot chocolate, with an empty bank.
(define (initial-machine num1 num2)
  (make-MachineState num1 num2 0 0))

;Testcase
(begin-for-test
  (check-equal? (initial-machine 5 10) (make-MachineState 5 10 0 0) "Error while initialize"))

;machine-next-state : MachineState CustomerInput -> MachineState
;GIVEN: a machine state and a customer input
;RETURNS: the state of the machine that should follow the customer's
;input
(define (machine-next-state MachineState CustomerInput)
  (cond
    [(and (integer? CustomerInput) (>= CustomerInput 0)) (input-coin MachineState CustomerInput)]
    [(string=? CustomerInput "coffee") (get-coffee MachineState)]
    [(string=? CustomerInput "hot chocolate") (get-chocolate MachineState)]
    [(string=? CustomerInput "change") (get-change MachineState)]))

;Testcases
(begin-for-test
  (check-equal? (machine-next-state (make-MachineState 5 10 0 0) 200) 
    (make-MachineState 5 10 0 200) "machine fails to count coins correctly")
  (check-equal? (machine-next-state (make-MachineState 5 10 230 155) "coffee") 
    (make-MachineState 4 10 380 5) "fail to process coffee request") 
  (check-equal? (machine-next-state (make-MachineState 5 10 300 130) "coffee") 
    (make-MachineState 5 10 300 130) "should have done nothing")
  (check-equal? (machine-next-state (make-MachineState 0 10 300 150) "coffee") 
    (make-MachineState 0 10 300 150) "should display out of item")
  (check-equal? (machine-next-state (make-MachineState 10 12 230 155) "hot chocolate") 
    (make-MachineState 10 11 290 95) "fail to process chocolate request")
  (check-equal? (machine-next-state (make-MachineState 10 12 230 55) "hot chocolate") 
    (make-MachineState 10 12 230 55) "should have done nothing")
  (check-equal? (machine-next-state (make-MachineState 10 0 300 150) "hot chocolate") 
    (make-MachineState 10 0 300 150) "should display out of item")
  (check-equal? (machine-next-state (make-MachineState 10 12 400 150) "change") 
    (make-MachineState 10 12 400 0) "fail to return change")
  )

;input-coin: MachineState NonNegInt -> MachineState
;GIVEN: a machine state, an non-negative integer representing the value of coins inserted by customer
;RETURNS: a new machine state after coins are inserted
;Example: (input-coin (make-MachineState 1 1 200 100) 100) -> (make-MachineState 1 1 200 200)
(define (input-coin ms input)
  (make-MachineState (MachineState-coffee ms) (MachineState-chocolate ms)
                     (MachineState-bank ms) (+ (MachineState-curbalance ms) input)))
;get-coffee: MachineState -> MachineState
;GIVEN: a machine state
;RETURNS: a new machine state after coffee-request is processed
;Example: (get-coffee (make-MachineState 1 1 200 150)) -> (make-MachineState 0 1 350 0)
(define (get-coffee ms)
  (if (and (>= (MachineState-curbalance ms) COFFEE-PRICE) (> (MachineState-coffee ms) 0))
      (dispense-coffee ms)
       ms))

;dispense-coffee: MachineState -> MachineState
;GIVEN: a machine state
;RETURNS: a machine state after a coffee is dispensed to customer
;Example: (dispense-coffee (make-MachineState 1 1 200 150)) -> (make-MachineState 0 1 350 0)
(define (dispense-coffee ms)
  (make-MachineState (- (MachineState-coffee ms) 1) (MachineState-chocolate ms)
                     (+ (MachineState-bank ms)  COFFEE-PRICE) (- (MachineState-curbalance ms) COFFEE-PRICE)))

;get-chocolate: MachineState -> MachineState
;GIVEN: a machine state
;RETURNS: a new machine state after chocolate-request is processed
;Example: (get-chocolate (make-MachineState 1 1 200 150)) -> (make-MachineState 1 0 260 90)
(define (get-chocolate ms)
  (if (and (>= (MachineState-curbalance ms) CHOCOLATE-PRICE) (> (MachineState-chocolate ms) 0))
      (dispense-chocolate ms)
      ms))

;dispense-chocolate: MachineState -> MachineState
;GIVEN: a machine state
;RETURNS: a machine state after a coffee is dispensed to customer
;Example: (dispense-chocolate: (make-MachineState 1 1 200 150)) -> (make-MachineState 1 0 260 90)
(define (dispense-chocolate ms)
  (make-MachineState (MachineState-coffee ms) (- (MachineState-chocolate ms) 1)
                     (+ (MachineState-bank ms)  CHOCOLATE-PRICE)
                     (- (MachineState-curbalance ms) CHOCOLATE-PRICE)))

;get-change: MachineState -> MachineState
;GIVEN: a machine state
;RETURNS: a machine state after the machine returns left-over money owed to customer
;Example: (get-change (make-MachineState 1 1 200 150)) -> (make-MachineState 1 1 200 0)
(define (get-change ms)
  (make-MachineState (MachineState-coffee ms) (MachineState-chocolate ms)
                     (MachineState-bank ms) 0))

;machine-output : MachineState CustomerInput -> MachineOutput
;GIVEN: a machine state and a customer input
;RETURNS: a MachineOutput that describes the machine's response to the
;customer input
;Example: (machine-output (make-MachineState 1 1 200 150) "coffee") -> "coffee"
(define (machine-output ms input)
  (cond
    [ (string=? input "coffee") (output-coffee ms)]
    [ (string=? input "hot chocolate") (output-chocolate ms)]
    [ (string=? input "change") (MachineState-curbalance ms)]
    [else "Nothing"]))

;Testcases
(begin-for-test
  (check-equal? (machine-output (make-MachineState 5 10 230 155) "coffee") "coffee" "Error: should display coffee")
  (check-equal? (machine-output (make-MachineState 0 10 300 150) "coffee") "Out of Item" "Error: should display out of item")
  (check-equal? (machine-output (make-MachineState 1 10 300 140) "coffee") "Nothing" "Error: should display nothing")
  (check-equal? (machine-output (make-MachineState 10 12 230 155) "hot chocolate") "hot chocolate" "Error: should display hot chocolate")
  (check-equal? (machine-output (make-MachineState 10 0 300 150) "hot chocolate") "Out of Item" "Error: should display out of item")
  (check-equal? (machine-output (make-MachineState 10 3 300 50) "hot chocolate") "Nothing" "Error: should display nothing")
  (check-equal? (machine-output (make-MachineState 10 12 400 150) "change") 150)
  (check-equal? (machine-output (make-MachineState 10 12 400 150) "abc") "Nothing" "should display nothing"))

;output-coffee: MachineState -> MachineOutput
;GIVEN: a machine state
;RETURNS: a machine output representing the current status of the machine, after a coffee-request is processed
;Example: (output-coffee (make-MachineState 1 1 200 150)) -> "coffee"
(define (output-coffee ms)
  (cond
    [(and (> (MachineState-coffee ms) 0) (>= (MachineState-curbalance ms) COFFEE-PRICE)) "coffee" ]
    [(and (<= (MachineState-coffee ms) 0) (>= (MachineState-curbalance ms) COFFEE-PRICE)) "Out of Item"]
    [else "Nothing"]))

;output-chocolate: MachineState -> MachineOutput
;GIVEN: a machine state
;RETURNS: a machine output representing the current status of the machine, after a chocolate-request is processed
;Example: (output-chocolate (make-MachineState 1 1 200 150)) -> "hot chocolate"
(define (output-chocolate ms)
  (cond
    [(and (> (MachineState-chocolate ms) 0) (>= (MachineState-curbalance ms) CHOCOLATE-PRICE)) "hot chocolate" ]
    [(and (<= (MachineState-chocolate ms) 0) (>= (MachineState-curbalance ms) CHOCOLATE-PRICE)) "Out of Item"]
    [else "Nothing"]))


;machine-remaining-coffee : MachineState -> NonNegInt
;GIVEN: a machine state
;RETURNS: the number of cups of coffee left in the machine
;Example: (machine-remaining-coffee (make-MachineState 1 2 200 150)) -> 1
(define (machine-remaining-coffee ms)
  (MachineState-coffee ms))

(begin-for-test
  (check-equal? (machine-remaining-coffee (make-MachineState 10 0 300 150)) 10 "Error: wrong num of coffee")
  )

;machine-remaining-chocolate : MachineState -> NonNegInt
;GIVEN: a machine state
;RETURNS: the number of cups of hot chocolate left in the machine
;Example: (machine-remaining-chocolate (make-MachineState 1 2 200 150)) -> 2
(define (machine-remaining-chocolate ms)
  (MachineState-chocolate ms))

(begin-for-test
  (check-equal? (machine-remaining-chocolate (make-MachineState 10 20 3000 50)) 20 "Error: wrong num of chocolate")
  )

;machine-bank : MachineState -> NonNegInt
;GIVEN: a machine state
;RETURNS: the amount of money in the machine's bank, in cents
;Example: (machine-bank (make-MachineState 1 2 200 150)) -> 200
(define (machine-bank ms)
  (MachineState-bank ms))

(begin-for-test
  (check-equal? (machine-bank (make-MachineState 10 20 3000 50)) 3000 "Error: wrong num of bank savings")
  )