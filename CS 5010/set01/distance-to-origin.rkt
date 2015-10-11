;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname distance-to-origin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide distance-to-origin)

;distance-to-origin : Number Number -> Number
;GIVEN: two integers representing the location of a point in 2D plane
;RETURNS: an integer representing the distance from origin to the given point

;Example: (distance-to-origin 3 4) ===> 5
;         (distance-to-origin 0 9) ===> 9
;Strategy: Pythagorean theory

;Function definition and implementation:
(define (distance-to-origin x y)
  (sqrt (+ (* x x) (* y y))))

;Testcases:
(begin-for-test
  (check-equal? (distance-to-origin 3 4) 5 "Wrong answer")
  (check-equal? (distance-to-origin 0 9) 9 "Wrong answer")
  (check-equal? (distance-to-origin 6 8) 10 "Wrong answer"))