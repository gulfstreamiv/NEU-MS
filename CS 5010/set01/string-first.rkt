;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-first) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide string-first)

;string-first : String -> String
;GIVEN: a non-empty string
;RETURNS: the first 1String of the given string

;Example: (string-first "abcdefg") ===> "a"
;         (distance-to-origin " hello") ===> " "
;Strategy: Use built-in function string-ith

;Function definition and implementation:
(define (string-first s)
  (string-ith s 0))

;Testcases:
(begin-for-test
  (check-equal? (string-first "abc") "a" "Wrong answer")
  (check-equal? (string-first " hello") " " "Wrong answer")
  (check-equal? (string-first ".nihao") "." "Wrong answer"))