;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-delete) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide string-delete)

;string-insert : String Number -> String
;GIVEN: a string and a number
;RETURNS: a new string in which a the Number-th character in the old string was deleted

;Example: (string-delete "abc" 1) ===> "ac"
;         (string-delete "i am bolun" 4) ===> "i ambolun"
;Strategy: Combine simpler functions

;Function definition and implementation:
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ i 1))))

;Testcases:
(begin-for-test
  (check-equal? (string-delete "abc" 1) "ac" "Wrong answer")
  (check-equal? (string-delete "hello nihao" 5) "hellonihao" "Wrong answer"))