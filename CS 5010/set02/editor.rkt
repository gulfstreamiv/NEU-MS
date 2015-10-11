;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)


(provide make-editor)
(provide editor-pre)
(provide editor-post)
(provide editor?)
(provide edit)
  
;Data definition
;editor:
;Interpretation: editor is consist of two strings: pre, which is the string before
;the cursor, and post, which is the string after the cursor
(define-struct editor (pre post))

;Template
;editor-fn takes in an editor object which can be created by
;function (make-editor string1 string2)
;Given: editor
;Returns: ???
(define (editor-fn editor)
  (... ((editor-pre editor) (editor-post editor))))


;edit: editor keyEvent -> editor
(define (edit ed ke)
  (cond
    [(key=? ke "\b") (delete-char ed)]
    [(key=? ke "left") (move-left ed)]
    [(key=? ke "right") (move-right ed)]
    [(if-ignore ke) ed]
    [else (add-char ed ke)]))

;Testcases
(begin-for-test
  (check-equal? (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld")
                "Error: cursor should move right")
  (check-equal? (edit (make-editor "hello" "") "right") (make-editor "hello" "")
                "Error: cursor should do nothing")
  (check-equal? (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld")
                "Error: cursor should move left")
  (check-equal? (edit (make-editor "" "world") "left") (make-editor "" "world")
                "Error: cursor should do nothing")
  (check-equal? (edit (make-editor "hello" "world") "\b") (make-editor "hell" "world")
                "Error: cursor should delete cursor's left char")
  (check-equal? (edit (make-editor "" "world") "\b") (make-editor "" "world")
                "Error: cursor should delete nothing")
  (check-equal? (edit (make-editor "hello" "world") "\r") (make-editor "hello" "world")
                "Error: cursor should do nothing")
  (check-equal? (edit (make-editor "hello" "world") "\t") (make-editor "hello" "world")
                "Error: cursor should do nothing")
  (check-equal? (edit (make-editor "hello" "world") "z") (make-editor "helloz" "world")
                "Error: cursor should move right"))

;if-ignore: Keyevent -> boolean
;GIVEN: a keyevent
;RETURNS: boolean value indicating if the keyevent should be ignored
;Example: (if-ignore "\r") -> true
(define (if-ignore ke)
  (cond
    [(string=? "\t" ke) #true]
    [(string=? "\r" ke) #true]
    [else #false]))

;delete-char
;GIVEN: an editor
;RETURNS: a new editor with its last char of the pre section deleted
;Example: (delete-char (make-editor "abc" "def")) -> (make-editor "ab" "def")
(define (delete-char ed)
  (cond
    [(<= (string-length (editor-pre ed)) 0) ed]
    [else (make-editor
           (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1))
           (editor-post ed))]))

;add-char
;GIVEN: an editor
;RETURNS: a new editor with a new char appended to the pre section of the old editor
;Example: (add-char (make-editor "abc" "def") "a") -> (make-editor "abca" "def")
(define (add-char ed ke)
  (make-editor
   (string-append (editor-pre ed) ke)
   (editor-post ed)))

;move-left
;GIVEN: an editor
;RETURNS: a new editor with its cursor moved to the left by 1 character 
;Example: (move-left (make-editor "abc" "def")) -> (make-editor "ab" "cdef")
(define (move-left ed)
  (cond
    [(<= (string-length (editor-pre ed)) 0) ed]
    [else
     (make-editor 
      (substring (editor-pre ed) 0 (- (string-length (editor-pre ed)) 1))
      (string-append (substring (editor-pre ed) (- (string-length (editor-pre ed)) 1))
                     (editor-post ed)))]))

;move-right
;GIVEN: an editor
;RETURNS: a new editor with its cursor moved to the right by 1 character 
;Example: (move-right (make-editor "abc" "def")) -> (make-editor "abcd" "ef")
(define (move-right ed)
  (cond
    [(<= (string-length (editor-post ed)) 0) ed]
    [else
     (make-editor
      (string-append (editor-pre ed) (substring (editor-post ed) 0 1))
      (substring (editor-post ed) 1))]))