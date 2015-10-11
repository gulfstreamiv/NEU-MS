;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)

;;Data definition
;;slip: (make-slip Color String String)
;;INTERP: (make-slip color s1 s2) in which
;;        color is the color of the slip,
;;        s1 is the first name of the student
;;        s2 is the last name of the student
(define-struct slip (color name1 name2))

;A Color is one of
;-- "yellow"
;-- "blue"

;Template
(define (slip-fn s)
  (... (slip-color s) (slip-name1 s) (slip-name2 s)))


;felleisen-roster : ListOfSlip -> ListOfSlip
;GIVEN: a list of slips
;RETURNS: a list of slips containing all the students in Professor
;Felleisen's class, without duplication.
(define (felleisen-roster slips)
 (get-f-roster slips (list)))

;;get-f-roster: ListofSlips ListofSlips -> ListofSlips
;;GIVEN: two lists of slips 
;;RETURNS: a list of slips containing felleisen's roster
;;Strategy: dividing into cases; combining simpler functions
(define (get-f-roster slips result)
  (cond
    [(empty? slips) result]
    [(if-color-not-yellow (first slips)) (get-f-roster (rest slips) result)]
    [(if-name-already-in-list (first slips) result)
     (get-f-roster (rest slips) result)]
    [else (get-f-roster (rest slips) (cons (first slips) result))]))

;;if-color-not-yellow: Slip -> Boolean
;;GIVEN: slip
;;RETURNS: true if slip's color is not yellow; false otherwise
(define (if-color-not-yellow slip)
  (not (string=? (slip-color slip) "yellow")))

;;if-name-already-in-list: Slip ListofSlips -> Boolean
;;GIVEN: slip and list of slips
;;RETURNS: true if slip's student name in either order is in the list of slips
(define (if-name-already-in-list slip result)
  (or (member slip result)
      (member (reverse-name-order slip) result)))

;;reverse-name-order: Slip -> Slip
;;GIVEN: a slip
;;RETURNS: a slip with name order reversed
(define (reverse-name-order slip)
  (make-slip (slip-color slip) (slip-name2 slip) (slip-name1 slip)))

;shivers-roster: ListOfSlip -> ListOfSlip
;GIVEN: a list of slips
;RETURNS: a list of slips containing all the students in Professor
;Shivers' class, without duplication.
(define (shivers-roster slips)
 (get-s-roster slips (list)))

;;get-s-roster: ListofSlips ListofSlips -> ListofSlips
;;GIVEN: two lists of slips 
;;RETURNS: a list of slips containing shivers's roster
;;Strategy: dividing into cases; combining simpler functions
(define (get-s-roster slips result)
  (cond
    [(empty? slips) result]
    [(if-color-not-blue (first slips)) (get-s-roster (rest slips) result)]
    [(if-name-already-in-list (first slips) result)
     (get-s-roster (rest slips) result)]
    [else (get-s-roster (rest slips) (cons (first slips) result))]))

;;if-color-not-blue: Slip -> Boolean
;;GIVEN: slip
;;RETURNS: true if slip's color is not blue; false otherwise
(define (if-color-not-blue slip)
  (not (string=? (slip-color slip) "blue")))


;;testing
(define TEST-PILE
  (cons (make-slip "yellow" "Wang" "Xi")
        (cons (make-slip "yellow" "Xi" "Wang")
              (cons (make-slip "blue" "Gu" "Anson")
                    (cons (make-slip "yellow" "Zhang" "Yi")
                          (cons (make-slip "yellow" "Zhou" "Li")
                                (cons (make-slip "blue" "Xiong" "Dailin")
                                      (cons (make-slip "yellow" "Yi" "Zhang")
                                            (cons (make-slip "blue" "Xiang" "Zheng")
                                                  (cons (make-slip "blue" "Dailin" "Xiong")
                                                        (cons (make-slip "yellow" "Lv" "San")
                                                              (cons (make-slip "blue" "Tian" "Zhen") empty))))))))))))

(begin-for-test
  (check-equal? (felleisen-roster TEST-PILE)
                (list (make-slip "yellow" "Lv" "San") (make-slip "yellow" "Zhou" "Li") (make-slip "yellow" "Zhang" "Yi") (make-slip "yellow" "Wang" "Xi")))
  (check-equal? (shivers-roster TEST-PILE)
                (list (make-slip "blue" "Tian" "Zhen") (make-slip "blue" "Xiang" "Zheng") (make-slip "blue" "Xiong" "Dailin") (make-slip "blue" "Gu" "Anson"))))


        