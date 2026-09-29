;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A02, Problem 4
;; ***************************************************
;;


;
; Question 4 (a)
;

;;A Blood-Type is (anyof 'O- 'O+ 'A- 'A+ 'B- 'B+ 'AB- 'AB+)

;;(can-donate-to/cond?) produces boolean value for whether blood tranfusion can occur given doner and recipient blood type.
;;can-donate-to/cond?: Blood-Type -> Bool
(define (can-donate-to/cond? doner recipient)
  (cond
    [(symbol=? doner 'O-) true]
    [(symbol=? recipient 'AB+) true]
    [(symbol=? doner recipient) true]
    [(symbol=? doner 'O+)
         (cond
           [(symbol=? recipient 'A+) true]
           [(symbol=? recipient 'B+) true]
           [else false])]
    [(symbol=? doner 'B-)
         (cond
           [(symbol=? recipient 'B+) true]
           [(symbol=? recipient 'AB-) true]
           [else false])]
    [(symbol=? doner 'A-)
         (cond
           [(symbol=? recipient 'A+) true]
           [(symbol=? recipient 'AB-) true]
           (else false))]
    [else false]))

;; Testing:
(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'AB+ 'B-) false)
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'A- 'AB-) true)
(check-expect (can-donate-to/cond? 'A- 'O+) false)
(check-expect (can-donate-to/cond? 'B+ 'B-) false)
(check-expect (can-donate-to/cond? 'O+ 'AB-) false)
(check-expect (can-donate-to/cond? 'B- 'O-) false)


;
; Question 4 (b)
;

;;A Blood-Type is (anyof 'O- 'O+ 'A- 'A+ 'B- 'B+ 'AB- 'AB+)

;;(can-donate-to/bool?) produces boolean value for whether blood tranfusion can occur given doner and recipient blood type.
;;can-donate-to/bool?: Blood-Type -> Bool
(define (can-donate-to/bool? doner recipient)
  (or
       [symbol=? doner 'O-]
       [symbol=? recipient 'AB+]
       [symbol=? doner recipient]
       [and (symbol=? recipient 'AB-) (or (symbol=? doner 'A-) (symbol=? doner 'B-))]
       [and (symbol=? recipient 'B+) (or (symbol=? doner 'B-) (symbol=? doner 'O+))]
       [and (symbol=? recipient 'A+) (or (symbol=? doner 'A-) (symbol=? doner 'O+))]))

;; Testing:
(check-expect (can-donate-to/bool? 'AB+ 'O-) false)
(check-expect (can-donate-to/bool? 'AB+ 'O+) false)
(check-expect (can-donate-to/bool? 'AB+ 'A-) false)
(check-expect (can-donate-to/bool? 'AB+ 'A+) false)
(check-expect (can-donate-to/bool? 'AB+ 'B-) false)
(check-expect (can-donate-to/bool? 'AB+ 'B+) false)
(check-expect (can-donate-to/bool? 'AB+ 'AB-) false)
(check-expect (can-donate-to/bool? 'AB- 'O-) false)
(check-expect (can-donate-to/bool? 'AB- 'O+) false)
(check-expect (can-donate-to/bool? 'AB- 'A-) false)
(check-expect (can-donate-to/bool? 'AB- 'A+) false)
(check-expect (can-donate-to/bool? 'AB- 'B-) false)
(check-expect (can-donate-to/bool? 'AB- 'B+) false)
(check-expect (can-donate-to/bool? 'B+ 'O-) false)
(check-expect (can-donate-to/bool? 'B+ 'O+) false)
(check-expect (can-donate-to/bool? 'B+ 'A-) false)
(check-expect (can-donate-to/bool? 'B+ 'A+) false)
(check-expect (can-donate-to/bool? 'B+ 'B-) false)
(check-expect (can-donate-to/bool? 'B+ 'B+) true)
(check-expect (can-donate-to/bool? 'B+ 'AB-) false)
(check-expect (can-donate-to/bool? 'B- 'O-) false)
(check-expect (can-donate-to/bool? 'B- 'O+) false)
(check-expect (can-donate-to/bool? 'B- 'A-) false)
(check-expect (can-donate-to/bool? 'B- 'A+) false)
(check-expect (can-donate-to/bool? 'B- 'B+) true)
(check-expect (can-donate-to/bool? 'B- 'AB-) true)
(check-expect (can-donate-to/bool? 'A+ 'O-) false)
(check-expect (can-donate-to/bool? 'A+ 'O+) false)
(check-expect (can-donate-to/bool? 'A+ 'A-) false)
(check-expect (can-donate-to/bool? 'A+ 'B-) false)
(check-expect (can-donate-to/bool? 'A+ 'B+) false)
(check-expect (can-donate-to/bool? 'A+ 'AB-) false)
(check-expect (can-donate-to/bool? 'A- 'O-) false)
(check-expect (can-donate-to/bool? 'A- 'O+) false)
(check-expect (can-donate-to/bool? 'A- 'A+) true)
(check-expect (can-donate-to/bool? 'A- 'B-) false)
(check-expect (can-donate-to/bool? 'A- 'B+) false)
(check-expect (can-donate-to/bool? 'A- 'AB-) true)
(check-expect (can-donate-to/bool? 'A- 'AB+) true)
(check-expect (can-donate-to/bool? 'O+ 'O-) false)
(check-expect (can-donate-to/bool? 'O+ 'O+) true)
(check-expect (can-donate-to/bool? 'O+ 'A-) false)
(check-expect (can-donate-to/bool? 'O+ 'A+) true)
(check-expect (can-donate-to/bool? 'O+ 'B-) false)
(check-expect (can-donate-to/bool? 'O+ 'B+) true)
(check-expect (can-donate-to/bool? 'O+ 'AB-) false)
(check-expect (can-donate-to/bool? 'O- 'B-) true)