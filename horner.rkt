;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname horner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A04, Problem 3
;; ***************************************************
;;

;;
;; Question 3
;;

;; Produces the result of evaluating a polynomial at the given value of x
;; eval-poly: (listof Num) Num -> Num
;; Requires list to contain the coefficients from lowest degree to highest degree.
(define (eval-poly lon x)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (* x (eval-poly (rest lon) x)))]))

;; Tests
(check-expect (eval-poly (cons -4 (cons 7 (cons 0 (cons 2 empty)))) 3) 71)
(check-expect (eval-poly (list -1 2 -6 2) 3) 5)
(check-expect (eval-poly empty 5) 0)
(check-expect (eval-poly (list 5) 3) 5)
(check-expect (eval-poly (list 3 2 1) 2) 11)

;; Citations:
;; https://www.youtube.com/watch?v=zEvfkSuPqWk