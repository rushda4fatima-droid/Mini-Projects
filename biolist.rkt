;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname biolist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A04, Problem 2
;; ***************************************************
;;

;;
;; Question 2 (a)
;;

;; A factor is a Nat
;; Produces the list of the next day's count of cells based on growth factor
;; next-day: factor (listof Nat) -> (listof Nat)
;; Requires factor to be strictly greater than 0
(define (next-day factor lon)
  (cond
    [(empty? lon) empty]
    [else (cons (+ 1 (* factor (first lon))) (next-day factor (rest lon)))]))

;; Tests:
(check-expect (next-day 2 (list 5 0 12))
(list 11 1 25))
(check-expect (next-day 3 empty) empty)
(check-expect (next-day 10 (list 0 4 2)) (list 1 41 21))

;;
;; Question 2 (b)
;;

;; A factor is a Nat
;; Produces a list of the count of cells based on growth factor on a specific day
;; after-days: factor (listof Nat) Nat -> (listof Nat)
;; Requires factor to be strictly greater than 0
(define (after-days factor lon days)
  (cond
    [(empty? lon) empty]
    [(zero? days) lon]
    [(= days 1) (next-day factor lon)]
    [else (after-days factor (next-day factor lon) (sub1 days))]))

;; Tests:
(check-expect (after-days 2 (list 5 0 12) 2) (list 23 3 51))
(check-expect (after-days 3 (list 4) 0) (list 4))
(check-expect (after-days 3 empty 2) empty)
(check-expect (after-days 5 (list 3 0 5) 3) (list 406 31 656))