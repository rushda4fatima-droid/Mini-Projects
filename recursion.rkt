;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A03, Problem 1
;; ***************************************************
;;


;;
;; Question 1 (a)
;;

;;(harmonic) adds the inverse of natural numbers starting at 1 till the nth term
;;harmonic: Nat -> Rat
(define (harmonic n)
  (cond
    [(zero? n) 0]
    [(= n 1) 1]
    [else (+ (/ 1 n) (harmonic (sub1 n)))]))


;; Test Cases:
(check-expect (harmonic 2) 1.5)
(check-expect (harmonic 1) 1)
(check-expect (harmonic 4) 25/12)
(check-expect (harmonic 0) 0)

;;
;; Question 1 (b)
;;

;;(ss-closed) computes the sum of squares of natural numbers till the nth term
;;ss-closed: Nat -> Nat
(define (ss-closed n)
  (/ (* n (+ 1 n) (+ (* 2 n) 1)) 6))

;; Test
(check-expect (ss-closed 5) 55)
(check-expect (ss-closed 0) 0)
(check-expect (ss-closed 3) 14)
(check-expect (ss-closed 1) 1)
(check-expect (ss-closed 2) 5)


;;(ss-recursive) uses recursion on the natural numbers to compute the sum of squares till the nth term
;;ss-recursive: Nat -> Nat
(define (ss-recursive n)
  (cond
    [(zero? n) 0]
    [(= n 1) 1]
    [else (+ (sqr n) (ss-recursive (sub1 n)))]))

;; Test
(check-expect (ss-recursive 5) 55)
(check-expect (ss-recursive 0) 0)
(check-expect (ss-recursive 3) 14)
(check-expect (ss-recursive 1) 1)
(check-expect (ss-recursive 2) 5)



