;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname efficiency) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A04, Problem 5
;; ***************************************************
;;

;;
;; Question 5 (a)
;;

;; Karina's code:

;; Produces the sum of the numbers in a list in ore
;; sum-k: (listof Num) -> Num
(define (sum-k lon)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (sum-k (rest lon)))]))

;; Claire's code:

;; Helper functions:

;; Combines two list of numbers together in the order of list given.
;; my-append: (listof Num) (listof Num) -> (listof Num)
(define (my-append lon1 lon2)
  (cond
    [(empty? lon1) lon2]
    [else (cons (first lon1) (my-append (rest lon1) lon2))]))

;; Reverses the order of numbers in a given list
;; my-reverse: (listof Num) -> (listof Num)
(define (my-reverse lon)
  (cond
    [(empty? lon) empty]
    [else (my-append (my-reverse (rest lon)) (list (first lon)))]))

;; Produces the sum of the numbers in a list plus one
;; add-up: (listof Num) -> Num
(define (add-up lon)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (add-up (rest lon)))]))

;; Actual Function:

;; Produces the sum of the numbers in a given list. 
;; sum-c: (listof Num) -> Num
(define (sum-c lon)
  (add-up (my-reverse lon)))

;; Testing Correctness:
(check-expect (sum-k empty) 0)
(check-expect (sum-k (list 7)) 7)
(check-expect (sum-k (list 1 2 3 4)) 10)
(check-expect (sum-k (list -5 10 3 -2)) 6)

(check-expect (sum-c empty) 0)
(check-expect (sum-c (list 7)) 7)
(check-expect (sum-c (list 1 2 3 4)) 10)
(check-expect (sum-c (list -5 10 3 -2)) 6)

;; Test findings:
;; Found error in function add-up, base case when lon is empty should output 0 instead of 1.
;; Documenting and testing must come before efficiency, because if the purpose is not understood then
;; understanding more efficient ways will be skewd, and if the code is wrong then it does not matter
;; how efficient it is.

;;
;; Question 5 (b)
;;

;; Step Counting:
(check-expect (sum-k (list 7)) 7)
(check-expect (sum-k (list 7 3)) 10)
(check-expect (sum-k (list 7 3 5)) 15)
(check-expect (sum-k (list 7 3 5 2)) 17)

(check-expect (sum-c (list 7)) 7)
(check-expect (sum-c (list 7 3)) 10)
(check-expect (sum-c (list 7 3 5)) 15)
(check-expect (sum-c (list 7 3 5 2)) 17)

;; Measured Findings:
;; (define karina-steps (list 11 18 25 32))
;; (define claire-steps (list 24 47 77 114))

;;
;; Question 5 (c)
;;

;; Formula for sum-k: steps = 7n + 4

;; Produces the number of steps taken to compute a list of n length for sum-k code
;; f-karina: Nat -> Nat
(define (f-karina n)
  (+ (* 7 n) 4))

;; Test cases:
(check-expect (f-karina 0) 4)
(check-expect (f-karina 1) 11)
(check-expect (f-karina 4) 32)


;; Formula for sum-c: steps = 3.5n^2 + 12.5n + 8

;; Produces the number of steps taken to compute a list of n length for sum-c code:
;; f-claire: Nat -> Nat
(define (f-claire n)
  (+ (* 3.5 (expt n 2)) (* 12.5 n) 8))

(check-expect (f-claire 0) 8)
(check-expect (f-claire 1) 24)
(check-expect (f-claire 2) 47)

;; Formula vs Measured Step Count:
(define karina-steps (list 11 18 25 32))
(define f-karina-list (list 11 18 25 32))
(define claire-steps (list 24 47 77 114))
(define f-claire-list (list 24 47 77 114))

;;
;; Question 5 (d)
;;

;; Estimate based on formula:
(check-expect (f-karina 100) 704)
(check-expect (f-claire 100) 36258)

;; Karina's solution is more efficient as it takes fewer steps to compute and is a linear time
;; function compared to Claire's quadratic time function and greater number of steps.
;; Claire's code line: "[else (my-append (my-reverse (rest lon)) (list (first lon)))]))"
;; recalls needlessly to my-reverse at every single step of the recursion blowing up the step-count.