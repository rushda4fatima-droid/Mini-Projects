;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname list-recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A03, Problem 5
;; ***************************************************
;;

;;
;; Question 5 (a)
;;

;; odd-sum produces the sum of all odd-integers given a list of integers
;; odd-sum: (listof Int) -> Int
(define (odd-sum lst)
  (cond
    [(empty? lst) 0]
    [(not (= (remainder (first lst) 2) 0)) (+ (first lst) (odd-sum (rest lst)))]
    [else (odd-sum (rest lst))]))

;; Tests:
(check-expect (odd-sum (cons 3 (cons 2 (cons 4 (cons 1 empty))))) 4)
(check-expect (odd-sum empty) 0)
(check-expect (odd-sum (cons -2 (cons -9 (cons 1 empty)))) -8)
(check-expect (odd-sum (cons 2 (cons -4 (cons 0 empty)))) 0)

;;
;; Question 5 (b)
;;

;; A UB is a Num 
;; A LB is a Num
;; count-within-range produces a count of how many numbers in a given list are within the
;; upper bound and lower bound (inclusive)
;; count-within-range: ((listof Num) LB UB) -> Nat
(define (count-within-range lst LB UB)
  (cond
    [(empty? lst) 0]
    [(and (<= (first lst) UB) (>= (first lst) LB)) (add1 (count-within-range (rest lst) LB UB))]
    [else (count-within-range (rest lst) LB UB)]))

;; Tests
(check-expect (count-within-range (cons -1 (cons 3.1 (cons 2 (cons -10 empty)))) -5 pi) 3)
(check-expect (count-within-range empty 5 20) 0)
(check-expect (count-within-range (cons 10 (cons -20 (cons 40 empty))) 2 5) 0)
(check-expect (count-within-range (cons -2 (cons 4 (cons 11.4 (cons 20 empty)))) -2 11.5) 3)

;;
;; Question 5 (c)
;;

;; strictly-increasing? checks if the numbers in a given list are in strictly increasing order
;; strictly-increasing?: (listof Num) -> bool
(define (strictly-increasing? lst)
  (cond
    [(empty? lst) true]
    [(empty? (rest lst)) true]
    [(< (first lst) (first (rest lst))) (and true (strictly-increasing? (rest lst)))]
    [else false]))

(check-expect (strictly-increasing? (cons 2 (cons 3 (cons 5 empty)))) true)
(check-expect (strictly-increasing? (cons 2 (cons 1 (cons 4 (cons 5 empty))))) false)
(check-expect (strictly-increasing? (cons 2 empty)) true)
(check-expect (strictly-increasing? empty) true)

;;
;; Question 5 (d)
;;

;; weird-sum calculates the sum of number of numbers and twice the number of symbols
;; weird-sum: (listof (anyof Num Bool Sym)) -> Nat
(define (weird-sum lst)
  (cond
    [(empty? lst) 0]
    [(number? (first lst)) (+ 1 (weird-sum (rest lst)))]
    [(symbol? (first lst)) (+ 2 (weird-sum (rest lst)))]
    [else (weird-sum (rest lst))]))

;; Tests:
(check-expect (weird-sum (cons 5 (cons 'bye (cons true (cons 11 empty))))) 4)
(check-expect (weird-sum empty) 0)
(check-expect (weird-sum (cons 'thisassignmentiseasy (cons true empty))) 2)
(check-expect (weird-sum (cons true (cons false empty))) 0)

;;
;; Question 5 (e)
;;

;; Helper Functions:

;; count-symbols counts the number of symbols in a given list
;; count-symbols: (listof (anyof Num Bool Sym)) -> Nat
(define (count-symbols lst)
  (cond
    [(empty? lst) 0]
    [(symbol? (first lst)) (add1 (count-symbols (rest lst)))]
    [else (count-symbols (rest lst))]))

;; Tests:
(check-expect (count-symbols empty) 0)
(check-expect (count-symbols (cons 'fullmarksplease (cons 'theLoo (cons 7 (cons true empty))))) 2)
(check-expect (count-symbols (cons 3 (cons false (cons 'ihateschool empty)))) 1)
(check-expect (count-symbols (cons 4 (cons 9 empty))) 0)

;; count-num counts the number of numbers in a given list
;; count-num: (listof (anyof Num Bool Sym)) -> Nat
(define (count-num lst)
  (cond
    [(empty? lst) 0]
    [(number? (first lst)) (add1 (count-num (rest lst)))]
    [else (count-num (rest lst))]))

;; Tests:
(check-expect (count-num empty) 0)
(check-expect (count-num (cons 'helloworld (cons 7 (cons false empty)))) 1)
(check-expect (count-num (cons 6 (cons 7 (cons 'sixseven empty)))) 2)
(check-expect (count-num (cons false (cons false (cons 'bruh. empty)))) 0)

;; count-bool counts the number of bool values in a list
;; count-bool: (listof (anyof Num Bool Sym)) -> Nat
(define (count-bool lst)
  (cond
    [(empty? lst) 0]
    [(boolean? (first lst)) (add1 (count-bool (rest lst)))]
    [else (count-bool (rest lst))]))

;; Tests:
(check-expect (count-bool empty) 0)
(check-expect (count-bool (cons 5 (cons 11 (cons 'ihatethis empty)))) 0)
(check-expect (count-bool (cons 9 (cons true (cons false empty)))) 2)

;; A Count-List is (cons Nat (cons Nat (cons Nat empty)))
;; count-types provides a list in order of the count of numbers, symbols, and booleans in a list
;; count-types: (listof (anyof Num Bool Sym)) -> Count-List
(define (count-types lst)
  (cons (count-num lst) (cons (count-symbols lst) (cons (count-bool lst) empty))))

;; Tests:
(check-expect (count-types empty) (cons 0 (cons 0 (cons 0 empty))))
(check-expect (count-types (cons 42 empty)) (cons 1 (cons 0 (cons 0 empty))))
(check-expect (count-types (cons 'hello empty)) (cons 0 (cons 1 (cons 0 empty))))
(check-expect (count-types (cons true empty)) (cons 0 (cons 0 (cons 1 empty))))
(check-expect (count-types (cons 1 (cons -5.5 (cons 0 empty)))) (cons 3 (cons 0 (cons 0 empty))))
(check-expect (count-types (cons 'a (cons 'b (cons 'c empty)))) (cons 0 (cons 3 (cons 0 empty))))
(check-expect (count-types (cons true (cons false (cons true empty))))
              (cons 0 (cons 0 (cons 3 empty))))
(check-expect (count-types (cons 10 (cons 'apple (cons true (cons 'banana (cons false empty))))))
              (cons 1 (cons 2 (cons 2 empty))))
(check-expect (count-types (cons false (cons 3.14 (cons 'x empty)))) 
              (cons 1 (cons 1 (cons 1 empty))))