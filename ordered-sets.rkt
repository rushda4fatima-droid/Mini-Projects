;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ordered-sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A05, Problem 2
;; ***************************************************
;;

;; An IntSet is a (listof Int)
;; Requires: elements must be unique
;; elements must be in ascending order

;;
;; Question 2 (a)
;;

;; Determines if an integer is contained within a IntSet.
;; in?: Int IntSet -> Bool
(define (in? int intset)
  (cond
    [(empty? intset) false]
    [(= int (first intset)) true]
    [else (in? int (rest intset))]))

;; Test Cases
(check-expect (in? 5 (list 1 2 8 10)) false)
(check-expect (in? 4 empty) false)
(check-expect (in? 67 (list 1 9 39 20 67 81 4 9)) true)

;;
;; Question 2 (b)
;;

;; Adds an integer to the IntSet if the integer is not already in the IntSet.
;; add: Int IntSet -> IntSet
(define (add int intset)
  (cond
    [(empty? intset) (list int)]
    [(= int (first intset)) intset]
    [(< int (first intset)) (cons int intset)]
    [else (cons (first intset) (add int (rest intset)))]))

;; Test Cases:
(check-expect (add 5 (list 1 2 3 4 6 7)) (list 1 2 3 4 5 6 7))
(check-expect (add 6 (list 2 4 6 7)) (list 2 4 6 7))
(check-expect (add 3 empty) (list 3))

;;
;; Question 2 (c)
;;

;; Produces the union of two IntSets
;; union: IntSet Intset -> Intset
(define (union intset1 intset2)
  (cond
    [(empty? intset1) intset2]
    [(empty? intset2) intset1]
    [(= (first intset1) (first intset2)) (cons (first intset1) (union (rest intset1) (rest intset2)))]
    [(< (first intset1) (first intset2)) (cons (first intset1) (union (rest intset1) intset2))]
    [else (cons (first intset2) (union intset1 (rest intset2)))]))

;; Test Cases
(check-expect (union (list 1 2 4 5) (list 1 3 7 9)) (list 1 2 3 4 5 7 9))
(check-expect (union (list -2 0 4 5) (list -3 -2 7 9)) (list -3 -2 0 4 5 7 9))
(check-expect (union empty (list 77 89 100)) (list 77 89 100))
(check-expect (union (list -5 3 2) empty) (list -5 3 2))
(check-expect (union (list -4 -2 -1 0) (list 2 4 6)) (list -4 -2 -1 0 2 4 6))
(check-expect (union (list 1 2 3 4) (list 1 2 3 4)) (list 1 2 3 4))

;;
;; Question 2 (d)
;;

;;Produces the intersection of two IntSets.
;; intersection: IntSet IntSet -> IntSet
(define (intersection intset1 intset2)
  (cond
    [(empty? intset1) empty]
    [(empty? intset2) empty]
    [(= (first intset1) (first intset2)) (cons (first intset1)
                                         (intersection (rest intset1) (rest intset2)))]
    [(< (first intset1) (first intset2)) (intersection (rest intset1) intset2)]
    [else (intersection intset1 (rest intset2))]))

;; Test Cases
(check-expect (intersection empty empty) empty)
(check-expect (intersection (list 1 4 8 9) empty) empty)
(check-expect (intersection empty (list 4 8 10 200)) empty)
(check-expect (intersection (list -1 -3 5 7 9) (list -2 4 6 8)) empty)
(check-expect (intersection (list -6 -4 7 90) (list -5 -4 6 7 90 100)) (list -4 7 90))
(check-expect (intersection (list 1 5 7) (list 2 3 5 8)) (list 5))

;;
;; Question 2 (e)
;;

;; Determines if two IntSets are equal
;; set=?: IntSet IntSet -> Bool
(define (set=? intset1 intset2)
  (cond
    [(and (empty? intset1) (empty? intset2)) true]
    [(empty? intset1) false]
    [(empty? intset2) false]
    [(= (first intset1) (first intset2)) (set=? (rest intset1) (rest intset2))]
    [else false]))

;; Test Cases:
(check-expect (set=? empty empty) true)
(check-expect (set=? (list 1 4 7 9) empty) false)
(check-expect (set=? empty (list 8 9 10)) false)
(check-expect (set=? (list 1 5 8) (list 1 5 8 9)) false)
(check-expect (set=? (list -3 8 9) (list -3 8 10)) false)
(check-expect (set=? (list 1 3 6) (list 1 3 6)) true)

;;
;; Question 2 (f)
;;

;; Helper Function:

;; Determines if a given list is a list of integers only
;; loi?: (listof Any) -> Bool
(define (loi? lst)
  (cond
    [(empty? lst) true]
    [(integer? (first lst)) (loi? (rest lst))]
    [else false]))

;; Test Cases:
(check-expect (loi? (list 5 6 0.9 pi 2)) false)
(check-expect (loi? (list 8 'birthday true 9)) false)
(check-expect (loi? (list 8 9 2 false)) false)
(check-expect (loi? (list 3 8 9 2)) true)

;; Determines if something is a IntSet
;; set?: Any -> Bool
(define (set? input)
  (cond
    [(list? input)
     (cond
       [(loi? input)
        (cond
          [(empty? input) true]
          [(= 1 (length input)) true] 
          [(< (first input) (second input)) (set? (rest input))]
          [else false])]
       [else false])]
    [else false]))

;; Test Cases:
(check-expect (set? empty) true)
(check-expect (set? 'birthday) false)
(check-expect (set? (list 1 -5 6 'happy)) false)
(check-expect (set? (list 3 5 8 7)) false)
(check-expect (set? (list -1 4 7 9)) true)