;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A05, Problem 1
;; ***************************************************
;;

;;
;; Question 1 (a)
;;

;; Determines if an element is contained within a set
;; in?: Sym Set -> Bool
(define (in? sym set)
  (cond
    [(empty? set) false]
    [(symbol=? sym (first set)) true]
    [else (in? sym (rest set))]))

;; Test Cases
(check-expect (in? 'cake (list 'bread 'butter 'eggs 'oven 'cream 'milk)) false)
(check-expect (in? 'birtday empty) false)
(check-expect (in? 'car (list 'plane 'boat 'walk 'car 'tricycle 'bicycle)) true)

;;
;; Question 1 (b)
;;

;; Adds an element to the set if the element is not already in the set.
;; add: Sym Set -> Set
(define (add sym set)
  (cond
    [(empty? set) (list sym)]
    [(in? sym set) set]
    [else (cons sym set)]))

;; Test Cases
(check-expect (add 'son (list 'father 'daughter 'mother)) (list 'son 'father 'daughter 'mother))
(check-expect (add 'grandma (list 'father 'sugardaddy 'grandma)) (list 'father 'sugardaddy 'grandma))
(check-expect (add 'kaplan empty) (list 'kaplan))

;;
;; Question 1 (c)
;;

;; Produces the union of two sets
;; union: Set Set -> Set
(define (union set1 set2)
  (cond
    [(empty? set1) set2]
    [(empty? set2) set1]
    [(in? (first set1) set2) (union (rest set1) set2)]
    [else (cons (first set1) (union (rest set1) set2))]))

   
;; Test Cases
(check-expect (union (list 'a 'b 'c 'd) (list 'a 'n 'four 'c)) (list 'b 'd 'a 'n 'four 'c))
(check-expect (union empty (list 'a 'v 'd)) (list 'a 'v 'd))
(check-expect (union (list 'craig 'kaplan) empty) (list 'craig 'kaplan))
(check-expect (union (list 'list 'a 'n 'capital 'canada) (list 'alberta 'referendum))
              (list 'list 'a 'n 'capital 'canada 'alberta 'referendum))
(check-expect (union (list 'a 'b 'c 'd) (list 'a 'b 'c 'd)) (list 'a 'b 'c 'd))

;;
;; Question 1 (d)
;;

;; Produces the intersection of two sets
;; intersection: Set Set -> Set
(define (intersection set1 set2)
  (cond
    [(empty? set1) empty]
    [(empty? set2) empty]
    [(in? (first set1) set2) (cons (first set1) (intersection (rest set1) set2))]
    [else (intersection (rest set1) set2)]))

;; Test Cases
(check-expect (intersection empty empty) empty)
(check-expect (intersection (list 'a 'b 'c 'd 'e) empty) empty)
(check-expect (intersection empty (list 'I 'love 'waterloo)) empty)
(check-expect (intersection (list 'apples 'banana 'strawberry) (list 'bread 'kiwi 'jam)) empty)
(check-expect (intersection (list 'orange 'red 'blue 'yellow) (list 'green 'red 'pink 'yellow))
              (list 'red 'yellow))

;;
;; Question 1 (e)
;;

;; Helper Functions:

;; Determines if the first set is a subset of the second set.
;; subset? Set Set -> Bool
(define (subset? set1 set2)
  (cond
    [(empty? set1) true]
    [(in? (first set1) set2) (subset? (rest set1) set2)]
    [else false]))

;; Test Cases:
(check-expect (subset? empty empty) true)
(check-expect (subset? empty (list 'a 'b 'c 'd)) true)
(check-expect (subset? (list 'a 'b 'c 'd) empty) false)
(check-expect (subset? (list 'a 'b) (list 'a 'b 'c 'd)) true)
(check-expect (subset? (list 'a 'n) (list 'a 'b 'c)) false)
(check-expect (subset? (list 'a) (list 'a)) true)

;; Actual Function:

;; Determines if two sets are equal
;; set=?: Set Set -> Bool
(define (set=? set1 set2)
  (cond
    [(and (empty? set1) (empty? set2)) true]
    [(and (subset? set1 set2) (subset? set2 set1)) true]
    [else false]))
    

;; Test Cases
(check-expect (set=? empty empty) true)
(check-expect (set=? (list 'jimin 'taehyung 'jin) empty) false)
(check-expect (set=? empty (list 'namjoon 'suga 'jhope)) false)
(check-expect (set=? (list 'a 'b 'c 'd) (list 'd 'c 'a 'b)) true)
(check-expect (set=? (list 'hello 'my 'name 'is) (list 'dobby 'harry 'hermione)) false)
(check-expect (set=? (list 'girl 'dinner 'happy) (list 'girl 'dinner 'happy)) true)

;;
;; Question 1 (f)
;;

;; Helper Function:

;; Determines if a list is strictly a list of symbols. 
;; lst-sym?: (listof Any) -> Bool
(define (lst-sym? lst)
  (cond
    [(empty? lst) true]
    [(symbol? (first lst)) (lst-sym? (rest lst))]
    [else false]))

;; Test Cases
(check-expect (lst-sym? empty) true)
(check-expect (lst-sym? (list 'symbol 'symbol 3 'fries)) false)
(check-expect (lst-sym? (list 'apple 'banana 'orange)) true)
(check-expect (lst-sym? (list true 'banana 'north)) false)

;; Actual Function:

;; Determines if something is a set
;; set?: Any -> Bool
(define (set? input)
  (cond
    [(list? input)
     (cond
       [(empty? input) true]
       [(lst-sym? input)
         (cond
          [(not (in? (first input) (rest input))) (set? (rest input))]
          [else false])]
       [else false])]         
    [else false]))

;; Test Cases
(check-expect (set? true) false)
(check-expect (set? 'pancakes) false)
(check-expect (set? 4) false)
(check-expect (set? (list 'hello 1 'apple)) false)
(check-expect (set? (list 'hello 'green 'yellow 'red 'green 'brown 'tarot)) false)
(check-expect (set? (list 1 2 4 5)) false)
(check-expect (set? empty) true)
(check-expect (set? (list 'apples 'banana 'pasta 'yellow)) true)