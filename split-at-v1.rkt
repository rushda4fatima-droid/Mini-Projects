;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname split-at-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A05, Problem 3 (a)
;; ***************************************************
;;

;;
;; Question 3 (a)
;;

;; A Pair is (list (listof Any) (listof Any))
;; Produces a pair where the first element of the pair is a list containing the first n items from a
;; given list and the second element of the pair contains the remaining items of the given list.
;; split-at: Nat (listof Any) -> Pair
;; Requires Nat to be less than the length of the list given.
(define (split-at n loa)
  (cond
    [(zero? n) (list empty loa)]
    [else (list (cons (first loa) (first (split-at (sub1 n) (rest loa))))
                (second (split-at (sub1 n) (rest loa))))]))

(check-expect (split-at 1 (list 0 1 2 3)) (list (list 0) (list 1 2 3)))
(check-expect (split-at 2 (list 'a 'b 'c 'd)) (list (list 'a 'b) (list 'c 'd)))        
(check-expect (split-at 0 empty) (list empty empty))
(check-expect (split-at 1 (list 'a false 'symbol 78)) (list (list 'a) (list false 'symbol 78)))
(check-expect (split-at 4 (list 1 2 'a 'pasta false true 5)) (list (list 1 2 'a 'pasta)
                                                                  (list false true 5)))
(check-expect (split-at 0 (list 0 1 2 3)) (list empty (list 0 1 2 3)))

;;
;; Question 3 (c)
;;

(define split-at-steps (list 4 19 49 109 229 469))

(define efficiency 'exponential)