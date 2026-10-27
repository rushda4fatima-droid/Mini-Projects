;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname split-at-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A05, Problem 3 (b)
;; ***************************************************
;;

;;
;; Question 3 (b)
;;

;; Helper Function:

;; A Pair is (list (listof Any) (listof Any))

;; Adds an element to the first of the list in a Pair
;; my-cons: Any Pair -> Pair
(define (my-cons n pair)
  (list (cons n (first pair)) (second pair)))

;; Test Cases:
(check-expect (my-cons 'goblin (list (list 'pizza) (list 'chatnoir)))
              (list (list 'goblin 'pizza) (list 'chatnoir)))
(check-expect (my-cons 'shrek (list empty (list 'swamp))) (list (list 'shrek) (list 'swamp)))
(check-expect (my-cons 'daddy (list empty empty)) (list (list 'daddy) empty))
(check-expect (my-cons 'son (list (list 'meme) empty)) (list (list 'son 'meme) empty))

;; Actual Function:

;; Produces a pair where the first element of the pair is a list containing the first n items from a
;; given list and the second element of the pair contains the remaining items of the given list.
;; split-at: Nat (listof Any) -> Pair
;; Requires Nat to be less than or equal to the length of the list given.
(define (split-at n loa)
  (cond
    [(zero? n) (list empty loa)]
    [else (my-cons (first loa) (split-at (sub1 n) (rest loa)))]))

(check-expect (split-at 1 (list 'mama 1 2 3)) (list (list 'mama) (list 1 2 3)))
(check-expect (split-at 2 (list 'may 'i 'have 'm)) (list (list 'may 'i) (list 'have 'm)))        
(check-expect (split-at 0 empty) (list empty empty))
(check-expect (split-at 1 (list 'a false 'symbol 78)) (list (list 'a) (list false 'symbol 78)))
(check-expect (split-at 4 (list 'cookie 'no 'diabeto 'roll 'back 'to 'kitchen))
              (list (list 'cookie 'no 'diabeto 'roll) (list 'back 'to 'kitchen)))
(check-expect (split-at 0 (list 0 false 2 '67)) (list empty (list 0 false 2 '67)))

;;
;; Question 3 (c)
;;

(define split-at-steps (list 4 15 26 37 48 59))

(define efficiency 'linear)