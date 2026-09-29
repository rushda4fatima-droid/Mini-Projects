;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A02, Problem 3
;; ***************************************************
;;

;;(median-of-3-simple) produces the median amoung three numbers given.
;;median-of-3-simple: Num -> Num
(define (median-of-3-simple a b c)
  (cond
    [(<= a b)
       (cond
         [(<= c a) a]
         [(<= b c) b]
         [else c])]
    [(<= a c) a]
    [(<= c b) b]
    [else c]))

;; Testing all possible orders:
(check-expect (median-of-3-simple 1 2 3) 2)
(check-expect (median-of-3-simple 2 1 3) 2)
(check-expect (median-of-3-simple 2 3 1) 2)
(check-expect (median-of-3-simple 1 3 2) 2)
(check-expect (median-of-3-simple 3 1 2) 2)
(check-expect (median-of-3-simple 3 2 1) 2)
(check-expect (median-of-3-simple 1 1 2) 1)
(check-expect (median-of-3-simple 2 1 1) 1)
(check-expect (median-of-3-simple 1 2 1) 1)