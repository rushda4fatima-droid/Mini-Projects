;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname activation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A02, Problem 2
;; ***************************************************
;;


;;
;; Question 2 (a)
;;

;;(sigmoid) transforms an inexact number into a value between 0 and 1.
;;sigmoid: Num -> [or 0 1]
(define (sigmoid x)
  (/ 1 (+ 1 (expt e (- x)))))

;; Testing:
(check-within (sigmoid 0.67) 0 1)
(check-within (sigmoid 7.3) 0 1)
(check-within (sigmoid -9.9) 0 1)
(check-within (sigmoid 0) 0 1)
(check-within (sigmoid 1) 0 1)


;;
;; Question 2 (b)
;;

;;(relu) turns negative numbers in 0 and positive numbers remain the same.
;;relu: Num -> Num
(define (relu x)
  (cond
    [(< x 0) 0]
    [(>= x 0) x]))

;; Testing:
(check-expect (relu -1.7) 0)
(check-expect (relu -20) 0)
(check-expect (relu 0) 0)
(check-expect (relu 1) 1)
(check-expect (relu 20.77) 20.77)

;;
;; Question 2 (c)
;;

;;(swish) Multiplies input by its sigmoid value. Allows slightly below 0 for negative numbers and positive numbers go linear.
;;swish: Num -> Num
(define (swish x)
  (* x (sigmoid x)))

;; Testing:
(check-within (swish -5000) 0 0.1)
(check-expect (swish 0) 0.0)
(check-within (swish 5) 4.9 5)

;;
;; Question 2 (d)
;;

;;(hard-sigmoid) gives a faster approximation of the function (sigmoid).
;;hard-sigmoid: Num -> [or 0 1]
(define (hard-sigmoid x)
  (cond
    [(<= x -3) 0]
    [(>= x 3) 1]
    [else (/ (+ x 3) 6)]))

;; Testing:
(check-expect (hard-sigmoid -3) 0)
(check-expect (hard-sigmoid -6) 0)
(check-expect (hard-sigmoid 0) 1/2)
(check-expect (hard-sigmoid 1) 2/3)
(check-expect (hard-sigmoid 3) 1)
(check-expect (hard-sigmoid 6) 1)

;;
;; Question 2 (e)
;;

;;(hard-swish) gives a faster approximation of the function (swish)
;;hard-swish: Num -> Num
(define (hard-swish x)
  (* x (hard-sigmoid x)))

;; Testing:
(check-expect (hard-swish 3) 3)
(check-expect (hard-swish 1) 2/3)
(check-expect (hard-swish -4) 0)
(check-expect (hard-swish 0) 0)