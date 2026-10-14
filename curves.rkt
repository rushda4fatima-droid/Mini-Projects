;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname curves) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A04, Problem 4
;; ***************************************************
;;

;;
;; Question 4 (a)
;;

;; Helper Function:

;; Produces the result of evaluating a polynomial at the given value of x
;; eval-poly: (listof Num) Num -> Num
;; Requires list to contain the coefficients from lowest degree to highest degree.
(define (eval-poly lon x)
  (cond
    [(empty? lon) 0]
    [else (+ (first lon) (* x (eval-poly (rest lon) x)))]))

;; Tests
(check-expect (eval-poly (cons -4 (cons 7 (cons 0 (cons 2 empty)))) 3) 71)
(check-expect (eval-poly (list -1 2 -6 2) 3) 5)
(check-expect (eval-poly empty 5) 0)
(check-expect (eval-poly (list 5) 3) 5)
(check-expect (eval-poly (list 3 2 1) 2) 11)
(check-expect (eval-poly (list 0 0 27 4) 2) 140)

;; Actual Function:

;; A Curve-Poly is (cons Int (cons Int (cons Int (cons Int empty))))
;; Checks if a given curve is elliptic
;; elliptic?: Curve-Poly -> Bool
;; Requires Curve-Poly to be non-empty
(define (elliptic? lon)
  (cond
    [(zero? (+ (eval-poly (list 0 0 0 4) (second lon))
          (eval-poly (list 0 0 27) (first lon))))  false]
    [else true]))

;; Tests:
(check-expect (elliptic? (list 7 0 0 1)) true)
(check-expect (elliptic? (list 2 -3 0 1)) false)
(check-expect (elliptic? (list 0 0 0 1)) false)

;;
;; Question 4 (b)
;;

;; A Curve-Poly is (cons Int (cons Int (cons Int (cons Int empty))))
;; Produces a list of a valid elliptic curve's values at each x-coordinate given.
;; curve-values: Curve-Poly (listof Int) -> (listof Int) or Bool
;; Requires Curve-Poly to be non-empty
(define (curve-values curve-poly lon)
  (cond
    [(not (elliptic? curve-poly)) false]
    [(empty? lon) empty]
    [(elliptic? curve-poly)
       (cons (eval-poly curve-poly (first lon)) (curve-values curve-poly (rest lon)))]))

;; Tests
(check-expect (curve-values (list 7 0 0 1) (list -2 -1 0 1 2))
(list -1 6 7 8 15))
(check-expect (curve-values (list 7 0 0 1) empty) empty)
(check-expect (curve-values (list 2 -3 0 1) (list 1)) false)
(check-expect (curve-values (list 2 -3 0 1) empty) false)

;;
;; Question 4 (c)
;;

;; Produces a list of values of b for which the Weierstrass equation is an elliptic curve given a.
;; usable-b: Int (listof Int) -> (listof Int)
(define (usable-b a lon)
  (cond
    [(empty? lon) empty]
    [(elliptic? (list (first lon) a)) (cons (first lon) (usable-b a (rest lon)))]
    [else (usable-b a (rest lon))]))

;; Tests:
(check-expect (usable-b 0 (list 0 7 -1)) (list 7 -1))
(check-expect (usable-b -3 (list 2 0 -2)) (list 0))
(check-expect (usable-b 5 empty) empty)