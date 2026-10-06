;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rgb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A03, Problem 3
;; ***************************************************
;;


;;
;; An RGB Triplet (RGB) is a (cons Nat (cons Nat (cons Nat empty)))
;; Requires: each element of the triplet must be <= 255
;;

;;
;; Question 3 (a)
;;

;; (mk-rgb) consumes three natural numbers and produces an RGB Triplet
;; Requires: each natural number input to be <= 255
;; mk-rgb: Nat -> RGB
(define (mk-rgb r g b)
  (cons r (cons g (cons b empty))))

;; get-r gets the red component in the RGB Triplet
;; get-r: RGB Triplet -> Nat
(define (get-r RGB) (first RGB))

;; get-g gets the green component in the RGB Triplet
;; get-g: RGB Triplet -> Nat
(define (get-g RGB)
  (first (rest RGB)))

;; get-b gets the blue component in the RGB Triplet
;; get-b: RGB Triplet -> Nat
(define (get-b RGB)
   (first (rest (rest RGB))))

;; Tests
(check-expect (mk-rgb 50 66 77) (cons 50 (cons 66 (cons 77 empty))))
(check-expect (get-r (cons 50 (cons 20 (cons 11 empty)))) 50)
(check-expect (get-g (cons 50 (cons 20 (cons 11 empty)))) 20)
(check-expect (get-b (cons 50 (cons 20 (cons 11 empty)))) 11)

;;
;; Question 3 (b)
;;

;; A Colour is (anyof 'red, 'green 'blue 'yellow 'cyan
;; 'magenta 'white 'black 'unknown)
;; (rgb->name) consumes an RGB Triplet and produces a colour name
;; rgb->name: RGB Triplet -> Colour
(define (rgb->name RGB)
  (cond
    [(and (= (get-r RGB) 255) (= (get-g RGB) 0) (= (get-b RGB) 0)) 'red]
    [(and (= (get-r RGB) 0) (= (get-g RGB) 255) (= (get-b RGB) 0)) 'green]
    [(and (= (get-r RGB) 0) (= (get-g RGB) 0) (= (get-b RGB) 255)) 'blue]
    [(and (= (get-r RGB) 0) (= (get-g RGB) 0) (= (get-b RGB) 0)) 'black]
    [(and (= (get-r RGB) 255) (= (get-g RGB) 255) (= (get-b RGB) 255)) 'white]
    [(and (= (get-r RGB) 255) (= (get-g RGB) 255) (= (get-b RGB) 0)) 'yellow]
    [(and (= (get-r RGB) 255) (= (get-g RGB) 0) (= (get-b RGB) 255)) 'magenta]
    [(and (= (get-r RGB) 0) (= (get-g RGB) 255) (= (get-b RGB) 255)) 'cyan]
    [else 'unknown]))

;; Test:
(check-expect (rgb->name (cons 255 (cons 0 (cons 0 empty)))) 'red)
(check-expect (rgb->name (cons 0 (cons 255 (cons 0 empty)))) 'green)
(check-expect (rgb->name (cons 0 (cons 0 (cons 255 empty)))) 'blue)
(check-expect (rgb->name (cons 0 (cons 0 (cons 0 empty)))) 'black)
(check-expect (rgb->name (cons 255 (cons 255 (cons 255 empty)))) 'white)
(check-expect (rgb->name (cons 255 (cons 255 (cons 0 empty)))) 'yellow)
(check-expect (rgb->name (cons 255 (cons 0 (cons 255 empty)))) 'magenta)
(check-expect (rgb->name (cons 0 (cons 255 (cons 255 empty)))) 'cyan)
(check-expect (rgb->name (cons 0 (cons 73 (cons 56 empty)))) 'unknown)
(check-expect (rgb->name (cons 39 (cons 38 (cons 180 empty)))) 'unknown)

;;
;; Question 3 (c)
;;

;; (valid-rgb?) consumes anything and produces bool value for if it is a valid RGB Triplet
;; valid-rgb?: Any -> Bool
(define (valid-rgb? Any)
  (cond
    [(empty? Any) false]
    [(cons? Any)
     (cond
       [(and (integer? (get-r Any)) (integer? (get-g Any)) (integer? (get-b Any)))
           (cond
              [(and (<= (get-r Any) 255) (<= (get-g Any) 255) (<= (get-b Any) 255)) true]
              [else false])])]
    [else false]))

;; Test:
(check-expect (valid-rgb? 'red) false)
(check-expect (valid-rgb? (cons 255 (cons 0 (cons 0 empty)))) true)
(check-expect (valid-rgb? (cons 314 (cons 159 (cons 26 empty)))) false)
(check-expect (valid-rgb? empty) false)
       