;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A01, Problem 2
;; ***************************************************


;;Problem (a)
(define (doughnut-surface-area r z)
 (* 4 (sqr pi) r z))


;;Problem (b)
(define R  8.3144626)
(define (pressure n T V)
  (/ (* n R T) V))


;;Problem (c)
(define (q s d t)
  (* s (exp (* (- d) t))))


;;Problem (d)
(define (freq base-frequency interval)
  (* base-frequency (expt 2 (/ interval 12))))