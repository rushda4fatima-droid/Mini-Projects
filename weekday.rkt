;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname weekday) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A02, Problem 5
;; ***************************************************
;;


;;Helper functions:

;;Gives the actual month number: Where 1=Jan, 2=Feb...12=Dec
(define (act-month date) 
  (remainder (quotient date 100) 100))


;;Gives formula's month number: Where 1=Mar, 2=Apr...11=Jan, 12=Feb
(define (month date)
  (cond
    ((<= (act-month date) 2) (+ (act-month date) 10))
    (else (- (act-month date) 2))))


;;Gives formula's four digit full year i.e 2008: Where Jan and Feb are months of the preceding year.
(define (full-year date)
  (cond
    ((>= (month date) 11) (- (quotient date 10000) 1))
    (else (quotient date 10000))))


;;Gives year within century: i.e 2008 -> 08
(define (year date)
  (remainder (full-year date) 100))


;;Gives century: i.e 2008 -> 20
(define (century date)
  (quotient (full-year date) 100))


;;Calculates the number from 0 to 6 for corresponding weekday. 
(define (number date)
  (modulo (floor (+ (remainder date 100) (- (* 2.6 (month date)) 0.2)
                    (- (* 2 (century date))) (remainder (year date) 100)
                                           (quotient (remainder (year date) 100) 4)
                                           (quotient (century date) 4))) 7))


;;--------------------------------------Final Function:

;;A Week-Day is (anyof 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday)

;;(date->day-of-week) produces what day of the week it was/is/will be given a date.
;;date->day-of-week: Nat -> Week-Day
;; Requires: date corresponds to a valid Gregorian date on or after 17530101
(define (date->day-of-week date)
  (cond
    [(= (number date) 0) 'Sunday]
    [(= (number date) 1) 'Monday]
    [(= (number date) 2) 'Tuesday]
    [(= (number date) 3) 'Wednesday]
    [(= (number date) 4) 'Thursday]
    [(= (number date) 5) 'Friday]
    [(= (number date) 6) 'Satuarday]))

;; Tests:
(check-expect (date->day-of-week 20240924) 'Tuesday)
(check-expect (date->day-of-week 38781202) 'Monday)
(check-expect (date->day-of-week 20050801) 'Monday)
(check-expect (date->day-of-week 20080402) 'Wednesday)
(check-expect (date->day-of-week 20121222) 'Satuarday)
(check-expect (date->day-of-week 19831216) 'Friday)
(check-expect (date->day-of-week 19750723) 'Wednesday)
(check-expect (date->day-of-week 20291122) 'Thursday)
(check-expect (date->day-of-week 64701102) 'Sunday)
(check-expect (date->day-of-week 20080214) 'Thursday)

;; Citations:
;; https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html