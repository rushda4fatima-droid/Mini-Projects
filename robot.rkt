;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A03, Problem 4
;; ***************************************************
;;

;;
;; Question 4 (a)
;;

;; A Direction is (anyof 'North 'South 'East 'West)
;; A State is (cons Int (cons Int (Direction empty)))
;; Requires: each integer to be greater than or equal to 0 and less than or equal to 10

;;
;; Question 4 (b)
;;

;; mk-state consumes two integers and a Direction and produces a State
;; Requires: each integer to be greater than or equal to 0 and less than or equal to 10
;; mk-state: (Int Int Direction) -> State
(define (mk-state x y Direction)
  (cons x (cons y (cons Direction empty))))

;; get-x consumes a State and produces the x coordinate
;; get-x: State -> Int
(define (get-x State)
  (first State))

;; get-y consumes a State and produces the y coordinate
;; get-y: State -> Int
(define (get-y State)
  (first (rest State)))

;; get-direction consumes a State and produces the Direction
;; get-direction: State -> Direction
(define (get-direction State)
  (first (rest (rest State))))

;; Test Helper Functions:
(check-expect (mk-state 3 5 'North) (cons 3 (cons 5 (cons 'North empty))))
(check-expect (get-x (cons 8 (cons 4 (cons 'West empty)))) 8)
(check-expect (get-y (cons 8 (cons 4 (cons 'West empty)))) 4)
(check-expect (get-direction (cons 8 (cons 4 (cons 'West empty)))) 'West)

;;
;; Question 4 (c)
;;

;; A Command is (anyof 'forward 'turn-left 'turn-right)
;; robot-ctl produces a new State based on command given
;; Requires a valid Command & State to be given
;; robot-ctl: (State Command) -> State
(define (robot-ctl State Command)
  (cond
    [(symbol=? Command 'turn-left)
     (cond
       [(symbol=? (get-direction State) 'North)
            (cons (get-x State) (cons (get-y State) (cons 'West empty)))]
       [(symbol=? (get-direction State) 'West)
            (cons (get-x State) (cons (get-y State) (cons 'South empty)))]
       [(symbol=? (get-direction State) 'South)
            (cons (get-x State) (cons (get-y State) (cons 'East empty)))]
       [else (cons (get-x State) (cons (get-y State) (cons 'North empty)))])]
    [(symbol=? Command 'turn-right)
     (cond
       [(symbol=? (get-direction State) 'North)
            (cons (get-x State) (cons (get-y State) (cons 'East empty)))]
       [(symbol=? (get-direction State) 'East)
            (cons (get-x State) (cons (get-y State) (cons 'South empty)))]
       [(symbol=? (get-direction State) 'South)
            (cons (get-x State) (cons (get-y State) (cons 'West empty)))]
       [else (cons (get-x State) (cons (get-y State) (cons 'North empty)))])]
    [(symbol=? Command 'forward)
     (cond
       [(and (= (get-x State) 0) (symbol=? (get-direction State) 'West)) State]
       [(and (= (get-x State) 10) (symbol=? (get-direction State) 'East)) State]
       [(and (= (get-y State) 0) (symbol=? (get-direction State) 'South)) State]
       [(and (= (get-y State) 10) (symbol=? (get-direction State) 'North)) State]
       [(symbol=? (get-direction State) 'North)
            (cons (get-x State) (cons (+ (get-y State) 1) (cons (get-direction State) empty)))]
       [(symbol=? (get-direction State) 'South)
            (cons (get-x State) (cons (- (get-y State) 1) (cons (get-direction State) empty)))]
       [(symbol=? (get-direction State) 'East)
            (cons (+ (get-x State) 1) (cons (get-y State) (cons (get-direction State) empty)))]
       [else (cons (- (get-x State) 1) (cons (get-y State) (cons (get-direction State) empty)))])]))
       
;; Tests:
(check-expect (robot-ctl (cons 4 (cons 9 (cons 'East empty))) 'turn-left)
              (cons 4 (cons 9 (cons 'North empty))))
(check-expect (robot-ctl (cons 4 (cons 9 (cons 'North empty))) 'turn-left)
              (cons 4 (cons 9 (cons 'West empty))))
(check-expect (robot-ctl (cons 4 (cons 9 (cons 'West empty))) 'turn-left)
              (cons 4 (cons 9 (cons 'South empty))))
(check-expect (robot-ctl (cons 4 (cons 9 (cons 'South empty))) 'turn-left)
              (cons 4 (cons 9 (cons 'East empty))))
(check-expect (robot-ctl (cons 7 (cons 0 (cons 'North empty))) 'turn-right)
              (cons 7 (cons 0 (cons 'East empty))))
(check-expect (robot-ctl (cons 7 (cons 0 (cons 'East empty))) 'turn-right)
              (cons 7 (cons 0 (cons 'South empty))))
(check-expect (robot-ctl (cons 7 (cons 0 (cons 'South empty))) 'turn-right)
              (cons 7 (cons 0 (cons 'West empty))))
(check-expect (robot-ctl (cons 7 (cons 0 (cons 'West empty))) 'turn-right)
              (cons 7 (cons 0 (cons 'North empty))))
(check-expect (robot-ctl (cons 10 (cons 0 (cons 'North empty))) 'forward)
              (cons 10 (cons 1 (cons 'North empty))))
(check-expect (robot-ctl (cons 10 (cons 1 (cons 'South empty))) 'forward)
              (cons 10 (cons 0 (cons 'South empty))))
(check-expect (robot-ctl (cons 0 (cons 5 (cons 'East empty))) 'forward)
              (cons 1 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 1 (cons 5 (cons 'West empty))) 'forward)
              (cons 0 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 0 (cons 5 (cons 'West empty))) 'forward)
              (cons 0 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 10 (cons 5 (cons 'East empty))) 'forward)
              (cons 10 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 2 (cons 10 (cons 'North empty))) 'forward)
              (cons 2 (cons 10 (cons 'North empty))))
(check-expect (robot-ctl (cons 2 (cons 0 (cons 'South empty))) 'forward)
              (cons 2 (cons 0 (cons 'South empty))))
