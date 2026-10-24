;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname w-store) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Rushda Fatima (21239088)
;; CS 135 Fall 2026
;; Assignment A05, Problem 4
;; ***************************************************
;;

;;
;; Question 4 (a)
;;

;; A Item is a (list Sym Num Nat)
;; Requires Num to be non-negative.

;; An Inventory is (listof Item)
;; Requires each Item's Sym within Inventory to be unique.

;; A SalesList is (listof Sym)
;; Requires symbols in SalesList to exist in the Inventory list.
;; Number of  times Symbol repeats in SalesList must be less than or equal to Nat of the
; matching Item in Inventory

;;
;; Question 4 (b)
;;

;; Helper Functions

;; Reduces item number in Inventory by one if it matches given symbol
;; and removes item from inventory when item number hits 0
;; reduce: Sym Inventory -> Inventory
(define (update item inventory)
  (cond
    [(empty? inventory) inventory]
    [(symbol=? item (first (first inventory)))
     (cond
       [(= 1 (third (first inventory))) (rest inventory)]
       [else (cons (list (first (first inventory)) (second (first inventory))
                         (sub1 (third (first inventory)))) (rest inventory))])]
    [else (cons (first inventory) (update item (rest inventory)))]))

;; Test Cases:
(check-expect (update 'clipboard empty) empty)
(check-expect (update 'calculator (list (list 'calculator 6.99 1))) empty)
(check-expect (update 'craigkaplan (list (list 'craigkaplan 6.99 8)))
                                 (list (list 'craigkaplan 6.99 7)))
(check-expect (update 'cookie (list (list 'birthdaycake 12.99 2) (list 'cookie 6.99 8)))
                                 (list (list 'birthdaycake 12.99 2) (list 'cookie 6.99 7)))
(check-expect (update 'baby
                      (list (list 'dancer 6.99 8) (list 'megamind 12.99 2)))
                            (list (list 'dancer 6.99 8) (list 'megamind 12.99 2)))

;; Actual Function:

;; Produces an updated Inventory where the number of items on hand is
;; reduced by one for each item named in the sales list.
;; adjust-inventory: SalesList Inventory -> Inventory
(define (adjust-inventory sales inventory)
  (cond
    [(empty? sales) inventory]
    [(empty? inventory) inventory]
    [else (adjust-inventory (rest sales) (update (first sales) inventory))]))

;; Test Cases:
(check-expect (adjust-inventory empty (list (list 'clipboard 6.99 8)))
              (list (list 'clipboard 6.99 8)))
(check-expect (adjust-inventory (list 'clipboard) empty) empty)
(check-expect (adjust-inventory (list 'clipboard) (list (list 'clipboard 6.99 8)))
              (list (list 'clipboard 6.99 7)))
(check-expect (adjust-inventory (list 'clipboard) (list (list 'clipboard 6.99 1))) empty)
(check-expect (adjust-inventory (list 'clipboard 'clipboard) (list (list 'clipboard 6.99 3)))
              (list (list 'clipboard 6.99 1)))
(check-expect (adjust-inventory (list 'clipboard 'pen) (list (list 'clipboard 6.99 8)
                             (list 'pen 0.99 10))) (list (list 'clipboard 6.99 7) (list 'pen 0.99 9)))
(check-expect (adjust-inventory (list 'calculator) (list (list 'clipboard 6.99 8)))
              (list (list 'clipboard 6.99 8)))
(check-expect (adjust-inventory (list 'glasses 'clipboard 'glasses) (list (list 'glasses 12.99 2)
                         (list 'clipboard 6.99 3))) (list (list 'clipboard 6.99 2)))




