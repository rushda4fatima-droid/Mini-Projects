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
;; Requires quantity of items within inventory to be greater than or equal to 1. 

;; A SalesList is (listof Sym)
;; Requires number of times one Symbol appears in SalesList must be less than or equal to Nat of the
; matching Item in Inventory

;;
;; Question 4 (b)
;;

;; Helper Functions:

;; Creates an item given the name, price and quantity
;; mk-item: Sym Num Nat -> Item
(define (mk-item name price amount)
  (list name price amount))

;; Gets item name
;; get-name: Item -> Sym
(define (get-name item)
  (first item))

;; Gets item's price
;; get-price: Item -> Num
(define (get-price item)
  (second item))

;; Gets quantity of an item.
;; get-amount: Item -> Nat
(define (get-amount item)
  (third item))

;; Test Cases:
(check-expect (mk-item 'cookie 5.50 2) (list 'cookie 5.50 2))
(check-expect (get-name (list 'cookie 5.50 2)) 'cookie)
(check-expect (get-price (list 'cookie 5.50 2)) 5.50)
(check-expect (get-amount (list 'cookie 5.50 2)) 2)

;; Counts the number of types an symbol appears in list
;; count-sale: Sym (listof Sym) -> Nat
(define (count-sale item sales)
  (cond
    [(empty? sales) 0]
    [(symbol=? item (first sales)) (add1 (count-sale item (rest sales)))]
    [else (count-sale item (rest sales))]))

;; Test Cases:
(check-expect (count-sale 'diabetes empty) 0)
(check-expect (count-sale 'cherry (list 'cherry 'on 'top 'cherry)) 2)
(check-expect (count-sale 'lift (list 'baby 'counter 'strategy 'chess)) 0)

;; Produces a list with an item whose quantity is reduced by one for each time the name
;; of the item appears in a second list and removes item if quantity reached is 0.
;; update-item: Item SalesList -> Inventory
(define (update-item item sales)
  (cond
    [(<= (get-amount item) (count-sale (get-name item) sales)) empty]
    [else (list (mk-item (get-name item) (get-price item)
                         (- (get-amount item) (count-sale (get-name item) sales))))]))

;; Test Cases:
(check-expect (update-item (mk-item 'calculator 2.50 10) empty) (list (list 'calculator 2.50 10)))
(check-expect (update-item (mk-item 'topi 5.55 4) (list 'banana 'bread 'pecan))
              (list (list 'topi 5.55 4)))
(check-expect (update-item (mk-item 'kopiko 4.44 7) (list 'kdrama 'star 'kopiko 'refreshing))
              (list (list 'kopiko 4.44 6)))
(check-expect (update-item (mk-item 'star 5.67 2) (list 'star 'drama 'dust 'star 'dust)) empty)
(check-expect (update-item (mk-item 'blue 9.99 3) (list 'happy 'blue 'blues 'blue 'green))
              (list (list 'blue 9.99 1)))

;; Actual Function:

;; Produces new Inventory with the quantity of item reduced by one for each time item appears in
;; SalesList. When quantity equals to zero item is removed from Inventory.
;; adjust-inventory: SalesList Inventory -> Inventory
(define (adjust-inventory sales inventory)
  (cond
    [(empty? inventory) empty]
    [(empty? sales) inventory]
    [else (append (update-item (first inventory) sales)
                  (adjust-inventory sales (rest inventory)))]))

;; Test Cases:
(check-expect (adjust-inventory (list 'pizza 'taco) empty) empty)
(check-expect (adjust-inventory empty (list (list 'shrek 5.50 3) (list 'donkey 2.25 2)))
              (list (list 'shrek 5.50 3) (list 'donkey 2.25 2)))
(check-expect (adjust-inventory (list 'among-us 'minecraft) (list (mk-item 'pizza 4.99 5)
                                                                  (mk-item 'taco 3.50 2)))
              (list (mk-item 'pizza 4.99 5) (mk-item 'taco 3.50 2)))
(check-expect (adjust-inventory (list 'chicken-nugget) (list (mk-item 'chicken-nugget 6.99 4)
                                                             (mk-item 'soup 2.50 3)))
              (list (mk-item 'chicken-nugget 6.99 3) (mk-item 'soup 2.50 3)))
(check-expect (adjust-inventory (list 'boba 'boba 'boba) (list (mk-item 'boba 7.25 5)
                                                               (mk-item 'ramen 12.99 2)))
              (list (mk-item 'boba 7.25 2) (mk-item 'ramen 12.99 2)))
(check-expect (adjust-inventory (list 'emotional-support-pickle 'emotional-support-pickle)
                                (list (mk-item 'emotional-support-pickle 1.00 2)
                                      (mk-item 'therapy-cat 999.99 1)))
              (list (mk-item 'therapy-cat 999.99 1)))
(check-expect (adjust-inventory (list 'cheese 'cheese 'cursed-sock) (list (mk-item 'cheese 8.00 2)
                                                                    (mk-item 'cursed-sock 0.50 4)
                                                                    (mk-item 'wifi 100.00 1)))
              (list (mk-item 'cursed-sock 0.50 3) (mk-item 'wifi 100.00 1)))
(check-expect (adjust-inventory (list 'braincell 'braincell 'last-fry)
                                (list (mk-item 'braincell 0.01 2) (mk-item 'last-fry 0.99 1))) empty)
              