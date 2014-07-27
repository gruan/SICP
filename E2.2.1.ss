;;;Section 2.2.1 Exercises:

;Context

;Squares a number.

(define (square x)
  (* x x))

;Exercise 2.17

;Returns the list that contains only the last element of a given (nonempty) list.

(define (last-pair list1)
  (if (null? (cdr list1))
    list1
    (last-pair (cdr list1))))

;Exercise 2.18

;Takes a list as an argument and returns a list of the same elements in reverse order.

(define (reverse-list list1)
  (define (rr count)
    (if (< count 0)
      '()
      (cons (list-ref list1 count) (rr (- count 1)))))
  (rr (- (length list1) 1)))

;Exercise 2.19

;Counts how many ways you can make change out coins for a certain amount.

(define us-coins (list 50 25 10 5 1)) ;List of US coins
(define uk-coins (list 100 50 20 10 5 2 1 0.5)) ;List of UK coins

(define (cc amount coin-values) 
  (cond ((= amount 0) 1) ;One possibility.
	((or (< amount 0) (no-more? coin-values)) 0) ;Coin combination doesn't work.
	(else
	  (+ (cc amount ;Checks the amount but without the first coin.
		 (except-first-denomination coin-values))
	     (cc (- amount ;Subtracts the amount from the first coin.
		    (first-denomination coin-values))
		 coin-values)))))

(define (except-first-denomination coin-values) ;Excludes first coin of the list
  (cdr coin-values))

(define (first-denomination coin-values) ;Gives first coin value.
  (car coin-values))

(define (no-more? coin-values) ;Checks if there are any coins left.
  (null? coin-values))

;No, the order of the list coin-values should not affect the answer produced by cc. 
;Although if you put the smaller denominations first, the process will take longer since it
;checks more combination possibilities the order should not matter.

;Exercise 2.20

;Takes one or more integers and returns a list of all the arguments that have the same even-odd
;parity as the first argument.

(define (same-parity x . y)
  
  (define (odd-parity rest-of-numbers) ;Filters through and creates a list of odd numbers.
    (same-parity-list next-odd-list rest-of-numbers))

  (define (even-parity rest-of-numbers) ;Filters through and creates a list of even numbers.
    (same-parity-list next-even-list rest-of-numbers))

  (if (odd? x) ;Tests if the first argument is odd or even.
    (cons x (odd-parity (next-odd-list y)))
    (cons x (even-parity (next-even-list y)))))


 (define (same-parity-list next-number rest-of-numbers) ;Template for lists with same parity.
    (if (or (null? rest-of-numbers) (null? (cdr rest-of-numbers)))
      rest-of-numbers
      (cons (car rest-of-numbers)
	    (same-parity-list next-number (next-number (cdr rest-of-numbers))))))

(define (next-list-number condition? givenlist) ;Template for finding the next same parity number
  (cond ((condition? (car givenlist)) givenlist)
	((null? (cdr givenlist)) '())
	(else (next-list-number condition? (cdr givenlist)))))

(define (next-odd-list list-of-numbers) ;Finds odd number in the list.
  (next-list-number odd? list-of-numbers))

(define (next-even-list list-of-numbers) ;Finds next even number in the list.
  (next-list-number even? list-of-numbers))

;Exercise 2.21

;Takes a list of numbers as an argument and returns a list of the squares of those numbers.

;Less abstract procedure.

(define (bad-square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (bad-square-list (cdr items)))))

;Procedure using maps template.

(define (square-list items)
  (map (lambda (x) (square x))
       items))

;Exercise 2.22

;Reverses a list and squares it iteratively.

(define (reverse-square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    (cons (square (car things))
		  answer))))
  (iter items '()))

;It produces the list in the reverse order because the recursing function is the first number in
;the pair. This causes the cons procedure to construct pairs in the opposite direction as
;intended. 


;Bugged square list procedure.

(define (bug-square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    (cons answer
		  (square (car things))))))
  (iter items '()))

;This procedure is bugged because the construct procedure is followed by the answer.
;The answer is already and will always be a complete pair so by constructing a pair onto another
;number, the cons procedures are nested in the wrong way and you're adding a pair and then a
;number and not the other way around.
;Also the first term in the list is nil.

;Exercise 2.23

;Implementation of for-each that prints the squares of numbers up to x.

(define (display-square x)
  (define (create-list count)
    (if (> count x)
      '()
      (cons count (create-list (+ count 1)))))
  (define numbers (create-list 1))
  (for-each (lambda (x) (display (square x)) (newline))
	    numbers))
