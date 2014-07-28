;;;Section 2.2.2 Exercises:

;Exercise 2.24

;(list 1 (list 2 (list 3 4))) outputs (1 (2 (3 4))).

;For the corresponding box and pointer structure and the interpretation of this as a tree look
;at the img_E2.24.jpg.

;Exercise 2.25

;(1 3 (5 7) 9)
;(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

;((7))
;(car (car (list (list 7))))

;(1 (2 (3 (4 (5 (6 7))))))

;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;Exercise 2.26

;(define x (list 1 2 3))
;(define y (list 4 5 6))

;(append x y)
;(1 2 3 4 5 6)

;(cons x y)
;((1 2 3) 4 5 6)

;(list x y)
;((1 2 3) (4 5 6))

;Exercise 2.27

;Takes a list as an argument and returns as its value the list with its elements reversed
;and with all sublists deep-reversed as well.

(define (deep-reverse things)
  (define (iter things answer)
    (cond ((null? things) answer) ;Base case.
	  ((pair? (car things)) ;Run a recursive case on any nested lists.
	   (iter (cdr things) 
		 (cons (deep-reverse (car things)) ;Creates reversed nested list.
		       answer)))
	  (else ;Move down the list while adding values in a leftward fashion.
	    (iter (cdr things)
		  (cons (car things)
			answer)))))
  (iter things '())) ;Start with nil at the right.

;Exercise 2.28

;Takes as an argument a tree (represented as a list) and returns a list whose elements are
;all the leaves of the tree arranged in left-to-right order.

(define (fringe things)
  (define (iter things answer)
    (cond ((null? things) answer) ;Base case test.
	  ((pair? (car things)) ;If nested list found run on the nested list.
	   (iter (cdr things) 
		 (iter (car things) answer)))
	  (else
	    (iter (cdr things)
		  (append answer 
			  (list (car things))))))) ;Convert value to list.
  (iter things '()))

;Exercise 2.29

;Binary mobile constructor consisting of two branches.

(define (make-mobile left right)
  (cons left right))

;Branch constructor consisting of a length and structure.

(define (make-branch length1 structure)
  (cons length1 structure))

;a.

;Binary mobile selectors.

(define (left-branch mobile) ;Left branch of a binary mobile.
  (car mobile))

(define (right-branch mobile) ;Right branch of a binary mobile.
  (cdr mobile))

;Branch selectors.

(define (branch-length branch) ;Length of branch.
  (car branch))

(define (branch-structure branch) ;Structure of branch.
  (cdr branch))

;b.

;Total weight of a mobile.

(define (weight mobile)
  (if (branch? mobile) ;If branch, give the weight.
    (branch-structure mobile) 
    (+ (weight (left-branch mobile)) ;Else call weight on both branches of the mobile.
       (weight (right-branch mobile)))))
  
;Tests if the object is a branch or mobile.

(define (branch? mobile)
  (not (pair? (left-branch mobile))))

;c.

;Tests whether a binary mobile is balanced.

(define (balanced? mobile)
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))

(define (torque mobile)
  (if (branch? mobile)
    (* (branch-length mobile) (branch-structure mobile))
    (+ (torque (left-branch mobile))
       (torque (right-branch mobile)))))

;d

;If we changed the constructors, I would only need to change one of each of their selectors.
;Instead of cadr to select the second value of the object, I would use cdr instead.

;Exercise 2.30

;Squares a number.

(define (square x)
  (* x x))

;Takes a tree of numbers as an argument and returns a tree of the squares of those numbers.
;Using maps

(define (square-tree-maps tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	   (square-tree sub-tree)
	   (square sub-tree)))
       tree))

;Takes a tree of numbers as an argument and returns a tree of the squares of those numbers.

(define (square-tree tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (square tree))
	(else
	  (cons (square-tree (car tree))
		(square-tree (cdr tree))))))

;Exercise 2.31

;Tree-map abstraction.

(define (tree-map proc tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (proc tree))
	(else
	  (cons (tree-map proc (car tree))
		(tree-map proc (cdr tree))))))

;Takes a tree of numbers as an argument and returns a tree of the squares of those numbers.
;Using tree-map.

(define (square-tree-tmap tree)
  (tree-map square tree))

;Exercise 2.32

;Generates the set of subsets of a set.

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x))
			rest)))))

;The idea is that we can represent the set of all of the set's sublists as the union of:
; - The set of all sublists excluding the first number.
; - The set of all sublists excluding the first number, while adding the first
;   number to the beginning of each sublist.

;Rest represents the first rule and (map (lambda (x) (cons (car s) x)) rest) represents 
;the second rule.
