;;;Section 2.3.1 Exercises:

;Exercise 2.53

;(a b c)
;((george))
;((y1 y2))
;(y1 y2)
;#f
;#f
;red shoes blue socks

;Exercise 2.54

;not pair a not pair b
;;;compare the two. finish

;not pair a pair b
;;;false

;pair a not pair b
;;;false

;pair a pair b
;;;compare the two elements
;;;if the lists have different lengths then it will be false since a or b will not be a pair when the other is.

(define (equal?? a b)
  (cond ((and (null? a) (null? b)) #t) ;Same length.

	((or (null? a) (null? b)) #f) ;Not same length.
	
	((and (pair? a) (pair? b)) ;Both pairs.
	 (and (equal?? (car a) (car b))
	      (equal?? (cdr a ) (cdr b))))
	
	((or (pair? a) (pair? b)) #f) ;Only one is a pair the other is not.

	((and (not (pair? a)) (not (pair? b))) ;both symbols so compare the two.
	 (eq? a b))))
	       
;Exercise 2.55

;' is shorthand for quote. so ''abracadabra gives (quote (quote abracadabra))
;(car (quote (quote abracadabra))) gives quote which is '.
