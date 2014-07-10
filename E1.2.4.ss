;;;Section 1.2.4 Exercises:

;;;Exercise 1.16

;;;Implement an exponential iterative recursive procedure in O(log(n))

(define (square x)
   (* x x))

(define (fast-exp b n)
   (ee b n 1))

(define (ee b n a)
   (cond ((zero? n) (* a))
	 ((odd? n) (ee b (- n 1) (* b a)))
         ((even? n) (ee (square b) (/ n 2) a))))

;;;Exercise 1.17

;;;Implement a multiplication recursive procedure in O(log(n))

(define (double x)
   (* x 2))

(define (halve x)
   (/ x 2))

(define (mlt a b)
   (cond ((= b 0) 0)
         ((even? b) (mlt (double a) (halve b)))
         (else (+ a (mlt a (- b 1))))))

;;;Exercise 1.18

;;;Implement a multiplication iterative recursive procedure in O(log(n))

(define (mlt-iter a b)
   (mm a b 0))

(define (mm a b counter) 
   (cond ((zero? b) counter)
	 ((even? b)(mm (double a) (halve b) counter))
	 ((odd? b) (mm a (- b 1) (+ counter a)))))

;;;Exercise 1.19

;;;Implement a Fibonacci iterative recursive procedure in O(log(n))

(define (fib n)
   (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
   (cond ((zero? count) b)
	 ((even? count)
	  (fib-iter a
		    b
		    (+ (expt q 2) (expt p 2))
		    (+ (expt q 2) (* 2 q p))
		    (/ count 2)))
	 (else (fib-iter (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (- count 1)))))
