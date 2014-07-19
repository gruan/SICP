;;;Section 1.3.3 Exercises:

;Context.

;Computes the root of a function f by means of the half-interval method.

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) ;Calls search with correct arguments a and b
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else 
	    (error "Values are not of opposite sign" a b))))) ;Displays error if a and b aren't of opposite sign.

;Searches for the root of a function f by means of the half-interval method.

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point) ;End case test: both endpoints must be close enough.
      midpoint
      (let ((test-value (f midpoint))) ;Calls next search function with the correct arguments.
	(cond ((positive? test-value)
	       (search f neg-point midpoint))
	      ((negative? test-value)
	       (search f midpoint pos-point))
	      (else midpoint))))))

;Determines whether the arguments are close enough as an end case test.

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

;Calculates average of two arguments.

(define (average x y)
  (/ (+ x y) 2))

;Calculates the fixed point of a function f: x satisfies the equation f(x) = x.
;Uses guesses while applying f repeatedly until the value doesn't change very much.
;f(x), f(f(x)), f(f(f(x))),....

(define (fixed-point f first-guess)
(define tolerance 0.00001) ;Tolerance accepted of the difference between two successive values.

  (define (close-enough? v1 v2) ;End case test: Are arguments satisfy the tolerance?
    (< (abs (- v1 v2)) tolerance))

  (define (try guess) ;Compares the values of the next guess and the current one. Returns solution.
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))

  (try first-guess))

;Exercise 1.35

;The golden ratio is a geometric relationship expressed algebraically, 
;for quantities a and b with a > b > 0 as:

;a/b = (a+b)/a.

;If we assume the golden ratio, P = a/b. We can re-arrange the equation.

;P = a/a + b/a.
;P = 1 + 1/P.

(define (golden-ratio first-guess)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) first-guess))

;Exercise 1.36

;Modifies fixed-point so that it prints the sequence of approximations it generates.

(define (fixed-point-print f first-guess)
(define tolerance 0.00001) ;Tolerance accepted of the difference between two successive values.

  (define (close-enough? v1 v2) ;End case test: Are arguments satisfy the tolerance?
    (< (abs (- v1 v2)) tolerance))

  (define (try guess);Compares the values of the next guess and the current one. Returns solution.
    (display guess) ;Displays every guess
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))

  (try first-guess))

;Finds the solution for x^x = 1000 using the fixed point method.

(define (xx first-guess)
  (fixed-point-print (lambda (x) (/ (log 1000) (log x))) first-guess))

;Finds the solution for x^x = 1000 using the fixed point method and average damping.

(define (xx-damping first-guess)
  (fixed-point-print (lambda (x) (average x (/ (log 1000) (log x)))) first-guess))

;The test results show that xx-damping arrived at the answer in less steps and that it was more
;accurate than its counterpart that didn't use average damping, xx. Though this won't 
;happen every time. It's good to know that we aren't sacrificing speed or accuracy 
;when we implement average damping. 

;Exercise 1.37

;Iterative recursive procedure template of an accummulator which operates on the basis of a condition

(define (accumulator-finder finder? operator null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (if (finder? a)
	(iter (next a) (operator (term a) result))
	(iter (next a) result))))
  (iter a null-value))

;Identity condition for accumulator.

(define (identity? x)
  (= x x))

;Calculates a k-term finite continued fraction.

(define (cont-frac n d k)
  (define (frac i)
      (if (< i k)
	(/ (n i) (+ (d i) (frac (+ i 1))))
	(/ (n i) (d i))))
  (frac 1))

;It must be set so that k >= 10 
;for the approximation of 1/golden rule to be accurate to 4 decimal places.

;Exercise 1.38

;Continued fraction expansion to approximate e - 2 where e is the base of natural logarithms.

(define (e-minus-2 k)
  (define (function x)
    (if (= (modulo x 3) 2) ;Calculates d which increments by 2 every 3rd number after the 2nd number.
      (* 2 (/ (+ x 1) 3))
      1))
  (cont-frac (lambda (x) 1.0) function k))

;Approximates e, the base of natural logarithms.
;Uses the continued fraction expansions of e -2.

(define (approx-e k)
  (+ 2 (e-minus-2 k)))

;Exercise 1.39

;Continued fraction approximation of tan(x).

(define (tan-cf x k)
  (define (n-function a)
    (if (= a 1)
      x
     (- 0 (square x))))
  
  (define (d-function a)
    (+ 1 (* 2.0 (- a 1))))
  
  (cont-frac n-function d-function k))

;Squares a number

(define (square x)
  (* x x))
