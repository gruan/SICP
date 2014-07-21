;;;Section 1.3.4 Exercises:

;Context.

;Calculates the square of a number.

(define (square x)
  (* x x))

;Calculates the cube of a number.

(define (cube x)
  (* x x x))

;Performs the fixed-point method on a function that is transformed.

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;Expresses Newton's method.

(define (newton-transform g) ;Transforms function g(x) into Newton's method.
  (lambda (x)
    (- x
       (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) ;Implements Newton's method.
  (fixed-point (newton-transform g) guess))

;Computes the derivative of a function g(x).

(define dx 0.000001) ;Defines how small we want dx to be.
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) 
		 dx)))

;Finds the square-root using average damping and the fixed point method.

(define (sqrt-approx x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

;Finds the fixed point of a function f(x).

(define tolerance .00001) ;defines how accurate we want close-enough? to be as an end-case.
(define (fixed-point f first-guess)
  (define (close-enough? x y) ;Defines end case test.
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess))) ;Defines next value.
      (if (close-enough? guess next) ;Implements end case test.
	next
	(try next))))
  (try first-guess))

;Expresses average damping.

(define (average-damp f)
  (lambda (x) (average x (f x))))

;Computes the average of 2 numbers.

(define (average x y)
  (/ (+ x y) 2))

;Computes the average of 3 numbers.

(define (average3 x y z)
  (/ (+ x y z) 3))

;Exercise 1.40

;Approximates the roots of a cubic function x^3 + ax^2 + bx + c using Newton's method.

(define (cubic-newtons a b c) ;Implements Newton's method.
  (newtons-method (cubic a b c) 1))

(define (cubic a b c) ;Expresses x^3 + ax^2 + bx +c.
  (lambda (x) (+ (cube x)
		 (* a (square x))
		 (* b x)
		 c)))

;Exercise 1.41

;Increments a number by 1.

(define (inc x)
  (+ x 1))

;Takes a procedure and applies it twice.

(define (double f)
  (lambda (x) (f (f x))))

;(((double (double double)) inc) 5)
;Ends up being 21 since the (double double) can be looked at as a quadruple.
;And then you nest quadruple with double making the consequent procedure be performed 16 times.
;5 + 16 = 21

;Exercise 1.42

;Expresses x -> f(g(x))

(define (compose f g)
  (lambda (x) (f (g x))))

;Exercise 1.43

;Takes a procedure and performs it n times.

(define (repeated f n)
  (cond ((= n 1) (lambda (x) (f x)))
	(else (lambda (x) ((compose f (repeated f (- n 1))) 
			   x)))))

;Exercise 1.44

;Smooths a function g(x) using a small number dx.

(define (smooth g dx)
  (lambda (x) (average3 (g (- x dx))
			(g x)
			(g (+ x dx)))))

;Repeatedly smooths a function g(x) n times.

(define (rep-smooth g dx n)
  (repeated (smooth g dx) n))

;Exercise 1.45

;Computes nth roots as a fixed-point search based upon repeated average damping of x/y^(n-1).

(define (nth-roots x n)
  (fixed-point ((repeated average-damp n)
		(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

;Exercise 1.46

;Expresses a computational strategy called iterative improvement.
;Takes inital guess and continues the process using the improved guess until the guess is good
;enough.

(define (iterative-improve good-enough? improve)
  (lambda (y) (if (good-enough? y)
		y
		((iterative-improve good-enough? improve) (improve y))))) ;Recursive call

;Square root procedure rewritten with iterative-improve.

(define (sqrt-iter x)

  (define (good-enough? guess) ;End case test: guess^2 is close enough to x?
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess) ;Improves the guess.
    (average guess (/ x guess)))

  ((iterative-improve good-enough? improve) 1.0))

;Fixed-point procedure rewritten with iterative-improve.

(define (fixed-point-iter f first-guess)
  (define tolerance 0.00000001) ;How accurate you want the answer to be.

  (define (good-enough? guess) ;End case test: f(guess) is close enough to guess?
    (< (abs (- guess (improve guess))) tolerance))

  (define (improve guess) ;Improves the guess.
    (f guess))

  ((iterative-improve good-enough? improve) first-guess))
