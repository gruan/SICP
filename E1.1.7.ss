;;;Section 1.1.7 Exercises:

;;;Exercise 1.6

;;;In principle, the same result should be obtained for substituting the procedure:

;;;(define (new-if predicate then-clause else-clause)
;;;  (cond (predicate then-clause)
;;;        (else else-clause)))

;;; for the 'if' procedure.
;;;In fact the

;;; (cond (predicate then-clause)
;;;       (else else-clause)))

;;;part of the procedure does the exact same thing as the 'if' procedure. 
;;;The difference arises in the fact that one is substituting a normally evaluated procedure 'new-if', for a procedure that is evaluated in a special way, the 'if' procedure. As a result, the interpreter uses applicative-order evaluation, evaluating the operator and the arguments before applying them.

;;;In the following:

;;;(define (sqrt-iter guess x)
;;;  (new-if (good-enough? guess x)
;;;          guess
;;;          (sqrt-iter (improve guess x)
;;;		x)))

;;;(good-enough? guess x), guess, and (sqrt-iter (improve guess x) x) are all evaluated. Since 'sqrt-iter calls itself, the procedure then runs into an infinite loop of calling itself.

;;;Exercise 1.7

;;;The 'good-enough' procedure fails to compute square roots with very small numbers due to its verification process. It passes a 'true' value if | guess^2 - x | < 0.001. Here is an example that shows why this is a bad verification process:

;;;Ex. x = 0.0000001, also 10^-7, and guess = 0.01
;;;When run through the procedure 'guess-enough' one gets | (0.01^2) - 0.00000001 | = 0.0000999 which is less than 0.001, returning a 'true' value. However the actual square root of 0.0000001 is around 0.00031622 which is not even close to 0.01.

;;;The procedure also becomes inefficient when trying to find the square root of very large numbers. This is because the the computation guess^2 in the 'guess-enough?' procedure creates HUGE differences between the radicand and the guess^2, with large numbers, with even the tiniest differences between the square root of the radicand and the guess. This creates a problem because the procedure 'good-enough?' will only pass a 'true' value if the difference between the radicand and guess^2 is less than 0.001.

;;;Ex. x = 10,000,000,000 and guess = 100,000.000001. The square root of x is 100,000.
;;;But | guess^2 - x | = 0.2 which is greater than 0.001, returning a 'false' value.

;;;New square-root procedure that uses different end test.

(define (average x y)
   (/ (+ x y) 2))

(define (improve guess x)
   (average (/ x guess) guess))

(define (guess-ratio guess btrguess)
   (- 1 (/ btrguess guess)))

(define (guess-good? guess btrguess)
   (and (< (guess-ratio guess btrguess) 0.001)
	(> (guess-ratio guess btrguess) 0)))

(define (ss x guess btrguess)
   (if (guess-good? guess btrguess)
      btrguess
      (ss x btrguess (improve btrguess x))))

(define (sqrt-iter x)
   (ss x 1.0 (improve 1.0 x)))

;;;This new end test works better than the original end test for both large and small numbers. It generally works better with larger numbers than smaller numbers however.

;;;Exercise 1.8:

;;;Procedure that calculates the cubic root based on Newton's method for cube roots.

(define (square x)
   (* x x))

(define (improve2 guess radicand)
   (/ 
      (+ 
         (/ radicand (square guess)) 
         (* 2 guess))
      3))

(define (cuberoot radicand guess btrguess)
   (if (guess-good? guess btrguess)
      btrguess
      (cuberoot radicand btrguess (improve2 btrguess radicand))))

(define (cuberoot-iter radicand)
   (cuberoot radicand 1.0 (improve2 1.0 radicand)))
