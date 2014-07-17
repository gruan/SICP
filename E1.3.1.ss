;;;Section 1.3.1 Exercises

;;;Context of the section.

;Summation procedure template.

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) 
       (sum term (next a) next b))))

;Integral function based off of sum procedure template where f = cube.

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;Calculates the cube of a number.

(define (cube x)
  (* x x x))

;;;Exercise 1.29

;Simpson's rule using summation procedure template where f = cube.

(define (simpsons a b n) ;Wrapper procedure for simpsons.
  (define h (/ (- b a) n))
  
  (define (ss) ;Calculates the summation.
    (define (add x) (+ x 1)) ;Adds 1 to a in the summation template procedure.

    (define (function x) ;Performs the actual different calculations.
      (define y (+ a (* x h))) 

      (cond ((or (= x 0) (= x n)) (cube y)) ;Changes coefficient based on what n is.
	    ((= (modulo x 2) 1) (* 4 (cube y)))
	    (else (* 2 (cube y)))))

    (/ (* h (sum-acc function 0.0 add n)) 
       3))

  (ss))

;;;Exercise 1.30

;Implements iterative recursive procedure for summation.

(define (sum-iter term a next b)
  (define (iter a result)
    (if (a > b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

;;;Exercise 1.31

;a

;Recursive procedure template for multiplying successive numbers.

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

;Factorial procedure using the product template.

(define (factorial n)
  (define (identity x) x)
  (define (add x) (+ x 1))
  (product identity 1 add n))

;Calculates pi/4 using the product template.

(define (pi/4) ;Wrapper procedure.
  (define (pp n)
    (define (function x) ;Performs the operation.
      (define first (/ x (- x 1)))
      (define second (/ x (+ x 1)))
      (cond ((= x 2) second)
	    (else (* first second))))

    (define (next x) (+ x 2)) ;Increments x.
  
    (product-acc function 2.0 next n)) ;Calls product template.
  (pp 100)) ;Stops when x reaches 100.

;b

;Iterative procedure template for multiplying successive numbers.

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

;;;Exercise 1.32

;a

;Recursive accumulate procedure template.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate combiner null-value term (next a) next b))))

;Product procedure template based on accumulate procedure template.

(define (product-acc term a next b)
  (accumulate-iter * 1 term a next b))

;Sum procedure template based on accumulate procedure template.

(define (sum-acc term a next b)
  (accumulate-iter + 0 term a next b))

;b

;Iterative recursive accumulate procedure template.

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;;;Exercise 1.33

;Iterative recursive filtered accumulate procedure template.

(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (iter a result)
    (if (filter? a) ;If filter? is satisfied accumulates the value
      (if (> a b)
	result
	(iter (next a) (combiner (term a) result)))
      (iter (next a) result))) ;OIf filter? is false continue to next value of a
    (iter a null-value))

;a

;Sum of squares of prime numbers in the interval a to b.

(define (sum-of-squares a b)
  (define (function x) (square x))
  (define (inc x) (+ x 1))
  (filtered-accumulate prime? + 0 function a inc b))

;Squares a number.

(define (square x)
  (* x x))

;Checks if a number is prime in O(sqrt(n)).

(define (prime? n)
  (= (smallest-divisor n) n))

;Finds the smallest divisor of a number.

(define (smallest-divisor n) ;Wrapper procedure.
  (define (find-divisor test-divisor)
    
    (define (next) ;Determines next number to test for find-divisor.
      (if (= test-divisor 2) (+ test-divisor 1) (+ test-divisor 2)))
    
    (define (divides?) ;Checks if n is divisible by test-divisor.
      (= (remainder n test-divisor) 0))

    (cond ((> (square test-divisor) n) n) ;Finds smallest divisor of n.
	  ((divides?) test-divisor)
	  (else (find-divisor (next)))))

  (find-divisor 2))

;b

;The product of all the positive integers less than n that are relatively prime to n
(define (relative-prime-product n)
  (define (rel-prime? x) ;Checks if GCD(x,n) = 1
    (= (gcd x n) 1))
  (define (identity x) x) 
  (define (inc x) (+ x 1)) ;Increment x by 1
  (filtered-accumulate rel-prime? * 1 identity 1 inc (- n 1)))
