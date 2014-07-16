;;;Section 1.2.6 Exercises:

;;;Exercise 1.21

;;;Squares a number

(define (square x)
  (* x x))

;;;Checks if a number is prime
;;;Process is O(sqrt(n))

(define (prime? n)
  (= (smallest-divisor n) n))

;;;Checks for the smallest divisor of a number

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (define (divides?)
      (= (remainder n test-divisor) 0))
    (cond ((> (square test-divisor) n) n)
	  ((divides?) test-divisor)
	  (else (find-divisor (next test-divisor)))))
  (find-divisor 2))

;;;Smallest divisor of 199, 1999, and 19999 are 199, 1999, and 7

;;;Exercise 1.22

;;;Defines runtime to be used in timed processes

(define (runtime)
  (current-milliseconds))

;;;Prints the number and checks if it's prime. If it is, it prints the time needed to check the number for primality.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;;Searches for the three smallest prime numbers after the given number.

(define (search-for-primes n)
  (define (ss n count)
    (if (= count 3)
      (display "\nEnd of program")
    (cond ((even? n) (ss (+ n 1) count))
	  ((prime? n) (timed-prime-test n)
		      (ss (+ n 2) (+ count 1)))
	  (else (ss (+ n 2) count)))))
  (ss n 0))

;;;Had to use bigger numbers 10,000,000,000 and 100,000,000,000 but the order of growth of O(sqrt(n)) seems to be accurate.
;;;Seems that machines do run in time proportional to the number of steps needed for a computation

;;;Exercise 1.23

;;;Implements a procedure that reduces unnecessary checking in the procedure 'smallest-divisor'

(define (next n)
  (if (= n 2) 
    3
    (+ n 2)))

;;;It seems that the time for the processes to run does get shorter by very close to 1/2 but not quite. I think this is because you are replacing a primitive operation by a user-created one. Not to mention that you have to evaluate another if statement.

;;;Exercise 1.24

;;;Prints a number and checks if it's prime based on Fermat's Little Theorem and if the number is prime it prints the time needed to check it.

(define (fast-timed-prime-test n)
  (newline)
  (display n)
  (fast-start-prime-test n (runtime)))

(define (fast-start-prime-test n start-time)
  (cond ((fast-prime? n 100) (fast-report-prime (- (runtime) start-time)))))

(define (fast-report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;;Determines if a number is prime based on Fermat's Little Theorem.
;;;Process is O(log(n))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test? n) (fast-prime? n (- times 1)))
	(else false)))

;;;Performs Fermat's Little Theorem which states that "If 'n' is a prime number and 'a' is any positive integer less than 'n', then 'a' raised to the 'n'th power is congruent to a % n.

(define (fermat-test? n)
  (define (try-it? a)
    (= (expmod a n n) a))
  (try-it? (+ (random (- n 1)))))

;;;Computes the exponential of a number % another number

(define (expmod base pwr m)
  (cond ((= pwr 0) 1)
	((even? pwr)
	 (remainder (square (expmod base (/ pwr 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- pwr 1) m))
		     m))))

;;;The time needed to check primes definitely increases logarithmically. The test using Fermat's Little Theorem is much more efficient than the old one.

;;;Exercise 1.25

;Alyssa P. Hacker's version of the procedure expmod

; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))

;It is true that we could have wrote the procedure like this and it would have worked. However, this procedure directly evaluates the modulo of the exponential of a number by another number. This would mean that we would have to deal with modulo evaluation with huge numbers.
;On the other hand, our expmod procedure recursively evaluates the modulo of the exponential of a number by another number. If we have any x y and m. (x * y) % m = (x % m) * (y % m). This means that we don't have to deal with huge numbers when we do our modulo evaluations and makes the entire process run faster.
;Thus it is preferred to use our expmod procedure in place of Alyssa P. Hacker's expmod procedure.

;;;Exercise 1.26

;Due to the interpreter using applicative-order evaluation, our version of expmod is O(log(n)) because the square procedure isn't evaluated until the next expmod is evaluated. Thus, expmod is evaluated into an integer before square is evaluated.
;However, Louis Reasoner's version of expmod will make the interpreter evaluate both expmods. This means that twice the evaluations are performed every time expmod is called. 
;This makes the process O((log(n)^2)) which is O(n).

;;;Exercise 1.27

;Tests Fermat's Little Theorem for every a < n.

(define (test-fermat n) ;Wrapper for test-ff
  (define (test-ff count) ;Checks all numbers a < n for Fermat's test to determine primality of a number
    (define (ff?)
      (= (expmod count n n) count)) ;Checks if count^n % n = count
    (cond ((= count n) true)
	  ((ff?) (test-ff (+ count 1)))
	  (else false)))
  (test-ff 1))

;;;Exercise 1.28

;Uses the Miller-Rabin test to determine whether a number is prime or not

(define (miller-test n) ;Wrapper procedure
  (define (prime? times) ;
    (cond ((= times 0) true)
	  ((mm?) (prime? (- times 1)))
	  (else false)))
  (define (mm?) ;Wrapper for try-it?
    (define (try-it? a) ;Tests a random number 'a' according to the Miller-Rabin test
      (= (expmod a (- n 1) n) 1))
    (try-it? (+ (random (- n 1)))))
  (define (expmod base pwr m) ;Calculates the modulo of a^(n-1) and n
    (cond ((= pwr 0) 1)
	  ((even? pwr)
	   (define x (square (expmod base (/ pwr 2 ) m))) 
	   (if (and 
		 (not (= x 1)) ;Checks if there is a nontrivial square root of 1 modulo n
		 (not (= x (- n 1)))
		 (= (square x) 1))
	     0
	     (remainder x m)))
	  (else
	    (remainder (* base (expmod base (- pwr 1) m))
	      	       m))))
  (prime? 100)) ;Executes 100 tries with different 'a's
