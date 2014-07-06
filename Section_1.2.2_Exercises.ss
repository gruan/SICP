;;;Section 1.2.2 Exercises:

;;;Exercise 1.11

;;;Recursive process

(define (ff n)
   (define (recursive)
      (+ (ff (- n 1)) (* 2 (ff (- n 2))) (* 3 (ff (- n 3)))))
   (if (< n 3)
      n
      (else (recursive))))

;;;Iterative Process

(define (f a b c count)
   (if (< count 3)
      a
      (else (f (+ a (* 2 b) (* 3 c)) a b (- count 1)))))

(define (ff n)
   (f 2 1 0 n))

;;;Exercise 1.12

;;;Rows and columns both start at 0.

(define (pascal row column)
   (cond ((or (< row 0) (< column 0) (< row column)) 0)
         ((= row 0) 1)
         ((= row column) 1)
         (else (+ (pascal (- row 1) column) (pascal (- row 1) (- column row))))))

;;;Exercise 1.13

;;;For simplicity on the digital media, let phi = P and psi = S

;;;Prove that Fib(n) is the closest integer to P^n/(sqrt(5)), where P = (1 + sqrt(5))/2.
;;;Hint: Let S = (1 - sqrt(5))/2. Use induction and the definition of the Fibonacci numbers to prove that Fib(n) = (P^n - S^n)/sqrt(5).

;;;Notice that:

;;;P^2 = P + 1 and 1/P + 1 = P.
;;;S^2 = S + 1 and 1/S + 1 = S.

;;;The definition of Fib(n) is as follows:

;;;Fib(n) = 0 if n = 0
;;;Fib(n) = 1 if n = 1
;;;Fib(n) = Fib(n - 1) + Fib(n - 2) Otherwise

;;;To prove that Fib(n) = (P^n - S^n)/sqrt(5) let us check that it is true for n = 0, 1, 2.

;;;n = 0
;;;Fib(0) = 0
;;;(1 - 1)/sqrt(5) = 0

;;;n = 1
;;;Fib(1) = 1
;;;((1 + sqrt(5))/2 - (1 - sqrt(5))/2)/sqrt(5) = (2 * sqrt(5))/sqrt(5) = 1

;;;n = 2
;;;Fib(2) = 2
;;;((1 + sqrt(5))/2 + 1 - ((1 - sqrt(5))/2 + 1))/sqrt(5) = (2 * sqrt(5))/sqrt(5) = 1
;;;Used the property that P^2 = P + 1 and S^2 = S + 1

;;;Let us assume that for n = 0, 1, 2 ... ,k it is true that Fib(n) = (P^n - S^n)/sqrt(5).
;;;Then let us set n = k + 1.

;;;(P^(k+1) - S^(k+1))/sqrt(5) = (P^k - S^k)/sqrt(5) + (P^(k-1) - S^(k-1))/sqrt(5)
;;;	= (P^k - S^k + P^(k-1) - S^(k-1))/sqrt(5)
;;;	= (P^k + P^(k-1) - S^k - S^(k-1))/sqrt(5)
;;;	= ((P^k + P^(k-1)) - (S^k + S^(k-1)))/sqrt(5)
;;;	= ((P^(k+1) * (P^(-1) + P^(-2))) - (S^(k+1) * (S^(-1) + S^(-2))))/sqrt(5)
;;;	= ((P^(k+1) * P^(-1) * (1 + P^(-1))) - (S^(k+1) * S^(-1) * (1 + S^(-1))))/sqrt(5)
;;;Using the property that 1/P + 1 = P and 1/S + 1 = S we get the following:
;;;	= ((P^(k+1) * P^(-1) * P) - (S^(k+1) * S^(-1) * S))/sqrt(5)
;;;	= (P^(k+1) - S^(k+1))/sqrt(5)

;;;Thus, by proof of induction, we know that Fib(n) = (P^n - S^n)/sqrt(5).

;;;Let us re-arrange the equation:

;;;Fib(n) - P^n/sqrt(5) = -(S^n/sqrt(5))

;;;We know that Fib(n) is the closest integer to P^n/sqrt(5) if Fib(n) - P^n/sqrt(5) < 1/2.
;;;Thus we must prove that

;;;- (S^n/sqrt(5)) < 1/2.

;;;Re-arrange it to:

;;;-(S^n) < sqrt(5)/2

;;;We can evaluate S since S = (1 - sqrt(5))/2 ~ -0.618
;;;Since S < 1. We know
;;; -(S^n) < 1 for all n > 0.

;;;We can then evaluate sqrt(5)/2 ~ 1.118

;;;Since -(S^n) < 1 and sqrt(5)/2 > 1 then
;;;-(S^n) < sqrt(5)/2.

;;;Thus Fib(n) must be the closest integer possible to P^n/sqrt(5).
;;;QED.
