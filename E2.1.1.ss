;;;Section 2.1.1 Exercises:

;Exercise 2.1

;Change the sign of a number.

(define (change-sign x)
  (- 0 x))

;Constructor for a rational number.

(define (make-rat n d)
  (let ((g (gcd n d))) ;Simplifies the rational number.
    (if (negative? d) ;Corrects signs in the rational number.
      (cons (/ (change-sign n) g) (/ (change-sign d) g))
      (cons (/ n g) (/ d g)))))

;Numerator selector for a rational number.

(define (numer x) (car x))

;Denominator selector for a rational number.

(define (denom x) (cdr x))

;Display a rational number.

(define (display-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))
