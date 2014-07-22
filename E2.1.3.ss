;;;Section 2.1.3 Exercises:

;Exercise 2.4

;Constructor of pairs using procedures.

(define (cons-procedure x y)
  (lambda (m) (m x y)))

;Selectors of pairs using procedures

(define (car-procedure z) ;First value of the pair
  (z (lambda (p q) p)))

(define (cdr-procedure z) ;Second value of the pair
  (z (lambda (p q) q)))

;(car (cons x y)) works like this:
;(car (lambda (m) (m x y)))
;((lambda (m) (m x y)) (lambda (p q) p))
;((lambda (p q) p) x y)
;x

;Exercise 2.5

;Pair constructor using only numbers and arithmetic operations.

(define (cons-arithmetic a b)
  (* (expt 2 a) (expt 3 b)))

;Pair selectors using only numbers and arithmetic operations.

(define (car-arithmetic x) ;First value of pair.
  (define (counter count x)
    (if (odd? x) ;3^n is always odd so this works as end case test.
      count
      (counter (+ count 1) (/ x 2))))
  (counter 0 x))

(define (cdr-arithmetic x) ;Second value of pair.
  (define (counter count x)
    (if (even? x)
      (counter count (/ x 2))
      (if (= x 1) ;x is now 3^b.
	count
	(counter (+ count 1) (/ x 3)))))
  (counter 0 x))

;Exercise 2.6

;Church's numerals.

(define (inc x) ;Procedure to pass through Church numerals
  (+ x 1))

(define zero (lambda (g) (lambda (a) a))) ;Definition of zero.

(define (add-1 n) ;Adds 1.
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(lambda (f) (lambda (x) (f (((lambda (g) (lambda (a) a)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (a) a) x))))
;(lambda (f) (lambda (x) (f x)))

(define one (lambda (g) (lambda (a) (g a)))) ;Definition of one. Applies procedure once.

(define two (lambda (g) (lambda (a) (g (g a))))) ;Definition of two. Applies procedure twice.

(define (add x y) ;Definition of addition.
  (lambda (g) (lambda (a) ((x g) ((y g) a)))))
