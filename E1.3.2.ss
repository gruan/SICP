;;;Section 1.3.2 Exercises:

;;;Exercise 1.34

(define (f g)
  (g 2))

;The interpreter spits out error since you feed the first 'f' an 'f' which is a procedure but that 'f' is given the argument of 2 which is a number and not a procedure. Since 'f' only accepts a procedure as an argument you get an error.
