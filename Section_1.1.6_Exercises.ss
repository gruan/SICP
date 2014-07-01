;;;Section 1.1.6 Exercises.

;;;Exercise 1.1:

;;;The output for the input given in the exercise.

10
12
8
3
6
19
#f
3
16
6
16

;;;Exercise 1.2:

;;;Scheme notation for (5+4+(2-(3-(6+(4/5))))) / (3*(6-2)*(2-7))

(/ (+ 5 4 
      (- 2
         ( -3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;;Exercise 1.3:

;;;Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (square a) (*a a))
(define (sum-of-square a b) (+ (square a) (square b)))

(define (larger-sum-of-square x y z)
   (cond ((and (< x y) (< x z)) sum-of-square y z)
	 ((and (< y x) (< y z)) sum-of-square x z)
	 (else sum-of-square x y)))

;;;Exercise 1.4:

;;;Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

;;;(define (a-plus-abs-b a b)
;;;((if (> b 0) + -) a b))

;;;The abstraction of a procedure defines a compound procedure with the name "a-plus-abs-b" with formal parameters 'a' and 'b'. 
;;;As Scheme uses an applicative-order evaluation method, it evaluates its arguments first which are 'a' and 'b', both of which evaluate to 'a' and 'b'.
;;;Then it looks at the if statement: (if (> b 0) + - ). Since if statements are evaluated using a special evaluation system we look if (> b 0) is true or false.
;;;If b is greater than 0 the expression is true and we simply use the + operation in the expression (+ a b).
;;;If b is less than 0 the expression is false and we use the - operation in the expression (- a b) and since b is negative it actually adds a positive number 'b' to a.

;;;Exercise 1.5:

;;;Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is. Using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

;;;(define (p) (p))
;;;(define (test x y)
;;;(if (= x 0)
;;;0
;;;y))

;;;Then he evaluates the expression (test 0 (p))
;;;What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.

;;;Using Applicative-order evaluation - It will try to evaluate 0 and (p) but (p) runs (p) so it enters an infinite loop trying to evaluate (p).
;;;Using Normal-order evaluation - It expands test which gives (if (= x 0) 0 y) and it evaluates the if statement first and since x = 0, the output is 0.
