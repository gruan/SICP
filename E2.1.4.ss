;;;Section 2.1.4 Exercises:

;Context

;Interval selectors.

(define (add-interval x y) ;Add two intervals.
  (make-interval (+ (interval-lower x) (interval-lower y)) 
		 (+ (interval-upper x) (interval-upper y))))

(define (old-mul-interval x y) ;Multiply two intervals.
  (let ((p1 (* (interval-lower x) (interval-lower y)))
	(p2 (* (interval-lower x) (interval-upper y)))
	(p3 (* (interval-upper x) (interval-lower y)))
	(p4 (* (interval-upper x) (interval-upper y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (old-div-interval x y) ;Divide two intervals.
  (mul-interval x 
		(make-interval (/ 1.0 (interval-upper y))
			       (/ 1.0 (interval-lower y)))))

;Exercise 2.7

;Interval constructor.

(define (make-interval a b) (cons a b))

;Interval selectors.

(define (interval-lower x) (car x)) ;Lower-bound of interval.

(define (interval-upper x) (cdr x)) ;Upper-bound of interval.

;Exercise 2.8

;Interval selectors.

(define (sub-interval x y) ;Subtracts two intervals.
  (make-interval (- (interval-lower x) (interval-upper y))
		 (- (interval-upper x) (interval-lower y))))

;Exercise 2.9

;Interval selectors.

(define (width-interval x) ;Width of the interval.
  (/ (- (interval-upper x) (interval-lower x)) 2.0))

;Test value interval x (35 75) and interval y (10 20).
;The width of x is 20 and the width of y is 5. So the sum of the widths of x and y is 25.
;Width of add-interval and sub-interval give 25.
;Width of mul-interval and div-interval give 575 and 2.875 respectively.

;Test value interval x (100 123) and interval y (50 67).
;The width of x is 11.5 and the width of y is 8.5. So the sum of the widths of x and y is 20.
;Width of add-interval and sub-interval give 20.
;Width of mul-interval and div-interval give 1620.5 and 0.48373134328358214 respectively.

;As you can see, the width of the sum or difference of two intervals is a function
;only of the widths of the intervals being added or subtracted.
;Whilst the width of the product or quotient of two intervals is not.

;Exercise 2.10

;Interval selectors.

(define (div-interval x y) ;Divides two intervals and signals an error if y spans over 0.
  (if (and (negative? (interval-lower y)) (positive? (interval-upper y)))
    (error "Error: Interval cannot span over 0." y)
    (old-div-interval x y)))

;Exercise 2.11

;Interval selectors.

(define (mul-interval x y) ;Divides two intervals using 9 cases.
  (let ((a (interval-lower x))
	(b (interval-upper x))
	(c (interval-lower y))
	(d (interval-upper y)))
   (cond ((and (positive? a) (positive? b) (positive? c) (positive? d)) ;Test 9 cases
	  (make-interval (* a c) (* b d)))
	 ((and (negative? a) (positive? b) (positive? c) (positive? d))
	  (make-interval (* a d) (* b d)))
	 ((and (positive? a) (positive? b) (negative? c) (positive? d))
	  (make-interval (* b c) (* b d)))
	 ((and (negative? a) (negative? b) (positive? c) (positive? d))
	  (make-interval (* a d) (* b c)))
	 ((and (positive? a) (positive? b) (negative? c) (negative? d))
	  (make-interval (* b c) (* a d)))
	 ((and (negative? a) (positive? b) (negative? c) (negative? d))
	  (make-interval (* b c) (* a c)))
	 ((and (negative? a) (negative? b) (negative? c) (positive? d))
	  (make-interval (* a d) (* a c)))
	 ((and (negative? a) (negative? b) (negative? c) (negative? d))
	  (make-interval (* b d) (* a c)))
	 (else ;Last case: test the four different intervals possible based on values given.
	   (let ((p1 (* a d))
		 (p2 (* b c))
		 (p3 (* a c))
		 (p4 (* b d)))
	     (make-interval (min p1 p2) (max p3 p4)))))))

;Exercise 2.12

;Center of interval and percentage tolerance constructor.

(define (make-center-percent center percent)
  (let ((x (- center (* center (/ percent 100.0))))
	(y (+ center (* center (/ percent 100.0)))))
    (make-interval x y)))

;Center of interval and percentage tolerance selectors.

(define (center x) ;Center of an interval.
  (/ (+ (interval-lower x) (interval-upper x)) 2))

(define (percent x) ;Percentage tolerance of an interval.
  (* (/ (- (interval-upper x) (center x)) (center x)) 100))

;Exercise 2.13

;Using an interval with center = 17 and percentage tolerance = 2 (16.66, 17.34) and
;another interval with center = 120 and percentage tolerance = 3 (116.4, 123.6)
;The product of these two intervals gives us (1939.224, 2143.224).
;This interval has a percentage tolerange of 4.997 which is pretty close to 2 + 3.

;Exercise 2.14

;Parallel Resistor Formulas

(define (par1 r1 r2) ;Alternate
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(define (par2 r1 r2) ;Original
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

;Using intervals:
;one, center = 1 percentage of tolerance = 0.
;t1, center = 100 percentage of tolerance = 1.
;t2, center = 300 percentage of tolerance = 2.
;t3, center = 100 percentage of tolerance = 3.
;t4, center = 100 percentage of tolerance = 4.

;We can see that whenever we multiply or divide an interval by one, the percentage of tolerance
;stays the same. However, when we multiply or divide an interval by another interval, we 
;approximately add the percentage of tolerances of the two intervals.

;Also, whenever we add an interval by one, the percentage of tolerance stays the
;same. However, when we add an interval by another interval, we get the average
;of the two intervals' percentage of tolerance plus or minus some constant based on the 
;ratio of the centers of the two intervals.

;In the first parallel resistor formula, we have (r1 * r2)/(r1 + r2).
;In the second parallel resistor formula, we have 1/((1/r1) + (1/r2)).

;If we assume p1 = percentage of tolerance of r1 and p2 = percentage of tolerance of p2.

;For the first parallel resistor formula, we can get an approximation of the resulting
;percentage of tolerance.

;p1 + p2 + (p1 + p2)/2 + c, where c is a constant based on the ratio of the centers of both
;intervals.

;For the second parallel resistor formula, we can get an approximation of the resulting
;percentage of tolerance as well.

;(p1 + p2)/2 + c, where c is a constant based on the ratio of the center of both intervals.

;As you can see the first parallel resistor formula has a larger percentage of tolerance 
;whilst the second parallel resistor formula has a smaller percentage of tolerance when
;both formulas are compared to each other.

;Exercise 2.15

;If given an interval A, A/A does not equal 1.

;If we transform Alyssa's equation to Lem's equation we can see why Alyssa's equation is better.

;Alyssa's: 1/((1/r1) + (1/r2)).
;Lem's: (r1 * r2)/(r1 + r2).

;1/(1/r1 + 1/r2)
;1/(r2/r2 * 1/r1 + 1/r2 * r1/r1) *****
;1/(r2/(r1*r2) + r1/(r1 * r2))
;1/((r1 + r2)/(r1 * r2))
;(r1 * r2)/(r1 + r2)

;The problem lies in the second line that is asterisked. We multiply the equation by r1/r1 and
;r2/r2 which should both equal 1. But since we are dealing with intervals and not simple
;numbers, r1/r1 and r2/r2 do not equal one but simply approximate to it, since A/A for any
;interval is never exactly 1.

;This is why Eva Lu Ator is right in saying that Alyssa's procedure is better since it doesn't
;involve any unnecessary transformations that involve interval divison of A/A.

;Exercise 2.16

;To be done...
