;;;Section 2.1.1 Exercises:

;Exercise 2.2

;Prints the coordinates of the starting and end point of a line segment.

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;Constructor of points.

(define (make-point x y) (cons x y))

;Selector of point, x-coordinate.

(define (x-point point) (car point))

;Selector of point, y-coordinate.

(define (y-point point) (cdr point))

;Constructor of line-segment.

(define (make-segment startpoint endpoint)
  (cons startpoint endpoint))

;Selector of segments, startpoint.

(define (start-segment segment) (car segment))

;Selector of segments, endpoint.

(define (end-segment segment) (cdr segment))

;Selector of segments, midpoint.

(define (midpoint-segment segment)
  (let ((x1 (x-point (start-segment segment)))
	(y1 (y-point (start-segment segment)))
	(x2 (x-point (end-segment segment)))
	(y2 (y-point (end-segment segment))))
    (make-point (average x1 x2) (average y1 y2))))

;Selector of segments, magnitude.

(define (magnitude-segment segment)
  (let ((x1 (x-point (start-segment segment)))
	(y1 (y-point (start-segment segment)))
	(x2 (x-point (end-segment segment)))
	(y2 (y-point (end-segment segment))))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))
	  

;Averages 2 numbers.

(define (average x y)
  (/ (+ x y) 2))

;Exercise 2.3

;Rectangle constructor based on 3 points.
;Assumes that the points are given in clockwise or counterclockwise fashion.

(define (make-rectangle a b c) 
  (cons (make-segment a b) (make-segment b c)))

;Rectangle selectors:

(define (rect-width x) ;Width
  (if (= (y-point (start-segment (car x))) 
	 (y-point (end-segment (car x))))
    (magnitude-segment (car x))
    (magnitude-segment (cdr x))))

(define (rect-height x) ;Height
  (if (= (x-point (start-segment (car x)))
	 (x-point (end-segment (car x))))
    (magnitude-segment (car x))
    (magnitude-segment (cdr x))))

(define (rect-perimeter x) ;Perimeter
  (+ (* 2 (rect-width x)) 
     (* 2 (rect-height x))))

(define (rect-area x) ;Area
  (* (rect-width x) (rect-height x)))

;Second rectangle constructor based on 2 corner points of a rectangle.

(define (make-rectangle2 a b)
  (define c (make-point (x-point a) (y-point b)))
  (cons (make-segment a c) (make-segment b c)))
