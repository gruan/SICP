;;;Section 2.2.3 Exercises:

;Context

;Squares a number.

(define (square x)
  (* x x))

;Accumulations template.

(define (accumulate op end-value sequence)
  (if (null? sequence)
    end-value
    (op (car sequence)
	(accumulate op end-value (cdr sequence)))))

;Exercise 2.33

;Basic list-manipulation operations as accumulations.

(define (map-acc p sequence) ;Map.
  (accumulate (lambda (x y) (cons (p x) y))
	      '()
	      sequence))

(define (append-acc seq1 seq2) ;Append.
  (accumulate cons seq2 seq1))

(define (length-acc sequence) ;Length.
  (accumulate (lambda (x y) (+ 1 y))  0 sequence))

;Exercise 2.34

;Evaluates Horner's Rule using accumulate.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;Exercise 2.35

;Counts the amount of elements of a list and its sublists.

(define (count-leaves tree)
  (accumulate +
	      0
	      (map (lambda (x) 1)
		   (enumerate tree))))

;Makes a list of all the leaves of a tree in their corresponding order.

(define (enumerate tree)
  (cond ((null? tree) tree)
	((not (pair? tree)) (list tree))
	(else
	  (append (enumerate (car tree))
		  (enumerate (cdr tree))))))

;Exercise 2.36

;Applies the designated accumulation procedure to combine all the first elements of the sequences,
;all the second elements of the sequences, and so on, and returns a sequence of the result.

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (same-element seqs))
	  (accumulate-n op init (exclude seqs)))))

;Takes a list as an argument and returns a list of all of the 1st elements.

(define (same-element seq)
  (if (null? seq)
    '()
    (cons (caar seq) (same-element (cdr seq)))))

;Takes a list as an argument and returns the same list excluding all of the 1st elements.

(define (exclude seq)
  (if (null? seq)
    '()
    (cons (cdar seq) (exclude (cdr seq)))))

;Exercise 2.37

;Matrix Algebra.

(define (dot-product v w) ;Dot product of vectors v and w.
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) ;Multiplies a matrix and a vector.
  (map dot-product m (vector-matrix m v)))

;Expands a vector into a simlaryly sized matrix as m. Ex. (2 2 2 2)
;((2 2 2 2) (2 2 2 2) (2 2 2 2))

(define (vector-matrix m v) 
  (if (null? m) 
    '()
    (cons v (vector-matrix (cdr m) v))))

(define (transpose mat) ;Transpose matrix.
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n) ;Multiply 2 matrices.
  (let ((cols (transpose n))) ;Transpose second matrix in preparation for multiplication.
   
    (define (mat-*-vec vec) ;Multiplies a matrix and a vector without specifying the matrix.
      (matrix-*-vector cols vec))
    
    (map mat-*-vec ;Multiplies cols to each vector of m to produce the new matrix.
	 m)))

;Exercise 2.38

;Combines the first element of the sequence with the result of combining all the elements
;to the left.

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
	    (cdr rest))))
  (iter initial sequence))

;Combines the first element of the sequence with the result of combining all the elements
;to the right.

(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(fold-right op initial (cdr sequence)))))

;op needs to be commutative for fold-left and fold-right to give the same answers.

;Exercise 2.39

;Takes a list as an argument and outputs a list with the elements in reversed order.
;Using fold-right.

(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (append y (list x)))
	      '() 
	      sequence))

;Takes a list as an argument and outputs a list with the elements in reversed order.
;Using fold-left.

(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (append (list y) x))
	     '()
	     sequence))

;Exercise 2.40

;Takes as an argument an integer n. Returns a list of pairs (i,j) where 1 <= j < i <= n.

(define (prime-ordered-pairs n)
  (accumulate append
	      '()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

;Takes two integer arguments and returns a list of numbers from low to hi inclusive.

(define (enumerate-interval low hi)
  (if (> low hi)
    '()
    (cons low (enumerate-interval (+ low 1) hi))))

;Combination of mapping and accumulating with append.

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;Takes as an argument a list of two numbers and determines if the sum of the numbers is prime.

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;Takes as an argument a number and returns the smallest-divisor.

(define (smallest-divisor n) ;Wrapper procedure
  (define (find-divisor n test-divisor) ;Finds smallest divisor.
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? test n) ;Checks to see if a number is a divisor of another number.
    (= (remainder n test) 0))

  (find-divisor n 2)) ;Call starts at 2.

;Takes as an argument a number and checks to see if it is prime.

(define (prime? n)
  (= (smallest-divisor n) n))

;Takes a list of two numbers as an argument and returns a list of the two numbers and their sum.

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;Given a positive integer n, finds all the ordered pairs of distinct positive integers i and j
;where 1 <= j < i <= n, such that i+j is prime.

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))
;Takes a list as an argument and returns a list of all of the permutations.

(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
	       (map (lambda (p) (cons x p))
		    (permutations (myremove x s))))
	     s)))

;Takes a item and list as an argument and returns the same list excluding the item.

(define (myremove item sequence)
  (filter (lambda (x) (not (= x item)))
	  sequence))

;Takes an integer as an argument and generates the sequence of pairs (i,j) with 1 <= j < i <= n.

(define (unique-pairs n)
  (flatmap (lambda (x)
	     (map (lambda (y) (list x y))
		  (enumerate-interval 1 (- x 1))))
	   (enumerate-interval 1 n)))

;Exercise 2.41

;Takes an integer n and an integer s and finds all ordered triples of distinct positive integers
;i,j, and k less than or equal to a given integer n that sum to a given integer s.

(define (triples-equal-sum n s)
  (define (triple-sum-equal? triple) ;Determines if the sum of triple is = to s.
    (= s (sum-triple triple)))

  (filter triple-sum-equal?
	  (unique-triples n)))

;Takes an integer n as an argument and generates a list of triples (i,j,k) where i,j,k are all < n.

(define (unique-triples n)
    (flatmap (lambda (i)
	       (flatmap (lambda (j)
			  (map (lambda (k) (list i j k))
			       (myremove i (myremove j (enumerate-interval 1 n))))) ;Removes i and j.
			(myremove i (enumerate-interval 1 n)))) ;Removes i.
	     (enumerate-interval 1 n)))

;Takes a triple list (i,j,k) as an argument and returns i+j+k.

(define (sum-triple triple)
  (+ (car triple) (cadr triple) (caddr triple)))

;Exercise 2.42

;Takes an integer n as an argument and returns a sequence of all solutions to the 8-queens 
;problem of placing n queens on an nxn chessboard.

(define (queens board-size)
  (define (queen-cols k) ;Returns the sequence of all ways to place queens in the first k cols.
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) (safe? k positions))
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

;Queen's position constructor.

(define (make-position row col)
  (append row col))

;Queen's position selector.

(define (position-row position) ;Row.
  (car position))

(define (position-col position) ;Column.
  (cadr position))

;Adjoins a new queen position (row, column) to a set of positions.

(define (adjoin-position row rest-of-queens)
  (make-position rest-of-queens (list row)))

;Represents an empty set of positions.

(define empty-board '())

;Determines for a set of positions, whether the queen in the kth column is safe with respect to
;others.

(define (safe? cur-k positions)
  (define cur-pos (- cur-k 1))
  (define cur-queen (list-ref positions cur-pos))
  
  (define (iter old-k positions)
    (define old-pos (- old-k 1))
    (define old-queen (list-ref positions old-pos))
    
    (if (< old-pos cur-pos)
      (and (touches? cur-k cur-queen old-k old-queen) (iter (+ old-k 1) positions))
      #t))
  
  (iter 1 positions))


;Determines whether the current-queen touches any other previous queens.

(define (touches? cur-k cur-queen old-k old-queen)
  (not (or (= (+ cur-k cur-queen) (+ old-k old-queen)) ;Test diagonal bottom left to upper right
	   (= (- cur-k cur-queen) (- old-k old-queen)) ;Test diagonal upper left to bottom right
	   (= cur-queen old-queen)))) ;Tests for the same row.

;Takes a list of solutions as an argument and returns the number of solutions.

(define (count-solutions solutions)
  (accumulate (lambda (x y) (+ 1 y))
	      0
	      solutions))

;Exercise 2.43

;Takes T^boardsize time to execute.
