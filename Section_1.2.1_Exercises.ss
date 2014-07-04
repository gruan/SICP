;;;Section 1.2.1 Exercises:

;;;Exercise 1.9

;;;The first procedure:

;;;(define (+ a b)
;;;  (if (= a 0)
;;;      b
;;;      (inc (+ (dec a) b))))

;;;is a recursive process, due to the fact that the interpreter must keep track of deferred operations. 

;;;(+ 4 5)
;;;(inc (+ 3 5))
;;;(inc (inc (+ 2 5)))
;;;(inc (inc (inc (+ 1 5))))
;;;(inc (inc (inc (inc (+ 0 5)))))
;;;(inc (inc (inc (inc (5)))))
;;;(inc (inc (inc (6))))
;;;(inc (inc (7)))
;;;(inc (8))
;;;9

;;;The second procedure:

;;;(define (fact-iter product counter max-count)
;;;  (if (> counter max-count)
;;;      product
;;;      (fact-iter (* counter product)
;;;                 (+ counter 1)
;;;                 max-count)))

;;;is an iterative process, because the parameter values are the only values that the interpreter must keep track of. These could be called 'state values' in that they are passed from one state of the process to another.

;;;(+ 4 5)
;;;(+ 3 6)
;;;(+ 2 7)
;;;(+ 1 8)
;;;(+ 0 9)
;;;9

;;;Exercise 1.10

;;;(A 1 10)
;;;(A 0 (A 1 9))
;;;(A 0 (A 0 (A 1 8)))
;;;(A 0 (A 0 (A 0 (A 1 7))))
;;;(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (2))))))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (4)))))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (8))))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (16)))))))
;;;(A 0 (A 0 (A 0 (A 0 (A 0 (32))))))
;;;(A 0 (A 0 (A 0 (A 0 (64)))))
;;;(A 0 (A 0 (A 0 (128))))
;;;(A 0 (A 0 (256)))
;;;(A 0 (512))
;;;1024

;;;Notice that 1024 is 2^10. We can assume that (A 1 n) = 2^n.

;;;(A 2 4)
;;;(A 1 (A 2 3))
;;;(A 1 (A 1 (A 2 2)))
;;;(A 1 (A 1 (A 1 (A 2 1))))
;;;(A 1 (A 1 (A 1 2)))
;;;(A 1 (A 1 (A 0 (A 1 1))))
;;;(A 1 (A 1 (A 0 2)))
;;;(A 1 (A 1 4))
;;;(A 1 (A 0 (A 1 3)))
;;;(A 1 (A 0 (A 0 (A 1 2))))
;;;(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;;;(A 1 (A 0 (A 0 (A 0 2))))
;;;(A 1 (A 0 (A 0 4)))
;;;(A 1 (A 0 8))
;;;(A 1 16)
;;;   .
;;;   .
;;;   .
;;;(A 0 32768)
;;;65536

;;;Notice that (A 2 4) = (A 1 16) and 2^4 = 16

;;;(A 3 3)
;;;(A 2 (A 3 2))
;;;(A 2 (A 2 (A 3 1)))
;;;(A 2 (A 2 2))
;;;(A 2 (A 1 (A 2 1)))
;;;(A 2 (A 1 2))
;;;(A 2 (A 0 (A 1 1)))
;;;(A 2 (A 0 2))
;;;(A 2 4)
;;;65536

;;;(f n) = 2n
;;;(g n) = 2^n
;;;(h n) = 2^n^n
;;;(k n) = 5*n^2

;;;ILY DAX
