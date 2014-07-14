;;;Section 1.2.5 Exercises:

;;;Exercise 1.20

;;;Euclid's Algorithm expressed in an iterative recursive procedure in O(log(n))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;;;Describe (gcd 206 40) using the normal-order-evaluation rule.

;;;(gcd 206 40)

;;;(if (= 40 0)
;;;  206
;;;  (gcd 40 (remainder 206 40)))

;;;(gcd 40 (remainder 206 40))

;;;(if (= (remainder 206 40) 0)
;;;  40
;;;  (gcd (remainder 206 40)
;;;       (remainder 40 (remainder 206 40))))

;;;(if (= 6 0) ;; 1 remainder evaluated
;;;  40
;;;  (gcd (remainder 206 40)
;;;       (remainder 40 (remainder 206 40))))

;;;(gcd (remainder 206 40)
;;;     (remainder 40 (remainder 206 40)))

;;;(if (= (remainder 40 (remainder 206 40)) 0)
;;;  (remainder 206 40)
;;;  (gcd (remainder 40 (remainder 206 40))
;;;       (remainder (remainder 206 40)
;;;                  (remainder 40 (remainder 206 40)))))

;;;(if (= 4 0) ;; 2 remainders evaluated
;;;  (remainder 206 40)
;;;  (gcd (remainder 40 (remainder 206 40))
;;;       (remainder (remainder 206 40)
;;;                  (remainder 40 (remainder 206 40)))))

;;;(gcd (remainder 40 (remainder 206 40))
;;;     (remainder (remainder 206 40)
;;;                (remainder 40 (remainder 206 40))))

;;;(if (= (remainder (remainder 206 40)
;;;                  (remainder 40 (remainder 206 40))) 0)
;;;  (remainder 40 (remainder 206 40))
;;;  (gcd (remainder (remainder 206 40)
;;;                  (remainder 40 (remainder 206 40)))
;;;       (remainder (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40))))))

;;;(if (= 2 0) ;;4 remainders evaluated
;;;  (remainder 40 (remainder 206 40))
;;;  (gcd (remainder (remainder 206 40)
;;;                  (remainder 40 (remainder 206 40)))
;;;       (remainder (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40))))))

;; (gcd (remainder (remainder 206 40)
;;;                (remainder 40 (remainder 206 40)))
;;;     (remainder (remainder 40 (remainder 206 40))
;;;                (remainder (remainder 206 40)
;;;                           (remainder 40 (remainder 206 40)))))

;;;(if (= (remainder (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40)))) 0)
;;;  (remainder (remainder 206 40)
;;;             (remainder 40 (remainder 206 40)))
;;;  (gcd (remainder (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40))))
;;;       (remainder (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 40 (remainder 206 40))
;;;                             (remainder (remainder 206 40)
;;;                                        (remainder 40 (remainder 206 40)))))))

;;;(if (= 0 0) ;;7 remainders evaluated
;;;  (remainder (remainder 206 40)
;;;             (remainder 40 (remainder 206 40)))
;;;  (gcd (remainder (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40))))
;;;       (remainder (remainder (remainder 206 40)
;;;                             (remainder 40 (remainder 206 40))
;;;                  (remainder (remainder 40 (remainder 206 40))
;;;                             (remainder (remainder 206 40)
;;;                                        (remainder 40 (remainder 206 40)))))))

;;;(remainder (remainder 206 40)
;;;           (remainder 40 (remainder 206 40)))

;;; 2 ;;4 remainders evaluated

;;;18 remainders evaluated total


;;;Describe (gcd 206 40) using the applicative-order-evaluation rule.

;;;(gcd 206 40)

;;;(if (= 40 0)
;;;  206
;;;  (gcd 40 (remainder 206 40)))

;;;(gcd 40 6) ;;1 remainder evaluated

;;;(if (= 6 0)
;;;  40
;;;  (gcd 6 (remainder 40 6)))

;;;(gcd 6 4) ;;1 remainder evaluated

;;;(if (= 4 0)
;;;  6
;;;  (gcd 4 (remainder 6 4)))

;;;(gcd 4 2) ;;1 remainder evaluated

;;;(if (= 2 0)
;;;  2
;;;  (gcd 2 (remainder 4 2)))

;;;(gcd 2 0) ;;1 remainder evaluated

;;;(if (= 0 0)
;;;  2
;;;  (gcd 0 (remainder 2 0))

;;;2

;;;4 remainders evaluated total
