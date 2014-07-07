;;;Section 1.2.3 Exercises:

;;;Exercise 1.14

;;;The order of growth of the space will be O(n) since the space needed grows with the depth of the recursion tree which grows linearly with the size of 'amount'.
;;;The order of growth of the number of steps needed will be O(n^2) but will increase multiplicatively as well as the amount grows since the 'kinds of coins' will increase. However it rests O(n^2) since it is only possible for 2 new branches to appear after 'count-change' gets called.

;;;Exercise 1.15

;;;a. (sine 12.15)
;;;   (p (sine 4.05))
;;;   (p (p (sine 1.35)))
;;;   (p (p (p (sine 0.45))))
;;;   (p (p (p (p (sine 0.15)))))
;;;   (p (p (p (p (p (sine 0.05)))))
;;;   (p (p (p (p (p 0.05)))))

;;;   p is called 5 times.

;;;b. The growth of the number of steps is O(log(n)) since n becomes n/3 in each step.
;;;   The growth of the amount of space needed is O(log(n)) since it grows linearly with n.
