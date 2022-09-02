#lang sicp

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

#|
With normal-order, (test 0 (p)) will cause an endless loop. With applicative order, (text 0 (p)) will return
0, since (p) is never called.
|#