#lang sicp

(define (good-enough? guess prev x)
  (< (abs (- guess prev)) 0.001))

#|
This works a lot better on smaller input.
|#