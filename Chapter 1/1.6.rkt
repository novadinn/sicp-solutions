#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

#|
When trying to evaluate the value of such an expression, both caluses of new-if will be called, i.e. the
program will never stop.
|#