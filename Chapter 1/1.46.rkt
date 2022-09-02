#lang sicp

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define iterative-improve
  (lambda (good-enough? improve)
    (lambda (x)
      (if (good-enough? x)
          x
          ((iterative-improve good-enough? improve) (improve x))))))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance)) 
  ((iterative-improve 
    (lambda (x) (close-enough? x (f x))) 
    f) 
   first-guess)) 
