#lang sicp

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess) 
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define repeated
  (lambda (f n)
    (lambda (x)
      (if (= n 0)
          x
          (f ((repeated f (- n 1)) x))))))

(define root
  (lambda (x n)
    (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) n)
                 1.0)))