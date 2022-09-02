#lang sicp

(define make-interval
  (lambda (a b)
    (cons a b)))
(define upper-bound
  (lambda (i)
    (car i)))
(define lower-bound
  (lambda (i)
    (cdr i)))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))