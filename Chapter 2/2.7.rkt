#lang sicp

(define upper-bound
  (lambda (i)
    (car i)))
(define lower-bound
  (lambda (i)
    (cdr i)))