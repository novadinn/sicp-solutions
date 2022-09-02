#lang sicp

(define make-vect
  (lambda (x y)
    (cons x y)))
(define xcor-vect
  (lambda (v)
    (car v)))
(define ycor-vect
  (lambda (v)
    (cdr v)))

(define make-segment
  (lambda (v1 v2)
    (cons v1 v2)))
(define start-segment
  (lambda (s)
    (car s)))
(define end-segment
  (lambda (s)
    (cdr s)))