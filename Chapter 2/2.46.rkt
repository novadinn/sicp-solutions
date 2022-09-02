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
(define add-vect
  (lambda (v1 v2)
    (make-vect
     (+ (xcor-vect v1) (xcor-vect v2))
     (+ (ycor-vect v1) (ycor-vect v2)))))
(define sub-vect
  (lambda (v1 v2)
    (make-vect
     (- (xcor-vect v1) (xcor-vect v2))
     (- (ycor-vect v1) (ycor-vect v2)))))
(define scale-vect
  (lambda (v s)
    (make-vect
     (* (xcor-vect v) s)
     (* (ycor-vect v) s))))