#lang sicp

(define make-point
  (lambda (x y)
    (cons x y)))
(define x-point
  (lambda (p)
    (car p)))
(define y-point
  (lambda (p)
    (cdr p)))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define make-segment
  (lambda (p1 p2)
    (cons p1 p2)))
(define start-segment
  (lambda (s)
    (car s)))
(define end-segment
  (lambda (s)
    (cdr s)))
(define midpoint-segment
  (lambda (s)
    (make-point
     (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
     (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2))))