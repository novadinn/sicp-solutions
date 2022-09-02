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

(define make-rect
  (lambda (p w h)
    (cons p (cons w h))))
(define rect-width
  (lambda (r)
    (cadr r)))
(define rect-height
  (lambda (r)
    (cddr r)))
#|
(define make-rect
  (lambda (p1 p2)
    (cons p1 p2)))
(define rect-width
  (lambda (r)
    (abs (- (x-point (car r))
            (x-point (cdr r))))))
(define rect-height
  (lambda (r)
    (abs (- (y-point (car r))
            (y-point (cdr r))))))
|#
(define rect-perimeter
  (lambda (r)
    (* 2 (+ (rect-width r)
            (rect-height r)))))
(define rect-square
  (lambda (r)
    (* (rect-width r)
       (rect-height r))))