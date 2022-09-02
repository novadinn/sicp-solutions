#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

#|
(define v (list 'a 'b 'c 'd))
(define w (mystery v))
The procedure mystery will produce the inverse of x, changing the value of x to (car x)
v => (a)
w => (d c b a)
|#