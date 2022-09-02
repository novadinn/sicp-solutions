#lang sicp

(define (square x) (* x x))

(define tan-cf
  (lambda (x k)
    (define iter
      (lambda (i)
        (cond
          ((> i k) 1)
          ((= i 0)
           (/ x (iter (+ i 1))))
          (else (- i (/ (square x)
                   (iter (+ i 2))))))))
    (iter 0)))