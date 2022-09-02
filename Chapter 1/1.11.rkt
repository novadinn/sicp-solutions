#lang sicp

(define (f n)
  (cond
    ((< n 3) n)
    (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))
(define (f-iterative n)
  (define (f-iter a b c n)
    (cond
      ((< n 2) a)
      (else (f-iter (+ a b c) c a (- n 1)))))
  (f-iter 1 1 0 n))