#lang sicp

(define (double x)
  (* x 2))
(define (halve x)
  (/ x 2))

(define (fast-mul n m)
  (define (iter a b c)
    (cond
      ((= c 0) a)
      ((even? c)
       (iter a (double b) (halve c)))
      (else (iter (+ a b) b (- c 1)))))
  (iter 0 n m))