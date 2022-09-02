#lang sicp

(define divisions-count
  (lambda (n d)
    (cond
      ((= n 0) 0)
      ((= (remainder n d) 0)
       (+ 1 (divisions-count (/ n d) d)))
      (else 0))))

(define cons
  (lambda (a b)
    (* (expt 2 a)
       (expt 3 b))))
(define car
  (lambda (n)
    (divisions-count n 2)))
(define cdr
  (lambda (n)
    (divisions-count n 3)))