#lang sicp

(define repeated
  (lambda (f n)
    (lambda (x)
      (if (= n 0)
          x
          (f ((repeated f (- n 1)) x))))))