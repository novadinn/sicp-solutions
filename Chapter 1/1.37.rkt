#lang sicp

(define cont-frac
  (lambda (n d k)
    (define iter
      (lambda (i)
        (cond
          ((> i k) 0)
          (else (/ (n i)
                   (+ (d i) (iter (+ i 1))))))))
    (iter 0)))
(define cont-frac-iter
  (lambda (n d k)
    (define iter
      (lambda (i result)
        (cond
          ((> i k) result)
          (else (iter (+ i 1)
                      (/ (n i)
                         (+ (d i) result)))))))
    (iter 0 0)))