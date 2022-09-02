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

(define euler
  (lambda (k)
    (+ 2 (cont-frac
     (lambda (i) 1.0)
     (lambda (i)
       (cond
         ((= (remainder (+ i 2) 3) 0)
          (* 2 (/ (+ i 2) 3)))
         (else 1.0)))
     k))))