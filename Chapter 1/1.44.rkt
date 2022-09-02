#lang sicp

(define repeated
  (lambda (f n)
    (lambda (x)
      (if (= n 0)
          x
          (f ((repeated f (- n 1)) x))))))

(define dx 0.00001)
(define smooth
  (lambda (f)
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(((repeated smooth 5) (lambda (x) (* x x))) 5)