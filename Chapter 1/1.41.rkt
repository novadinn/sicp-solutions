#lang sicp

(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))

#|
(((double (double double)) inc) 5) => 21
|#