#lang sicp

(define same-parity
  (lambda (x . rest)
    (define iter
      (lambda (pred rest)
        (cond
          ((null? rest) '())
          ((pred (car rest))
           (cons (car rest)
                 (iter pred (cdr rest))))
          (else (iter pred (cdr rest))))))
    (cons x (if (even? x)
                (iter even? rest)
                (iter odd? rest)))))