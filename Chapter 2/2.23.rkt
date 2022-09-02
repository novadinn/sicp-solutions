#lang sicp

(define for-each
  (lambda (proc lst)
    (cond
      ((null? lst) #f)
      (else 
       (proc (car lst))
       (for-each proc (cdr lst))))))