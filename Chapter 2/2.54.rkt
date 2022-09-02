#lang sicp

(define equal?
  (lambda (lst1 lst2)
    (cond
      ((and (null? lst1) (null? lst2)) #t)
      ((eq? (car lst1) (car lst2))
       (equal? (cdr lst1) (cdr lst2)))
      (else #f))))