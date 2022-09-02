#lang sicp

(define last-pair
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (car lst))
      (else (last-pair (cdr lst))))))