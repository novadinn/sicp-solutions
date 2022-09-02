#lang sicp

(define last-pair
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) (car lst))
      (else (last-pair (cdr lst))))))
(define last-rember
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((null? (cdr lst)) '())
      (else (cons (car lst)
                  (last-rember (cdr lst)))))))

(define reverse
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons (last-pair lst)
                  (reverse (last-rember lst)))))))