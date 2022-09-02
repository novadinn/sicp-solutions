#lang sicp

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
(define (square-list-mapped items)
  (map (lambda (x)
         (square x))
       items))