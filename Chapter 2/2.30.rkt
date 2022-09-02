#lang sicp

(define (square x) (* x x))

(define square-tree
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((not (pair? tree) (square tree)))
      (else (cons (square-tree (car tree))
                  (square-tree (cdr tree)))))))

(define square-tree-mapped
  (lambda (tree)
    (map (lambda (t)
           (if (pair? t)
               (square-tree-mapped t)
               (square t)))
         tree)))

