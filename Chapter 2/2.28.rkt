#lang sicp

(define fringe
  (lambda (tree)
    (cond
      ((null? tree) '())
      ((pair? (car tree))
       (append (fringe (car tree))
               (fringe (cdr tree))))
      (else (cons (car tree)
                  (fringe (cdr tree)))))))