#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map ; rest is the same as s, but with no first element
                      (lambda (x)
                        (cons (car s) x)) ; so we can cons the first element to every variation of rest
                      rest)))))