#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define reverse-right
  (lambda (seq)
    (fold-right (lambda (x y)
                 (append y (list x)))
               '()
               seq)))
(define reverse-left
  (lambda (seq)
    (fold-left (lambda (x y)
                 (cons y x))
               '()
               seq)))