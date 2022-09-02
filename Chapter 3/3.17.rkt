#lang sicp

(define count-pairs
  (lambda (x)
    (define iter
      (lambda (x accumulator)
        (cond
          ((not (pair? x)) 0)
          ((memq x accumulator) 0)
          (else (+ (iter (car x) (cons x accumulator))
                   (iter (cdr x) (cons x accumulator))
                   1)))))
    (iter x '())))