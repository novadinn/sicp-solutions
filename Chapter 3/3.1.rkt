#lang sicp

(define make-accumulator
  (lambda (start-val)
    (lambda (add-val)
      (begin
        (set! start-val (+ start-val add-val))
        start-val))))