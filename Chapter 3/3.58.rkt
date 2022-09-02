#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

#|
(expand 1 7 10) => 1 4 2 8 5...
(expand 3 8 10) => 3 7 5 0 0...
Expand = (/ num den), where radix is the base.
|#