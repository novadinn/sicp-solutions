#lang sicp

#|
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
The result can be 1000000, 100000, 10000, 1000 or 100.
|#

#|
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
The result is equal to 1000000.
|#