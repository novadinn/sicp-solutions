#lang sicp

(define (f g)
  (g 2))

#|
(f f) will throw an error, since the call to g must be a procedure, but after the third call (2 2)
|#