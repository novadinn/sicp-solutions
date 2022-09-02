#lang sicp

(define (a-plus-abs-b a b)
  ((if (> b 0) ; if b is greater than 0
       + ; + a b
       -) ; - a b
   a b))