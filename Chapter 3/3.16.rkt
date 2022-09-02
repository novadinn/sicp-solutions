#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

#|
(count-pairs '(1 2 3)) => 3
(count-pairs '(1 2 (3))) => 4
(count-pairs '(1 2 ((((3)))))) => 7
(count-pairs (make-cycle '(1 2 3))) => will never end
|#