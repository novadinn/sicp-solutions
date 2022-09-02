#lang sicp

(define make-interval
  (lambda (a b)
    (cons a b)))
(define upper-bound
  (lambda (i)
    (car i)))
(define lower-bound
  (lambda (i)
    (cdr i)))

(define make-center-percent
  (lambda (center percent)
    (make-interval (- center (* center (/ percent 100)))
                   (+ center (* center (/ percent 100))))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define percent
  (lambda (i)
    (/ (* (- (upper-bound i) (center i))
          100)
       (center i))))