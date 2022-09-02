#lang sicp

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond
      ((and (< n 0) (< d 0))
       (cons (/ (- n) g) (/ (- d) g)))
      ((< d 0)
       (cons (/ (- n) g) (/ (- d) g)))
      (else
       (cons (/ n g) (/ d g))))))