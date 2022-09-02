#lang sicp

(define (pascal-row r c)
  (cond
    ((or (= r 1) (= c 1) (= c r)) 1)
    ((or (< r 1) (< c 1) (> c r)) 0)
    (else (+ (pascal-row (- r 1) (- c 1))
             (pascal-row (- r 1) c)))))