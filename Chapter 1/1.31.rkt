#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (term k)
    (/ (* k (+ k 2))
       (* (+ k 1) (+ k 1))))
  (define (next k)
    (+ k 2))
  (* 4 (product term 2 next n)))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))