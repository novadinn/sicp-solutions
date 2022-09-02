#lang sicp

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond
    ((> a b) null-value)
    ((filter a)
     (combiner (term a)
               (filtered-accumulate filter combiner null-value term (next a) next b)))
    (else (filtered-accumulate filter combiner null-value term (next a) next b))))

(define (prime-sum a b)
  (filtered-accumulate prime? + 0 square a inc b))
(define (product-of-relative-primes n)
  (define (filter k)
    (= (gcd k n) 1))
  (filtered-accumulate filter * 1 identity 1 inc n))