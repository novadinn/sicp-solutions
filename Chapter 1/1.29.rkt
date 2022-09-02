#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (term m)
    (cond
      ((or (= m 0) (= m n)) (yk m))
      ((even? m) (* (yk m) 2))
      (else (* (yk m) 4))))
  (* (/ h 3) (sum term 0 inc n)))