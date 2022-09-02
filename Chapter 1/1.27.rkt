#lang sicp

(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-all n)
  (define (iter i)
    (cond
      ((= i n) #t)
      (else (and (= (expmod i n n) i)
                 (iter (+ i 1))))))
  (iter 2))

#|
(fermat-test-all 561) => #t
(fermat-test-all 1105) => #t
(fermat-test-all 1729) => #t
(fermat-test-all 2465) => #t
(fermat-test-all 2821) => #t
(fermat-test-all 6601) => #t
|#