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

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes s e)
  (cond
    ((> s e) #t)
    (else
     (timed-prime-test s)
     (search-for-primes (+ s 1) e))))

#|
(search-for-primes 1000 1100) => 1009 (4), 1013 (3), 1019 (5)
(search-for-primes 10000 10100) => 10007 (16), 10009 (16), 10037 (14)
(search-for-primes 100000 100100) => 100003 (24), 100019 (23), 100043 (25)
(search-for-primes 1000000 1000100) => 1000003 (67), 1000033 (69), 1000037 (71)
|#