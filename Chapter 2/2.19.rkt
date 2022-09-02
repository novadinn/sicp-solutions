#lang sicp

(define no-more?
  (lambda (lst)
    (null? lst)))

(define except-first-denomination
  (lambda (coins)
    (cdr coins)))
(define first-denomination
  (lambda (coins)
    (car coins)))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))