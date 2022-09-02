#lang sicp

(define random-init
  (lambda ()
    (random 2147483647)))
(define rand-update
  (let ((a (random-init))
        (b (random-init))
        (m (random-init)))
    (lambda (x)
      (remainder (+ (* a x) b) m))))
(define rand
  (let ((x (random-init)))
    (define dispatch
      (lambda (msg)
        (cond
          ((eq? msg 'generate)
           (begin
             (set! x (rand-update x))
             x))
          ((eq? msg 'reset)
           (lambda (new-val)
             (begin
               (set! x new-val)
               x)))
          (else (error "Unknown message" msg)))))
    dispatch))