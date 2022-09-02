#lang sicp

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Неправильный сигнал" s))))
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define logical-and
  (lambda (s1 s2)
    (cond
      ((and (= s1 1) (= s2 1)) 1)
      (else 0))))

(define or-gate
  (lambda (a1 a2 output)
    (let ((w1 (make-wire))
          (w2 (make-wire))
          (w3 (make-wire)))
      (inverter a1 w1)
      (inverter a2 w2)
      (and-gate w1 w2 w3)
      (inverter w3 output))
    'ok))