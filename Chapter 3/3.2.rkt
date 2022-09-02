#lang sicp

(define make-monitored
  (lambda (fn)
    (let ((count 0))
      (define dispatch
        (lambda (msg)
          (cond
            ((equal? msg 'how-many-calls?) count)
            ((number? msg)
             (begin
               (set! count (+ count 1))
               (fn msg)))
            (else (error "Unknown message" msg)))))
      dispatch)))