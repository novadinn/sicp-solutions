#lang sicp

(define square
  (lambda (x)
    (* x x)))

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Неизвестная оп. -- MAKE-FROM-MAG-ANG" op))))
  dispatch)