#lang sicp

(define clear!
  (lambda (cell)
    (set! cell (list false))))
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-semaphore-a n)
  (let ((mutex (make-mutex))
        (cur-n 0))
    (define (the-semaphore m)
      (cond
        ((eq? m 'acquire)
         (mutex 'acquire)
         (if (< cur-n n)
             (begin
               (set! cur-n (+ 1 cur-n))
               (mutex 'release))
             (begin
               (mutex 'release)
               (the-semaphore 'acquire))))
        ((eq? m 'release) (begin
                            (set! cur-n (- cur-n 1))
                            (mutex 'release)))))
    the-semaphore))
(define (make-semaphore-b n)
  (define make-false-list
    (lambda (n)
      (cond
        ((= n 1) false)
        (else (list false
                    (make-false-list (- n 1)))))))
  (define find-free-slot
    (lambda (slots)
      (cond
        ((null? slots) false)
        (else
         (or (car slots)
             (find-free-slot (cdr slots)))))))
  (let ((slots (make-false-list n)))
    (define (the-semaphore m)
      (cond
        ((eq? m 'acquire)
         (if (not (find-free-slot slots))
             (the-semaphore 'acquire)))
        ((eq? m 'release)
         (set! slots (make-false-list n)))))
    the-semaphore))