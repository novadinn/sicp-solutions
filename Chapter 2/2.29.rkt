#lang sicp

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define left-branch
  (lambda (mob)
    (car mob)))
(define right-branch
  (lambda (mob)
    (cadr mob)))
(define branch-length
  (lambda (b)
    (car b)))
(define branch-structure
  (lambda (b)
    (cadr b)))

(define total-weight
  (lambda (m)
    (cond
      ((null? m) 0)
      ((number? m) m)
      (else (+ (total-weight (branch-structure (left-branch m)))
               (total-weight (branch-structure (right-branch m))))))))

(define balanced?
  (lambda (m)
    (define (torque branch) 
      (* (branch-length branch) (total-weight (branch-structure branch)))) 
    (cond
      ((not (pair? m)) #t)
      (else (and (= (torque (left-branch m)) (torque (right-branch m))) 
                 (balanced? (branch-structure (left-branch m))) 
                 (balanced? (branch-structure (right-branch m))))))))