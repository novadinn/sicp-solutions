#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (define iter
        (lambda (prev-table cur-table keys)
          (cond
            ((null? (cdr keys))
             (let ((record (assoc (car keys) (cdr cur-table))))
               (if record
                   (cdr record)
                   false)))
            (else
             (let ((subtable (assoc (car keys) (cdr prev-table))))
               (if subtable
                   (if (iter cur-table subtable (cdr keys))
                       (iter cur-table subtable (cdr keys))
                       false)
                   false))))))
      (iter local-table local-table keys))
    (define (insert! keys value)
      (define make-keys-list
      (lambda (keys value)
        (cond
          ((null? (cdr keys))
           (cons (car keys) value))
          (else (list (car keys)
                      (make-keys-list (cdr keys) value))))))
      (define iter
        (lambda (prev-table cur-table keys)
          (cond
            ((null? (cdr keys))
             (let ((record (assoc (car keys) (cdr cur-table))))
               (if record
                   (set-cdr! record value)
                   (set-cdr! cur-table
                             (cons (cons (car keys) value)
                                   (cdr cur-table))))))
            (else (let ((subtable (assoc (car keys) (cdr prev-table))))
                    (if subtable
                        (iter cur-table subtable (cdr keys))
                        (set-cdr! prev-table
                                  (cons (make-keys-list keys value)
                                        (cdr local-table)))))))))
      (iter local-table local-table keys))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Неизвестная операция -- TABLE" m))))
    dispatch))