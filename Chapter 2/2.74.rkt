#lang sicp

(define (get-record division employee-name) 
  ((get division 'record) employee-name)) 

(define (get-salary division record) 
  ((get division 'salary) record)) 

(define (find-employee-record employee-name division-list) 
  (if (null? division-list) 
      #f 
      (or (get-record (car division-list) employee-name) 
          (find-employee-record employee-name (cdr division-list))))) 