#lang racket
(provide ismember?)
(define (multiple-index lst strings)
  (if (empty? strings)
      (list )
      (append (list (index-of lst (car strings))) (multiple-index lst (cdr strings)))))

(define (multiple-list-ref lst numbers)
  (if (empty? numbers)
      (list )
      (append (list (list-ref lst (car numbers))) (multiple-list-ref lst (cdr numbers)))))

(define (aremembers? col-names lst)
  (if (empty? col-names)
     #t
     (and (ismember? (car col-names) lst) (aremembers? (cdr col-names) lst))))

(define (ismember? str strs) (if [member str strs] #t #f))