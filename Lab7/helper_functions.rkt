#lang racket
(require "make-reader.rkt")
(provide (all-defined-out))

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

(define (without-spaces lst)
  (if (eq? empty lst)
      (list )
      (if (not (or (string-contains? (car lst) "ELSE")
               (string-contains? (car lst) "(")
               (string-contains? (car lst) ")")
               (string-contains? (car lst) "=")
               (string-contains? (car lst) "<>")
               (string-contains? (car lst) "<=")
               (string-contains? (car lst) "<")
               (string-contains? (car lst) ">=")
               (string-contains? (car lst) ">")))
          (append (list (string-replace (car lst) " " "" #:all? #t)) (without-spaces (cdr lst)))
          (append (list (car lst)) (without-spaces (cdr lst))))))

(define (check-cols lst head)
  (if (empty? lst)
      #t (if (and (ismember? (substring (car lst) 0 (- (string-length (car lst)) 1)) head)
                 (string-ci=? (substring (car lst) (- (string-length (car lst)) 1)) ","))
             (and #t (check-cols (cdr lst) head))
             #f)))

(define (get-n lst index)
  (define (helper-function llst start)
      (if (= start index)
          (list)
          (append (list (car llst)) (helper-function (cdr llst) (+ start 1)))))
  (helper-function lst 0))

(define (union-check lst)
  (cond
    [(or (empty? lst) (= (length lst) 1)) (list )]
    [#t (define port1 (open-input-string (file->string (fourth (string-split (car lst) " ")))))
        (define port2 (open-input-string (file->string (fourth (string-split (car (cdr lst)) " ")))))
  (define read-row1 (make-reader port1))
  (define read-row2 (make-reader port2))
  (define head1 (read-row1))
  (define head2 (read-row2))
  (if (eq? head1 head2)
      (union-check (cdr lst))
      (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще"))
  ]))

(define (check-columns lst table1 table2 head1 head2)
   (cond
     [(eq? empty lst) #t] 
  [else (define tab (string-split (car lst) "."))
 (if (and (or (string-ci=? (first tab) table1) (string-ci=? (first tab) table2))
          (or (ismember? (second tab) head1) (ismember? (second tab) head2)))
                  (and #t (check-columns (cdr lst) table1 table2 head1 head2))
                  #f)]))

(define (multiple-tabs-index table1 table2 head1 head2 columns)
  (define (helper A columns)
    (cond
    [(eq? empty columns) A]
    [(string-ci=? (first (string-split (car columns) ".")) table1)
     (helper (append (list (list 1 (index-of head1 (second (string-split (car columns) "."))))) A) (cdr columns))]
    [(string-ci=? (first (string-split (car columns) ".")) table2)
     (helper (append (list (list 2 (index-of head2 (second (string-split (car columns) "."))))) A) (cdr columns))])
    )
    (helper '() columns))

(define (get-row rows str)
  (define (helper-function rows str)
    (cond
      [(empty? rows) (list )]
      [(ismember? str (car rows)) (car rows)]
      [else (helper-function (cdr rows) str)])
    )
  (helper-function rows str))

(define (fill-row length)
       (if (= length 0)
           (list )
           (append (list "null") (fill-row (- length 1)))))

(define (check-presence head1 head2 table1 table2 cols)
  (cond
    [(empty? cols) (list)]
    [(or (and (string-ci=? (first (string-split (first cols) ".")) table1)
          (ismember? (second (string-split (first cols) ".")) head1))
         (and (string-ci=? (first (string-split (first cols) ".")) table2)
          (ismember? (second (string-split (first cols) ".")) head2)))
     (append (list (second (string-split (first cols) "."))) (check-presence head1 head2 table1 table2 (cdr cols)))]
    [else (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще")]))

(define (multiple-tabs-list-ref cols1 cols2 numbers)
    (cond
      [(empty? numbers) (list )]
      [(= (first (first numbers)) 1) (append (list (list-ref cols1 (second (first numbers)))) (multiple-tabs-list-ref cols1 cols2 (cdr numbers)))]
      [(= (first (first numbers)) 2) (append (list (list-ref cols2 (second (first numbers)))) (multiple-tabs-list-ref cols1 cols2 (cdr numbers)))])
        )
