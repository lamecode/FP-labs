#lang racket
(require "helper_functions.rkt")
(provide check-single-condition)

(define (check-single-condition condition head row)
  (cond
    [(string-contains? condition "<>")
     (define part
       (string-split condition "<>"))
     (when (not (ismember? (first part) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (if (not (string-ci=? (second part) (string-replace (list-ref row (index-of head (first part))) " " "" #:all? #t)))
         #t #f)]
   [(string-contains? condition "<=")
     (define part
       (cond
         [(number? (string->number (second (string-split condition "<=")))) (string-split condition "<=")]
         [else (error 'помилка "невірно введено одну з умов. Будь ласка, спробуйте ще")]))          
     (when (not (ismember? (first part) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (if (<= (string->number (list-ref row (index-of head (first part)))) (string->number (second part)))
         #t #f)]
     [(string-contains? condition ">=")
     (define part
       (cond
         [(number? (string->number (second (string-split condition ">=")))) (string-split condition ">=")]
         [else (error 'помилка "невірно введено одну з умов. Будь ласка, спробуйте ще")]))          
    (when (not (ismember? (first part) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (if (>= (string->number (list-ref row (index-of head (first part)))) (string->number (second part)))
         #t #f)]
     [(string-contains? condition "=")
     (define part
       (string-split condition "="))          
     (when (not (ismember? (first part) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (if (string-ci=? (second part) (string-replace (list-ref row (index-of head (first part))) " " "" #:all? #t))
         #t #f)]
     [(string-contains? condition "<")
     (define part
       (cond
         [(number? (string->number (second (string-split condition "<")))) (string-split condition "<")]
         [else (error 'помилка "невірно введено одну з умов. Будь ласка, спробуйте ще")]))          
     (when (not (ismember? (first part) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (if (< (string->number (list-ref row (index-of head (first part)))) (string->number (second part)))
         #t #f)]   
   [(string-contains? condition ">")
     (define part
       (cond
         [(number? (string->number (second (string-split condition ">")))) (string-split condition ">")]
         [else (error 'помилка "невірно введено одну з умов. Будь ласка, спробуйте ще")]))          
     (when (not (ismember? (first part) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (if (> (string->number (list-ref row (index-of head (first part)))) (string->number (second part)) )
         #t #f)]
    )
  )
