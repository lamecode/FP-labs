#lang racket
(require "make-reader.rkt" "helper_functions.rkt")
(provide (all-defined-out))
(define (max_f head column rows)
  (define sum 0)
  (define j 0)
  (define result-rows (for/list ([row rows])
                 (define value (list-ref row (index-of head column)))
                 (cond
                   [(string-ci=? value "") 0]
                   [(not (number? (string->number value))) (error 'помилка "формат даних в колонці має бути числом")]
                   [else (string->number value)])))
  (define maximum (number->string (apply max result-rows)))
  maximum)

(define (avg head column rows)
  (define sum 0)
  (define j 0)
  (define result-rows (for/list ([row rows])
                 (define value (list-ref row (index-of head column)))
                 (cond
                   [(string-ci=? value "") 0]
                   [(not (number? (string->number value))) (error 'помилка "формат даних в колонці має бути числом")]
                   [else (string->number value)])))
  (define average (number->string (/ (apply + result-rows) (string->number (string-append (number->string (length rows)) ".0")))))
  average)

(define (count1 rows)
  (number->string (length rows)))