#lang racket
(require (planet neil/csv:1:=7))
(provide make-reader)
(provide all-rows)

(define make-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (all-rows port)
  (define read-row (make-reader port))
  (define head (append (read-row) (list "\n")))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define xs (map string->number row))
                 (append row (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows))))

