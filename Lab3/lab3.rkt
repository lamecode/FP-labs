#lang racket
(require (planet neil/csv:1:=7) net/url)
 
(define make-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
 
(define (all-rows port)
  (define read-row (make-reader port))
  (define head (append (read-row) '("")))
  (define rows (for/list ([row (in-producer read-row '())])
                (define xs (map string->number row))
                 (append row (list 1 2 3))))
  (define (->string row) (string-join row "\t" #:after-last "\n"))
  (string-append* (map ->string (cons head rows))))

(define csv-file 
  (file->string "convertcsv.csv"))
 
(display (all-rows (open-input-string csv-file)))