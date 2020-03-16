#lang racket
(require (planet neil/csv:1:=7) net/url)
 
(define make-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
 
(define (all-rows port)
  (define read-row (make-reader port))
  (define head (append (read-row) '("\t")))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define xs (map string->number row))
                 (append row (list " "))))
  (define (->string row) (string-join row "\t" #:after-last "\n"))
  (string-append* (map ->string (cons head rows))))

(define csv-file 
  (file->string "convertcsv.txt"))
 
(display (all-rows (open-input-string csv-file)))

(define (cli )(writeln "Ласкаво просимо до lab 3 cli!Будь ласка, введіть команду"))