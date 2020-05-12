#lang racket
(provide ismember?)
(define (ismember? str strs) (if [member str strs] #t #f))