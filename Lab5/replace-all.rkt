#lang racket
(provide replace-all)

(define (replace-all str from to)
  (string-replace str from to #:all? #t))