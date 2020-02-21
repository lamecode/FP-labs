#lang Racket
(define (factorial n [res 1])
  (if (= n 1)
      res
      (factorial (- n 1) (+ res (expt -2 (- n 1)))))
      )

(factorial (string->number (read-line (current-input-port))))
