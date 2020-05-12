 #lang racket
(require srfi/13)
(require "get-condition.rkt")

;--------------------------------------------------------case-----------------------------------------------------------------------
(define (having port command))
;--------------------------------------------------------case-----------------------------------------------------------------------

;--------------------------------------------------------initialazer-----------------------------------------------------------------------
(define (cli)
  (writeln "Ласкаво просимо до lab 6 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "CASE"))
    (case (open-input-string (file->string (list-ref (string-split option " ")
                                             (- (length (string-split option " ")) 1)))) option)]
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "GROUP BY") (string-contains? option "HAVING"))
     (display (having (open-input-string (file->string (list-ref (string-split option " ")
                                             (- (length (string-split option " ")) 1))))
                                                                 (string-split option)))]
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "GROUP BY"))
     (display (group-by (open-input-string (file->string (list-ref (string-split option " ")
                                           (- (length (string-split option " ")) 1))))
                                           (string-split option)))]
    [else (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")])
 )

(cli )


