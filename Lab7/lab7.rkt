 #lang racket
(require srfi/13)
(require "helper_functions.rkt" "get-condition.rkt" "make-reader.rkt")

;SELECT row, col, CASE WHEN Quantity > 30 THEN "The quantity is greater than 30" WHEN Quantity = 30 THEN "The quantity is 30" ELSE "The quantity is under 30" END AS QuantityText FROM map_zal-skl9.csv
;--------------------------------------------------------case-----------------------------------------------------------------------
(define (case port command)
   (define read-row (make-reader port))
   (define head (read-row))
   (define before-case (string-split (substring command 0 (+ (string-contains command "CASE") 4)) " "))
   (define after-end (string-split (substring command (string-contains command "END")) " "))
   (define syntax (and (string-ci=? (first before-case) "SELECT")
                       (string-ci=? (list-ref before-case (- (length before-case) 1)) "CASE")
                       (or(check-cols (remove "CASE" (cdr before-case)) head)
                          (string-ci=? (second before-case) "*"))
                       (string-ci=? (first after-end) "END")
                       (string-ci=? (second after-end) "AS")
                       (string-ci=? (fourth after-end) "FROM")))
  (when (not syntax)
    (error 'помилка "невірно введено команду SELECT..CASE. Будь ласка, спробуйте ще"))
   (define head-add (third after-end))
   (define conditions (remove-duplicates (string-split (string-join (string-split (substring command
                                      (+ (string-contains command "CASE") 4) (string-contains command " END")) "\"") "") " WHEN ")))
  conditions)
;--------------------------------------------------------case-----------------------------------------------------------------------

;--------------------------------------------------------initialazer-----------------------------------------------------------------------
(define (cli)
  (writeln "Ласкаво просимо до lab 6 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "CASE"))
    (case (open-input-string (file->string (list-ref (string-split option " ")
                                             (- (length (string-split option " ")) 1)))) option)])
;    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "GROUP BY") (string-contains? option "HAVING"))
;     (display (having (open-input-string (file->string (list-ref (string-split option " ")
;                                             (- (length (string-split option " ")) 1))))
;                                                                 (string-split option)))]
;    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "GROUP BY"))
;     (display (group-by (open-input-string (file->string (list-ref (string-split option " ")
;                                           (- (length (string-split option " ")) 1))))
;                                           (string-split option)))]
;    [else (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")])
 )

(cli )


