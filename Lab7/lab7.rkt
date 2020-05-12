 #lang racket
(require srfi/13)
(require "helper_functions.rkt" "get-condition.rkt")

;--------------------------------------------------------case-----------------------------------------------------------------------
(define (case port command)
   (define before-case (string-split (substring command 0 (string-contains command "CASE")) " "))
   (define after-end (string-split (substring command (string-contains command "END")) " "))
   (define conditions (remove-duplicates (string-split (string-join (without-spaces (string-split (substring command
                                      (+ (string-contains command "CASE") 4) (string-contains command " END")) "\"")) "") " WHEN ")))
  (writeln before-case)
  (writeln after-end)
  conditions
  )
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


