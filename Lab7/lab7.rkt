 #lang racket
(require srfi/13)
(require "helper_functions.rkt" "get-condition.rkt" "make-reader.rkt")

;SELECT col, row, CASE WHEN row > 5 THEN "Рядок більший за 5" WHEN row = 5 THEN "Рядок рівний 5" ELSE "Рядок менший за 5" END AS Txt FROM map_zal-skl9.csv
;--------------------------------------------------------case-----------------------------------------------------------------------
(define (case port command)
   (define read-row (make-reader port))
   (define head (read-row))
   (define before-case (string-split (substring command 0 (+ (string-contains command "CASE") 4)) " "))
   (define after-end (string-split (substring command (string-contains command "END")) " "))
   (define syntax (and (string-ci=? (first before-case) "SELECT")
                       (string-ci=? (list-ref before-case (- (length before-case) 1)) "CASE")
                       (or(check-cols (remove "CASE" (cdr before-case)) head)
                          (and (string-ci=? (second before-case) "*") (= (length before-case) 3)))
                       (string-ci=? (first after-end) "END")
                       (string-ci=? (second after-end) "AS")
                       (string-ci=? (fourth after-end) "FROM")
                       (string-contains? command " ELSE ")
                       (string-contains? command " WHEN ")))
  (when (not syntax)
    (error 'помилка "невірно введено команду SELECT..CASE. Будь ласка, спробуйте ще"))
   (define head-add (third after-end))
   (define temp (remove-duplicates (string-split (string-join (string-split (substring command
                                      (+ (string-contains command "CASE") 4) (string-contains command " END")) "\"") "") " WHEN ")))
   (define col (if (= (length before-case) 3)
       (second before-case)
       (map (lambda (x) (substring x 0 (- (string-length x) 1))) (remove "CASE" (cdr before-case)))))
   (define conditions (append (get-n temp (- (length temp) 1)) (string-split (car (list-tail temp (- (length temp) 1))) " ELSE ")))
    (cond
    [(and (string? col)(string-ci=? col "*"))
      (define rows (for/list ([row (in-producer read-row '())])
                     (append row (list (get-line conditions head row)) (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append head (list head-add "\n") ) rows)))]
    [(list? col)
     (define rows (for/list ([row (in-producer read-row '())])
                 (define column (multiple-list-ref row (multiple-index head col)))
                 (append column (list (get-line conditions head row)) (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head col)) (list head-add "\n")) rows)))]
    [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]))
;--------------------------------------------------------case-----------------------------------------------------------------------

;--------------------------------------------------------initialazer-----------------------------------------------------------------------
(define (cli)
  (writeln "Ласкаво просимо до lab 6 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "CASE"))
    (display (case (open-input-string (file->string (list-ref (string-split option " ")
                                             (- (length (string-split option " ")) 1)))) option))])
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


