 #lang racket
(require srfi/13)
(require "helper_functions.rkt" "get-condition.rkt" "make-reader.rkt" "agregat-functions.rkt")

;SELECT COUNT(col),row FROM map_zal-skl9.csv GROUP BY row
;SELECT row,AVG(col) FROM map_zal-skl9.csv GROUP BY row
;SELECT MAX(col),row FROM map_zal-skl9.csv GROUP BY row
;SELECT title,COUNT(col) FROM map_zal-skl9.csv GROUP BY title HAVING COUNT(col)>1
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

;------------------------------------------------------------group_by--------------------------------------------------------------------------------
(define (group-by port command)
  (define read-row (make-reader port))
  (define head (read-row))
  (define syntax (and (string-ci=? (first command) "SELECT")
                      (string-ci=? (third command) "FROM")
                      (string-ci=? (fifth command) "GROUP")
                      (string-ci=? (sixth command) "BY")
                      (= (length (string-split (second command) ",")) 2)
                      (or (string-contains? (first (string-split (second command) ",")) "COUNT")
                          (string-contains? (first (string-split (second command) ",")) "AVG")
                          (string-contains? (first (string-split (second command) ",")) "MAX")
                          (string-contains? (second (string-split (second command) ",")) "COUNT")
                          (string-contains? (second (string-split (second command) ",")) "AVG")
                          (string-contains? (second (string-split (second command) ",")) "MAX"))
                      (or (ismember? (first (string-split (second command) ",")) head)
                          (ismember? (second (string-split (second command) ",")) head))))
  (when (not syntax)
    (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (define cols (string-split (second command) ","))
  (define all-rows (for/list ([row (in-producer read-row '())])
                      row))
  (define agregat-col (cond
    [(or (string-contains? (first cols) "COUNT")
         (string-contains? (first cols) "AVG")
         (string-contains? (first cols) "MAX"))
     (first cols)]
    [(or (string-contains? (second cols) "COUNT")
         (string-contains? (second cols) "AVG")
         (string-contains? (second cols) "MAX"))
     (second cols)]
    [else (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")]))
  (define values (cond
    [(string-ci=? agregat-col (first cols))
     (remove-duplicates (for/list ([row all-rows])
                 (list-ref row (index-of head (second cols)))))]
    [(string-ci=? agregat-col (second cols))
     (remove-duplicates (for/list ([row all-rows])
                 (list-ref row (index-of head (first cols)))))]
    [else (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")]))  
  (define result
    (cond
      [(string-ci=? agregat-col (first cols)) (for/list ([value values])
                 (append (list (do-agregat-function agregat-col (second cols) value head all-rows)) (list value) (list "\n")))]
      [(string-ci=? agregat-col (second cols)) (for/list ([value values])
                 (append (list value) (list (do-agregat-function agregat-col (first cols) value head all-rows)) (list "\n")))]))
  (define (->string row) (string-join row "\t"))
  (if (string-ci=? agregat-col (first cols))
      (string-append* (map ->string (cons (append (list agregat-col) (list (second cols)) (list "\n")) result)))
      (string-append* (map ->string (cons (append (list (first cols)) (list agregat-col) (list "\n")) result))))
  )

(define (having port command)
  (define read-row (make-reader port))
  (define head (read-row))
  (define syntax (and (string-ci=? (first command) "SELECT")
                      (string-ci=? (third command) "FROM")
                      (string-ci=? (fifth command) "GROUP")
                      (string-ci=? (sixth command) "BY")
                      (= (length (string-split (second command) ",")) 2)
                      (or (string-contains? (first (string-split (second command) ",")) "COUNT")
                          (string-contains? (first (string-split (second command) ",")) "AVG")
                          (string-contains? (first (string-split (second command) ",")) "MAX")
                          (string-contains? (second (string-split (second command) ",")) "COUNT")
                          (string-contains? (second (string-split (second command) ",")) "AVG")
                          (string-contains? (second (string-split (second command) ",")) "MAX"))
                      (or (ismember? (first (string-split (second command) ",")) head)
                          (ismember? (second (string-split (second command) ",")) head))
                      (string-ci=? (eighth command) "HAVING")))
  (when (not syntax)
    (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (define cols (string-split (second command) ","))
  (define all-rows (for/list ([row (in-producer read-row '())])
                      row))
  (define agregat-col (cond
    [(or (string-contains? (first cols) "COUNT")
         (string-contains? (first cols) "AVG")
         (string-contains? (first cols) "MAX"))
     (first cols)]
    [(or (string-contains? (second cols) "COUNT")
         (string-contains? (second cols) "AVG")
         (string-contains? (second cols) "MAX"))
     (second cols)]
    [else (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")]))
  (define values (cond
    [(string-ci=? agregat-col (first cols))
     (remove-duplicates (for/list ([row all-rows])
                 (list-ref row (index-of head (second cols)))))]
    [(string-ci=? agregat-col (second cols))
     (remove-duplicates (for/list ([row all-rows])
                 (list-ref row (index-of head (first cols)))))]
    [else (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")]))  
  (define temp
    (cond
      [(string-ci=? agregat-col (first cols)) (for/list ([value values])
                 (append (list (do-agregat-function agregat-col (second cols) value head all-rows)) (list value) (list "\n")))]
      [(string-ci=? agregat-col (second cols)) (for/list ([value values])
                 (append (list value) (list (do-agregat-function agregat-col (first cols) value head all-rows)) (list "\n")))]))
  (define new-head (if (string-ci=? agregat-col (first cols))
      (append (list agregat-col) (list (second cols)) (list "\n"))
      (append (list (first cols)) (list agregat-col) (list "\n"))))
  (define result (for/list ([row temp]
                            #:when (check-single-condition (ninth command) new-head row)) row))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons new-head result)))
  )
;------------------------------------------------------------group_by--------------------------------------------------------------------------------

(define (do-agregat-function agregat-col other-col value head all-rows)
  (define rows (get-rows-by-value value other-col head all-rows))
  (cond
    [(and (string-ci=? (substring agregat-col 0 6) "COUNT(")
          (string-ci=? (substring agregat-col (- (string-length agregat-col) 1)) ")"))
     (count1 rows)]
    [(and (string-ci=? (substring agregat-col 0 4) "AVG(")
          (string-ci=? (substring agregat-col (- (string-length agregat-col) 1)) ")"))
     (avg head (substring agregat-col (+ (string-contains agregat-col "AVG(") 4) (string-contains agregat-col ")")) rows)]
    [(and (string-ci=? (substring agregat-col 0 4) "MAX(")
          (string-ci=? (substring agregat-col (- (string-length agregat-col) 1)) ")"))
     (max_f head (substring agregat-col (+ (string-contains agregat-col "MAX(") 4) (string-contains agregat-col ")")) rows)]
    [else (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")])
  )

(define (get-rows-by-value value other-col head all-rows)
  (for/list ([row all-rows]
             #:when (string-ci=? value (list-ref row (index-of head other-col))))
    row)
  )

;--------------------------------------------------------initialazer-----------------------------------------------------------------------
(define (cli)
  (writeln "Ласкаво просимо до lab 6 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "CASE"))
    (display (case (open-input-string (file->string (list-ref (string-split option " ")
                                             (- (length (string-split option " ")) 1)))) option))]
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "GROUP BY") (string-contains? option "HAVING"))
     (display (having (open-input-string (file->string (fourth (string-split option " "))))
                                           (string-split option " ")))]
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "GROUP BY"))
     (display (group-by (open-input-string (file->string (fourth (string-split option " "))))
                                           (string-split option " ")))]
    [else (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")]))


(cli )


