 #lang racket
(require srfi/13)
(require "helper_functions.rkt" "make-reader.rkt" "sort.rkt" "shunting-yard.rkt" "get-condition.rkt")

;SELECT * FROM map_zal-skl9.csv WHERE col=28 AND ( title="Вільне місце" OR title="Гривко Сергій Дмитрович" )
;SELECT row FROM map_zal-skl9.csv ORDER BY title
;SELECT Max(id_fr) FROM map_zal-skl9.csv
;SELECT Avg(col) FROM map_zal-skl9.csv
;------------------------------------------------------------------SELECT_WHERE_command-----------------------------------------------------------------------
(define (select-where port command)
  (define before-part (string-split(substring command 0 (+ (string-contains command " WHERE") 6))))
   (when (not (string-ci=? (third before-part) "FROM"))
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (when (not (string-ci=? (fifth before-part) "WHERE"))
    (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще"))
  (define temp (substring command (+ (string-contains command "WHERE ") 6)))
  (define part
    (if (string-contains? temp "\"")
;        (remove* (list "") (string-split (string-join (without-spaces (string-split temp "\"")) "") " ") )
        (string-join (without-spaces (string-split temp "\"")) "")
        temp))
  (define read-row (make-reader port))
  (define head (append (read-row) (list "\n")))
  (operation (second before-part) read-row head (shunt part))
)
;------------------------------------------------------------------SELECT_WHERE_command-----------------------------------------------------------------------


;------------------------------------------------------------------ORDER_BY_command---------------------------------------------------------------------------
(define (orderby-select port option)
  (define command (string-split option))
  (define syntax (and (string-ci=? (first command) "SELECT") (string-ci=? (third command) "FROM") (string-ci=? (fifth command) "ORDER")
                      (string-ci=? (sixth command) "BY") (> (length command) 6)))
  (when (not syntax)
    (error 'помилка "невірно введено команду SELECT ... ORDER BY. Будь ласка, спробуйте ще"))
  (define read-row (make-reader port))
  (define head (read-row))
  (cond
    [(string-ci=? (second command) "*")
     (when (not (ismember? (seventh command) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (define sort-by (seventh command))
     (define rows (for/list ([row (in-producer read-row '())])
                 (append row (list "\n"))))
     (define col-data (for/list ([row rows])
                 (define column (multiple-list-ref row (multiple-index head (list sort-by))))
                  (string-join column)))
  (define (->string row) (string-join row "\t"))
  (cond
    [(or (eq? (length command) 7) (string-ci=? (eighth command) "ASC"))
     (string-append* (map ->string (cons (append head (list "\n")) (sort rows col-data))))]
    [(string-ci=? (eighth command) "DESC")
     (string-append* (map ->string (cons (append head (list "\n")) (reverse (sort rows col-data)))))]
    [else (error 'помилка "невірно введено порядок. Будь ласка, спробуйте ще")]
      )]
    [(not (string-ci=? (second command) "*")) (define column-name (string-split (second command) ","))
  (define contains-column (aremembers? column-name head))
  (when (not contains-column)
    (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще"))
  (when (not (ismember? (seventh command) head))
       (error 'помилка "невірно введено назву колонки. Будь ласка, спробуйте ще"))
     (define sort-by (seventh command))
     (define rows (for/list ([row (in-producer read-row '())])
                 (append row (list "\n"))))
     (define col-data (for/list ([row rows])
                 (define column (multiple-list-ref row (multiple-index head (list sort-by))))
                  (string-join column)))
  (define cols
    (cond
       [(or (eq? (length command) 7) (string-ci=? (eighth command) "ASC")) (sort (for/list ([row rows])
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                  (append column (list "\n"))) col-data)]
        [(string-ci=? (eighth command) "DESC") (reverse (sort (for/list ([row rows])
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                  (append column (list "\n"))) col-data))]
         [else (error 'помилка "невірно введено порядок. Будь ласка, спробуйте ще")]))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) cols)))])
  )
;------------------------------------------------------------------ORDER_BY_command---------------------------------------------------------------------------


;------------------------------------------------------------checking_condition_helper_functions-----------------------------------------------------
(define (operation col read-row head condition)
  (cond
    [(string-ci=? col "*")
      (define rows (for/list ([row (in-producer read-row '())]
                             #:when (car (calculate-RPN condition head row)))
                     (append row (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows)))]
    [(not (string-ci=? col "*"))
     (define column-name (string-split col ","))
     (define rows (for/list ([row (in-producer read-row '())]
                             #:when (car (calculate-RPN condition head row)))
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                 (append column (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) rows)))]
    [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]))

;------------------------------------------------------------checking_condition_helper_functions-----------------------------------------------------





;-------------------------------------------------------identify_functions-------------------------------------------------------------------
(define (select str)
  (when (not (string-contains? str " "))
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (define command (string-split str))
  (when (< (length command) 4)
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (cond
    [(>= (length command) 6) (display (select-where (open-input-string (file->string (fourth command))) str))]
    [else (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще")]
    )
  )
;-------------------------------------------------------identify_functions-------------------------------------------------------------------




;------------------------------------------------------------------arithmetic_functions-----------------------------------------------------------------
(define (ar-func option)
  (define command (string-split option))
  (define syntax (and (= (length command) 4) (string-ci=? (first command) "SELECT") (string-ci=? (third command) "FROM")))
  (when (not syntax)
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
    (cond
      [(<= (string-length (second command)) 5) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще")]
      [(and (string-ci=? (substring (second command) 0 4) "Avg(")
            (string-ci=? (substring (second command) (- (string-length (second command)) 1)) ")")) (avg command)]
      [(and (string-ci=? (substring (second command) 0 4) "Max(")
            (string-ci=? (substring (second command) (- (string-length (second command)) 1)) ")")) (max_f command)]
      [else (error 'помилка "невірно введено арифметичну функцію. Будь ласка, спробуйте ще")]))
;------------------------------------------------------------------arithmetic_functions-----------------------------------------------------------------



;------------------------------------------------------------------AVG----------------------------------------------------------------------------------
(define (avg option)
  (define column (substring (second option) 4 (- (string-length (second option)) 1)))
  (define file-name (fourth option))
  (define port (open-input-string(file->string file-name)))
  (define read-row (make-reader port))
  (define head (append (read-row) (list "\n")))
  (define contains-column (ismember? column head))
  (when (not contains-column)
    (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще"))
  (define sum 0)
  (define j 0)
  (define new-head (list (second option) "\n"))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define value (list-ref row (index-of head column)))
                 (cond
                   [(string-ci=? value "") 0]
                   [(not (number? (string->number value))) (error 'помилка "формат даних в колонці має бути числом")]
                   [else (string->number value)])))
  (define average (number->string (/ (apply + rows) (string->number (string-append (number->string (length rows)) ".0")))))
  (string-join (append new-head (list average)))
  )
;------------------------------------------------------------------AVG----------------------------------------------------------------------------------

;------------------------------------------------------------------MAX----------------------------------------------------------------------------------
(define (max_f option)
(define column (substring (second option) 4 (- (string-length (second option)) 1)))
  (define file-name (fourth option))
  (define port (open-input-string(file->string file-name)))
  (define read-row (make-reader port))
  (define head (append (read-row) (list "\n")))
  (define contains-column (ismember? column head))
  (when (not contains-column)
    (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще"))
  (define sum 0)
  (define j 0)
  (define new-head (list (second option) "\n"))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define value (list-ref row (index-of head column)))
                 (cond
                   [(string-ci=? value "") 0]
                   [(not (number? (string->number value))) (error 'помилка "формат даних в колонці має бути числом")]
                   [else (string->number value)])))
  (define maximum (number->string (apply max rows)))
  (string-join (append new-head (list maximum)))
  )
;------------------------------------------------------------------MAX----------------------------------------------------------------------------------


;--------------------------------------------------------initialazer-----------------------------------------------------------------------
(define (cli)
  (writeln "Ласкаво просимо до lab 5 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "WHERE")) (display (select-where (open-input-string
                                                                                                         (file->string (fourth (string-split option)))) option))]
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "ORDER BY")) (display (orderby-select (open-input-string
                                                                                                         (file->string (fourth (string-split option)))) option))]
    [(and (string-ci=? (substring option 0 6) "SELECT") (or (string-contains? option "Avg") (string-contains? option "Max"))) (display (ar-func option))]
    [else (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")])
 )

(cli )


