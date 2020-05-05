 #lang racket
(require srfi/13)
(require "make-reader.rkt" "helper_functions.rkt")

;SELECT GJ FROM map_zal-skl9.csv INNER JOIN map_zal-skl9_2.csv ON EWGW = EGEWWG
;------------------------------------------------------------------INNER_JOIN_command-----------------------------------------------------------------------
(define (inner-join port1 port2 command)
  (define syntax (and (string-ci=? (first command) "SELECT")
                      (string-ci=? (third command) "FROM")
                      (string-ci=? (fifth command) "INNER")
                      (string-ci=? (sixth command) "JOIN")
                      (string-ci=? (eighth command) "ON")
                      (string-ci=? (tenth command) "=")))
  (when (not syntax)
    (error 'помилка "невірно введено команду INNER JOIN. Будь ласка, спробуйте ще"))
  (define read-row1 (make-reader port1))
  (define read-row2 (make-reader port2))
  (define head1 (append (read-row1) (list "\n")))
  (define head2 (append (read-row2) (list "\n")))
  (writeln head1)
  (writeln head2)
;  (define temp (substring command (+ (string-contains command "WHERE ") 6)))
 ; (define part
  ;  (if (string-contains? temp "\"")
;        (remove* (list "") (string-split (string-join (without-spaces (string-split temp "\"")) "") " ") )
   ;     (string-join (without-spaces (string-split temp "\"")) "")
    ;    temp))
  ;(define read-row (make-reader port))
  ;(define head (append (read-row) (list "\n")))
  ;(operation (second before-part) read-row head (shunt part))
)
;------------------------------------------------------------------INNER_JOIN_command-----------------------------------------------------------------------



;--------------------------------------------------------initialazer-----------------------------------------------------------------------
(define (cli)
  (writeln "Ласкаво просимо до lab 4 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "INNER JOIN")) (display (inner-join (open-input-string
                                                                                                         (file->string (fourth (string-split option))))
                                                                                                         (open-input-string
                                                                                                         (file->string (seventh (string-split option))))
                                                                                                         (string-split option)))]
;    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "FULL OUTER JOIN")) (display (full-outer-join (open-input-string
;                                                                                                         (file->string (fourth (string-split option)))) option))]
;    [(and (string-ci=? (substring option 0 6) "SELECT") (string-contains? option "RIGHT JOIN")) (display (right-join (open-input-string
;                                                                                                         (file->string (fourth (string-split option)))) option))]
;    [else (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")])
 ))

(cli )


