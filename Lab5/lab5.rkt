#lang racket
(require (planet neil/csv:1:=7))
(require srfi/13)
 
(define make-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
 
(define (all-rows port)
  (define read-row (make-reader port))
  (define head (append (read-row) (list "\n")))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define xs (map string->number row))
                 (append row (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows))))

(define (load str)
  (define scope1 (string-length str))
  (define scope2 (string-length str))
  (when (> scope1 6)(set! scope1 (substring str 4 6))
    (set! scope2 (substring str (- (string-length str) 2))))
  (define command-syntax (and (string-ci=? scope1 "(\"") (string-ci=? scope2 "\")")))
  (define filename (substring str 6 (- (string-length str) 2)))
  (define file-format "")
  (when (> (string-length filename) 4)
    (set! file-format (substring filename (- (string-length filename) 4))))
   (if(equal? command-syntax #t)
      (if(or (string-ci=? file-format ".csv") (string-ci=? file-format ".tsv") (string-ci=? file-format ".txt"))
            (display (all-rows (open-input-string (file->string filename))))
         (writeln "Невірно введено назву або формат файлу. Будь ласка, спробуйте ще.")
         )
      (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")
 ))

(define (simple-select port command)
  (when (not (string-ci=? (third command) "FROM"))
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (define file-format (substring (fourth command) (- (string-length (fourth command)) 4)))
  (when (not (or (string-ci=? file-format ".csv") (string-ci=? file-format ".tsv") (string-ci=? file-format ".txt")))
      (error 'помилка "невірно введено формат файлу. Будь ласка, спробуйте ще"))
  (define read-row (make-reader port))
  (define head (read-row))
  (cond
    [(string-ci=? (second command) "*") (all-rows (open-input-string (file->string (fourth command))))]
    [(not (string-ci=? (second command) "*"))
     (define column-name (string-split (second command) ","))
  (define contains-column (aremembers? column-name head))
  (when (eq? contains-column #f)
    (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще")) 
  (define rows (for/list ([row (in-producer read-row '())])
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                 (append column (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) rows)))]))

(define (distinct-select port command)
  (when (not (string-ci=? (fourth command) "FROM"))
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (when (not (string-ci=? (second command) "DISTINCT"))
    (error 'помилка "невірно введено команду SELECT DISTINCT. Будь ласка, спробуйте ще"))
  (define file-format (substring (fifth command) (- (string-length (fifth command)) 4)))
  (when (not (or (string-ci=? file-format ".csv") (string-ci=? file-format ".tsv")))
      (error 'помилка "невірно введено формат файлу. Будь ласка, спробуйте ще"))
  (define read-row (make-reader port))
  (define head (read-row))
  (cond
    [(string-ci=? (third command) "*")
     (define rows (remove-duplicates (for/list ([row (in-producer read-row '())])
                 (define xs (map string->number row))
                 (append row (list "\n")))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows)))]
    [(not (string-ci=? (third command) "*")) (define column-name (string-split (third command) ","))
  (define contains-column (aremembers? column-name head))
  (when (eq? contains-column #f)
    (error 'помилка "невірно введено назви колонок. Будь ласка, спробуйте ще"))
  (define checker (list "random row"))
  (define rows (remove-duplicates (for/list ([row (in-producer read-row '())])
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                  (append column (list "\n")))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) rows)))]))


(define (select-where port command)
  (define before-part (string-split(substring command 0 (+ (string-contains command " WHERE") 6))))
  (define part (substring command (+ (string-contains command "WHERE ") 6)))
   (when (not (string-ci=? (third before-part) "FROM"))
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (define file-format (substring (fourth before-part) (- (string-length (fourth before-part)) 4)))
  (when (not (or (string-ci=? file-format ".csv") (string-ci=? file-format ".tsv") (string-ci=? file-format ".txt")))
      (error 'помилка "невірно введено формат файлу. Будь ласка, спробуйте ще"))
  (when (not (string-ci=? (fifth before-part) "WHERE"))
    (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще"))
  (define read-row (make-reader port))
  (define head (append (read-row) (list "\n")))
  (define condition
    (cond
      [(string-contains part "=")(string-split part "=")]
      [(string-contains part "<")(string-split part "<")]
      [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]))
  (when (and (< (length condition) 2) (not (ismember? (first condition) head)))
    (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще"))
  (cond
    [(number? (string->number (second condition)))
     (if (string-contains part "=")
         (equal (second before-part) read-row head condition)
         (number-inequal (second before-part) read-row head condition))]
    [(and (not (number? (string->number (second condition))))
          (string-ci=? (substring (second condition) 0 1) "\"") (string-ci=? (substring (second condition) (- (string-length (second condition)) 1)) "\""))
      (if (string-contains part "=")
         (equal (second before-part) read-row head (list (first condition) (substring (second condition) 1 (- (string-length (second condition)) 1))))
         (string-inequal (second before-part) read-row head (list (first condition) (substring (second condition) 1 (- (string-length (second condition)) 1)))))]
    [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]
     ))

(define (equal col read-row head condition)
  (cond
    [(string-ci=? col "*")
      (define rows (for/list ([row (in-producer read-row '())]
                             #:when (string-ci=? (list-ref row (index-of head (first condition))) (second condition)))
                     (append row (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows)))]
    [(not (string-ci=? col "*"))
     (define column-name (string-split col ","))
     (define rows (for/list ([row (in-producer read-row '())]
                             #:when (string-ci=? (list-ref row (index-of head (first condition))) (second condition)))
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                 (append column (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) rows)))]
    [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]))

(define (number-inequal col read-row head condition)
  (cond
    [(string-ci=? col "*")
      (define rows (for/list ([row (in-producer read-row '())]
                             #:when (< (string->number (list-ref row (index-of head (first condition)))) (string->number (second condition))))
                     (append row (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows)))]
    [(not (string-ci=? col "*"))
     (define column-name (string-split col ","))
     (define rows (for/list ([row (in-producer read-row '())]
                             #:when (< (string->number (list-ref row (index-of head (first condition)))) (string->number (second condition))))
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                 (append column (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) rows)))]
    [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]))

(define (string-inequal col read-row head condition)
  (cond
    [(string-ci=? col "*")
      (define rows (for/list ([row (in-producer read-row '())]
                             #:when (string-ci< (list-ref row (index-of head (first condition))) (second condition)))
                     (append row (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons head rows)))]
    [(not (string-ci=? col "*"))
     (define column-name (string-split col ","))
     (define rows (for/list ([row (in-producer read-row '())]
                             #:when (string-ci< (list-ref row (index-of head (first condition))) (second condition)))
                 (define column (multiple-list-ref row (multiple-index head column-name)))
                 (append column (list "\n"))))
  (define (->string row) (string-join row "\t"))
  (string-append* (map ->string (cons (append (multiple-list-ref head (multiple-index head column-name)) (list "\n")) rows)))]
    [else (error 'помилка "невірно введено умову WHERE. Будь ласка, спробуйте ще")]))

(define (multiple-index lst strings)
  (if (empty? strings)
      (list )
      (append (list (index-of lst (car strings))) (multiple-index lst (cdr strings)))))

(define (multiple-list-ref lst numbers)
  (if (empty? numbers)
      (list )
      (append (list (list-ref lst (car numbers))) (multiple-list-ref lst (cdr numbers)))))

(define (aremembers? col-names lst)
  (if (empty? col-names)
     #t
     (and (ismember? (car col-names) lst) (aremembers? (cdr col-names) lst))))

(define (ismember? str strs) (if [member str strs] #t #f))

(define (select str)
  (when (not (string-contains? str " "))
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (define command (string-split str))
  (when (< (length command) 4)
    (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще"))
  (cond
    [(= (length command) 4) (display (simple-select (open-input-string (file->string (fourth command))) command))]
    [(= (length command) 5) (display (distinct-select (open-input-string (file->string (fifth command))) command))]
    [(>= (length command) 6) (display (select-where (open-input-string (file->string (fourth command))) str))]
    [else (error 'помилка "невірно введено команду SELECT. Будь ласка, спробуйте ще")]
    )
  )

(define (cli)
  (writeln "Ласкаво просимо до lab 4 cli!Будь ласка, введіть команду")
  (define option (read-line (current-input-port))) 
(when (<= (string-length option) 6) (error 'помилка "невірно введено команду. Будь ласка, спробуйте ще"))
  (cond
    [(string-ci=? (substring option 0 4) "load") (load option)]
    [(string-ci=? (substring option 0 6) "SELECT") (select option)]
    [else (writeln "Невірно введено команду. Будь ласка, спробуйте ще.")])
 )
(cli )