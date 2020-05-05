#lang racket
;print column of width w
(provide shunt)
(define (display-col w s)
  (let* ([n-spaces (- w (string-length s))]
         [spaces (make-string n-spaces #\space)])
    (display (string-append s spaces))))
;print columns given widths (idea borrowed from PicoLisp)
(define (tab ws . ss) (for-each display-col ws ss) (newline))
 
(define input "condition1 AND  ( ( condition2 OR condition3 ) AND condition4 )")
 
(define (paren? s) (or (string=? s "(") (string=? s ")")))
(define-values (prec lasso? rasso? op?)
  (let ([table '(["AND" 2 l]
                 ["OR" 2 l])])
    (define (asso x) (caddr (assoc x table)))
    (values (λ (x) (cadr (assoc x table)))
            (λ (x) (symbol=? (asso x) 'l))
            (λ (x) (symbol=? (asso x) 'r))
            (λ (x) (member x (map car table))))))
 
(define (shunt s)
  (let shunt ([out '()] [ops '()] [in (string-split s)])
    (match in
      ['() (if (memf paren? ops)
               (error "unmatched parens")
               (reverse (append (reverse ops) out)))]
      [(cons x in)
       (match x
         [(not(or "AND" "OR" "(" ")")) (shunt (cons x out) ops in)]
         ["(" (shunt out (cons "(" ops) in)]
         [")" (let-values ([(l r) (splitf-at ops (λ (y) (not (string=? y "("))))])
                (match r
                  ['() (error "unmatched parens")]
                  [(cons _ r) (shunt (append (reverse l) out) r in)]))]
         [else (let-values ([(l r) (splitf-at ops (λ (y) (and (op? y)
                                                              ((if (lasso? x) <= <) (prec x) (prec y)))))])
                 (shunt (append (reverse l) out) (cons x r) in))])])))
