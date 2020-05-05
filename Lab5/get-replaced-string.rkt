#lang racket
(require srfi/13  "replace-all.rkt")
(define str "SELECT * FROM map_zal-skl9.csv WHERE col=28 AND ( title=\"Вільне місце\" OR title=\"Зуєв Максим Сергійович\" )")
(writeln (string-contains str "\""))
(replace-all str (substring str (string-contains str "\"")) (string-contains (substring str (string-contains str "\"")) "\"") #:all? #t)