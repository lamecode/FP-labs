#lang Racket
;1. Описати неіменовану функцію для об'єднання голів трьох
;списків в один список, вихідні дані взяти з таблиці 1.
(writeln (list(car '("Z" "X" "C" "S" "A" "D" "F"))(car '(("R") (30) (3) 23))(car '("U" "I" 8 9 6 5 4 3 (1 2 3)))))
;2. Описати іменовану функцію для створення нового списку з
;елементів декількох вихідних списків. В якості вихідних списків
;використовувати списки таблиці 1. Номери елементів списків взяти в
;таблиці 2.
(define task2 (writeln (list (fifth '("Z" "X" "C" "S" "A" "D" "F")) (fourth '(("R") (30) (3) 23)) (seventh '("U" "I" 8 9 6 5 4 3 (1 2 3))))))
;Завдання 3. Описати іменовану функцію для обчислення результату
;відповідно до варіанта індивідуального завдання зі списку. Набори для
;множин взяти в таблиці 1.
(define A(list "Z" "X" "C" "S" "A" "D" "F"))
(define B(list '("R") '(30) '(3) 23))
(define Universum(append  A B '("U" "I" 8 9 6 5 4 3 (1 2 3))) )
(define nA(remove* A Universum))
(define nB(remove* B Universum))
(define brace11(remove* '("U" "I" 8 9 6 5 4 3 (1 2 3)) (append nA nB)))
(define brace12(append brace11 '("U" "I" 8 9 6 5 4 3 (1 2 3))))
(define brace1(remove* brace12 Universum))
(define brace21(append B (remove* B (append nA B))))
(define brace2(remove* brace21 Universum))
(define brace31(remove* '("U" "I" 8 9 6 5 4 3 (1 2 3)) (append nA '("U" "I" 8 9 6 5 4 3 (1 2 3)))))
(define brace3(append '("U" "I" 8 9 6 5 4 3 (1 2 3)) brace31 ))
(define final-brace(append brace3 (append brace1 brace2)))
(writeln final-brace)