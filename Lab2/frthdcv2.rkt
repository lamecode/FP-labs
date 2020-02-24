#lang Racket
 ;(define i (-(/ (list-length A) 2) 1))
(define (swap A k j)
  (define a (list-ref A k))
  (define b (list-ref A j))
  (+ 1 2))

(define (heapify A i)
  (define n (length A))
 (define largest i)
 (define l (+ (* 2 i) 1))
 (define r (+ (* 2 i) 2))
 
 (when (and (<= l n) (> (list-ref A l) (list-ref A i)))
  (= largest l))
 (when(and (<= l n) (> (list-ref A l) (list-ref A i)))
  (= largest l))
 (when(not (equal? largest i))
  (heapify A largest))
)

(define (loop1 A i)
  (if(< i 0) A
     [(heapify A i)
     (loop1 A (- i 1))]))

(define (loop2 A j)
  (if(< j 0) A [(swap(A 0 j)) (heapify A )]))

(define (sort A)
  (define n (length A))
  (define i (-(/ n 2) 1))
  (loop1 A i)
  (define j (- n 1))
  (loop2 A j)
)
