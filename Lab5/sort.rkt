#lang racket
(provide sort)

(define (swap A k j)
  (define a (list-ref A k))
  (define b (list-ref A j))
  (list-set (list-set A k b) j a)
  )

(define (heapify A B n i)
 (define largest i)
 (define l (+ (* 2 i) 1))
 (define r (+ (* 2 i) 2))
(when (and (< l n) (string-ci>=? (list-ref B l) (list-ref B largest)))
  (set! largest l))
 (when(and (< r n) (string-ci>=? (list-ref B r) (list-ref B largest)))
  (set! largest r))
 (if (not (equal? largest i))
   (heapify (swap A i largest) (swap B i largest) n largest) (list A B))  
)

(define (loop1 A B i)
  (cond
    [(< i 0) (list A B)]
  [(loop1 (first (heapify A B (length A) i)) (second (heapify A B (length A) i)) (- i 1))]))

(define (loop2 A B j)
  (cond
    [(< j 0) A]
     [(loop2 (first (heapify (swap A 0 j) (swap B 0 j) j 0)) (second (heapify (swap A 0 j) (swap B 0 j) j 0)) (- j 1))]))

(define (sort A B)
  (define n (length A))
  (define i (floor (-(/ n 2) 1)))
  (define j (- n 1))
  (loop2 (first (loop1 A B i)) (second (loop1 A B i)) j)
)