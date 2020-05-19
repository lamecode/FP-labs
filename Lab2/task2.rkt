#lang Racket
(define (swap A k j)
  (define a (list-ref A k))
  (define b (list-ref A j))
  (list-set (list-set A k b) j a)
  )

(define (heapify A n i)
 (define largest i)
 (define l (+ (* 2 i) 1))
 (define r (+ (* 2 i) 2))
(when (and (< l n) (> (list-ref A l) (list-ref A largest)))
  (set! largest l))
 (when(and (< r n) (> (list-ref A r) (list-ref A largest)))
  (set! largest r))
 (if (not (equal? largest i))
   (heapify (swap A i largest) n largest) A)  
)

(define (loop1 A i)
  (cond
    [(< i 0) A]
  [(loop1 (heapify A (length A) i) (- i 1))]))

(define (loop2 A j)
  (cond
    [(< j 0) A]
     [(loop2 (heapify (swap A 0 j) j 0) (- j 1))]))

(define (sort A n)
  (define i (floor (-(/ n 2) 1)))
  (define j (- n 1))
  (writeln (loop2 (loop1 A i) j))
)

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream (letrec ([f (lambda (x) (cons x (lambda () (f (* x -2)))))])
                              (lambda () (f 1))))


(define lst (stream-for-n-steps funny-number-stream (string->number (read-line (current-input-port)))))
(sort lst (length lst))

