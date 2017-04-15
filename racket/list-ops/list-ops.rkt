#lang racket

(provide my-length
         ; my-reverse
         my-map
         ; my-filter
         ; my-fold
         ; my-append
         ; my-concatenate
)

(define (my-length lst)
  (cond
   [(empty? lst) 0]
   [else (+ 1 (my-length (rest lst)))]))

(define (my-length lst)
  (define (helper lst len)
    (cond
     [(empty? lst) len]
     [else (helper (rest lst) (+ len 1))]))
  (helper lst 0))

(define (my-map f lst)
  (cond
   [(empty? lst) empty]
   [else (cons (f (first lst))
               (my-map f (rest lst)))]))

(define (my-map f lst)
  (define (helper lst acc)
    (cond
      [(empty? lst) acc]
      [else (helper (rest lst)
                    (cons (f (first lst)) acc))]))
  (reverse (helper lst empty)))
