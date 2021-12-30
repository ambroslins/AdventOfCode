#lang racket

(require racket/file)
(require threading)

(define input
  (list->vector
   (map
    (lambda~> (string-trim) (string->number))
    (string-split (file->string "inputs/7.txt") ","))))

(define (median xs)
  (vector-ref (vector-sort xs <) (quotient (vector-length xs) 2)))

(define (fuel-costs1 positions target)
  (for/sum ([p positions])
    (abs (- p target))))

(define (solve1 positions)
  (fuel-costs1 positions (median positions)))

(println (solve1 input))

(define (mean xs)
  (/ (for/sum ([x xs]) x) (vector-length xs)))

(define (fuel-costs2 positions target)
  (for/sum ([p positions])
    (let ([n (abs (- p target))])
      (quotient (* n (+ n 1)) 2)
      )))

(define (solve2 positions)
  (min
   (fuel-costs2 positions (exact-floor (mean positions)))
   (fuel-costs2 positions (exact-ceiling (mean positions)))))

(println (solve2 input))

