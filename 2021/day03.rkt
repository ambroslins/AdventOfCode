#lang racket

(require racket/file)

(define input
  (for/list ([line (file->lines "inputs/3.txt")])
    (for/list ([c (string->list line)])
      (match c
        [#\0 #f]
        [#\1 #t]))))

(define (bit-list->integer xs)
  (foldl (lambda (x acc) (+ (* acc 2) (if x 1 0))) 0 xs))

(define (transpose xss)
  (apply map list xss))

(define (most-common-bit xs)
  (>= (for/sum ([x xs]) (if x 1 0)) (/ (length xs) 2)))

(define (least-common-bit xs)
  (not (most-common-bit xs)))

(define (solve1 xs)
  (let* ( [gamma (map most-common-bit (transpose xs))]
          [epsilon (map not gamma)])
    (* (bit-list->integer gamma) (bit-list->integer epsilon))))

(println (solve1 input))

(define (rating proc xss)
  (first
   (for/fold ([xs xss])
             ([i (in-range (length (first xss)))])
     (let ([b (proc (for/list ([x xs]) (list-ref x i)))])
       (if (eq? (length xs) 1) xs (filter (lambda (x) (eq? b (list-ref x i))) xs))))))

(define (solve2 xs)
  (let ([oxygen (rating most-common-bit xs)]
        [co2 (rating least-common-bit xs)])
    (* (bit-list->integer oxygen) (bit-list->integer co2))))

(println (solve2 input))