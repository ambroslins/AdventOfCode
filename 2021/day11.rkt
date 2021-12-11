#lang racket

(require
  racket/file
  math/array)

(define input
  (vector*->array
   (for/vector ([line (file->lines "inputs/11.txt")])
     (for/vector ([c (in-string line)])
       (string->number (list->string (list c)))))
   number?))

(define (adjacent-offsets n)
  (if (< n 1)
      (list (list))
      (for*/list ([offsets (adjacent-offsets (- n 1))]
                  [d (list -1 0 1)])
        (cons d offsets))))

(define (inside? index shape)
  (for/and ([i (in-vector index)]
            [s (in-vector shape)])
    (< -1 i s)))

(define (adjacent-indexes shape index)
  (let* ([n (vector-length shape)]
         [indexes
          (for/list
              ([offset (adjacent-offsets n)]
               #:unless (apply = 0 offset))
            (for/vector #:length n
              ([d (in-list offset)]
               [i (in-vector index)])
              (+ i d)))])
    (filter (lambda (i) (inside? i shape)) indexes)))

(define (try-flash! grid index seen)
  (if (or (set-member? seen index) (< (array-ref grid index) 10))
      seen
      (let ([adjacent (adjacent-indexes (array-shape grid) index)])
        (for ([js (in-list adjacent)])
          (array-set! grid js (add1 (array-ref grid js))))
        (for/fold ([s (set-add seen index)])
                  ([js (in-list adjacent)])
          (try-flash! grid js s)))))

(define (step grid)
  (let ([mgrid (array->mutable-array (array+ grid (array 1)))])
    (for/fold ([seen (set)])
              ([js (in-array-indexes (array-shape grid))])
      (try-flash! mgrid js seen))
    (array-if (array> mgrid (array 9))
              (array 0)
              mgrid)))

(define (stream-iterate proc val)
  (stream-cons val (stream-iterate proc (proc val))))

(define octopuse-stream
  (stream-iterate step input))

(define solve1
  (for/sum ([g (in-stream (stream-take octopuse-stream 101))])
    (array-count zero? g)))

(println solve1)

(define solve2
  (for/first ([(g i) (in-indexed (in-stream octopuse-stream))]
              #:when (array-all-and (array= g (array 0))))
    i))

(println solve2)