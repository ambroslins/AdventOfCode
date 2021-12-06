#lang racket

(require racket/file)
(require threading)

(define input
  (map
   (lambda~> (string-trim) (string->number))
   (string-split (file->string "inputs/6.txt") ",")))

(define (solve-one days)
  (if (< days 9)
      1
      (+ (stream-ref solve-stream (- days 7)) (stream-ref solve-stream (- days 9)))))

(define solve-stream
  (for/stream
      ([i (in-naturals)])
    (solve-one i)))

(define (solve days fish)
  (for/sum ([f fish])
    (stream-ref solve-stream (- (+ days 8) f))))

(println (solve 80 input))

(println (solve 256 input))
