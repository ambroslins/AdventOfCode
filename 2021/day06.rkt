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

(define (step-day fish)
  (let ([len (vector-length fish)])
    (build-vector
     len
     (lambda (i)
       (let ([v (vector-ref fish (modulo (add1 i) len))])
         (if
          (= i 6)
          (+ v (vector-ref fish 0))
          v))))))

(define (fish->vector fish)
  (define v (make-vector 9))
  (for ([f fish])
    (vector-set! v f (add1 (vector-ref v f))))
  v )

(define (solve-vector days fish)
  (apply
   +
   (vector->list
    (for/fold ([f (fish->vector fish)])
              ([i (in-range days)])
      (step-day f)))))

(println (solve 80 input))

(println (solve 256 input))

(println (solve-vector 80 input))

(println (solve-vector 256 input))
