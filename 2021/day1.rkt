#lang racket

(require racket/file)
(require algorithms)

(define input (map string->number (file->lines "inputs/1.txt")))

(define (part1 xs) (count positive? (adjacent-map xs (lambda (x y) (- y x)))))

(println (part1 input))

(define (part2 xs) (part1 (map sum (sliding xs 3 1))))

(println (part2 input))