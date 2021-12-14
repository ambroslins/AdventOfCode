#lang racket

(require
  racket/file
  racket/hash)

(define input
  (match (file->lines "inputs/14.txt")
    [(list template "" rules ...)
     (cons (string->list template) rules)]))

(define template
  (for/fold ([polymer (hash)])
            ([a (in-list (car input))]
             [b (in-list (rest (car input)))])
    (hash-update polymer (cons a b) add1 0)))

(define rules
  (for/hash ([rule (in-list (cdr input))])
    (match-let
        ([(list (list a b) (list c)) (map string->list (string-split rule " -> "))])
      (values (cons a b) c))))

(define (step polymer)
  (for/fold ([poly (hash)])
            ([(pair n) (in-hash polymer)])
    (let ( [a (car pair)]
           [b (cdr pair)]
           [c (hash-ref rules pair)])
      (hash-union poly (hash (cons a c) n (cons c b) n) #:combine +))))

(define (polymer-iter polymer)
  (stream-cons polymer (polymer-iter (step polymer))))

(define polymer-stream
  (polymer-iter template))

(define (histogram polymer)
  (for/fold ([occur (hash (first (car input)) 1)])
            ([(pair n) (in-hash polymer)])
    (hash-union occur (hash (cdr pair) n) #:combine +)))

(define (solve n)
  (let* ([polymer (stream-ref polymer-stream n)]
         [occurrences (hash-values (histogram polymer))])
    (- (apply max occurrences) (apply min occurrences))))

(println (solve 10))
(println (solve 40))
