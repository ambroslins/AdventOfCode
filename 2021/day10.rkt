#lang racket

(require racket/file)

(define input
  (file->lines "inputs/10.txt"))

(define syntax-score
  (match-lambda [#\) 3]
                [#\] 57]
                [#\} 1197]
                [#\> 25137]))

(define matching-characters
  (hash #\( #\) #\[ #\] #\{ #\} #\< #\>))

(define opening
  (hash-keys matching-characters))

(define closing
  (hash-values matching-characters))

(struct corrupted (char) #:transparent)

(define (check-syntax line)
  (for/fold ([stack '()])
            ([c (in-string line)]
             #:break (corrupted? stack))
    (cond
      [(member c opening) (cons c stack)]
      [(equal? c (hash-ref matching-characters (first stack))) (rest stack)]
      [else (corrupted c)])))


(define (solve1 lines)
  (for/sum
      ([check (map check-syntax lines)]
       #:when (corrupted? check))
    (syntax-score (corrupted-char check))
    ))

(println (solve1 input))

(define autocomplete-points
  (match-lambda [#\) 1]
                [#\] 2]
                [#\} 3]
                [#\> 4]))

(define (autocomplete-score chars)
  (for/fold ([acc 0])
            ([c (in-list chars)])
    (+ (* acc 5)
       (autocomplete-points (hash-ref matching-characters c)))))

(define (solve2 lines)
  (let ([scores
         (for/list ([check (map check-syntax lines)]
                    #:when (list? check))
           (autocomplete-score check))])
    (list-ref (sort scores <) (quotient (length scores) 2))))

(println (solve2 input))
