#lang racket

(require
  racket/file
  racket/hash)

(define input
  (for/fold ([graph (hash)])
            ([line (file->lines "inputs/12.txt")])
    (match-let ([(list from to) (string-split line "-")])
      (hash-union
       graph
       (hash from (set to) to (set from))
       #:combine set-union))))

(define (small-cave? node)
  (for/and ([c (in-string node)])
    (char-lower-case? c)))


(define (search explore start end)
  (define (search-iter path)
    (match path
      [(list) null]
      [(list current previous ...)
       (if (string=? current end)
           (list path)
           ( for*/list
                ([next (in-list (explore current path))]
                 [p (search-iter (cons next path))])
              p))]))
  (map reverse (search-iter (list start))))

(define (solve1 graph)
  (define (explore current path)
    (for/list
        ([next (in-set (hash-ref graph current))]
         #:unless (and (small-cave? next) (member next path)))
      next)
    )
  (length (search explore "start" "end")))

(println (solve1 input))

(define (solve2 graph)
  (define (explore current path)
    (for/list
        ( [next (in-set (hash-ref graph current))]
          #:unless (and
                    (small-cave? next)
                    (let ([smalls (filter small-cave? path)])
                      (if (check-duplicates smalls)
                          (member next smalls)
                          (string=? next "start")
                          ))))
      next))
  (length (search explore "start" "end")))

(println (solve2 input))