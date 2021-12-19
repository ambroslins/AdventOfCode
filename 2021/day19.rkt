#lang typed/racket

(require
  math/matrix)

(define-type Scanner (Setof (Matrix Integer)))

(require/typed
 "day19-parser.rkt"
 [read-input (-> String (Listof Scanner))])

(define input
  (read-input "inputs/19.txt"))

(define rotate-x : (Matrix Integer)
  (matrix [[1 0 0]
           [0 0 -1]
           [0 1 0 ]]))

(define rotate-y : (Matrix Integer)
  (matrix [[0 0 1]
           [0 1 0]
           [-1 0 0]]))

; https://stackoverflow.com/a/16453299/12893756
(define rotations
  (for*/set ([a (in-range 4)]
             [b (in-range 4)]
             [c (in-range 4)]
             [d (in-range 4)]) : (Setof (Matrix Integer))
    (matrix-map (lambda (x) (assert x exact-integer?))
                (matrix* (matrix-expt rotate-x a)
                         (matrix-expt rotate-y b)
                         (matrix-expt rotate-x c)
                         (matrix-expt rotate-y d)))))

(struct transform
  ([rotation : (Matrix Integer)]
   [translation : (Matrix Integer)])
  #:transparent)

(define (transform-scanner [t : transform] [scanner : Scanner])
  (for/set ([s (in-set scanner)]) : Scanner
    (matrix+ (transform-translation t)
             (matrix* (transform-rotation t) s))))

(define (overlap [t : transform] [s1 : Scanner] [s2 : Scanner])
  (set-intersect s1 (transform-scanner t s2)))

(define (find-transform [s1 : Scanner] [s2 : Scanner])
  (for*/fold ([trans : (U transform #f) #f])
             ([rot (in-set rotations)]
              [b1 (drop (set->list s1) 11)]
              [b2 (drop (set->list s2) 11)]
              [t (list (transform rot (matrix- b1 (matrix* rot b2))))]
              #:break trans)
    (if (>= (set-count (overlap t s1 s2)) 12) t #f)))

(define (solve1 [scanners : (Listof Scanner)]) : Integer
  (define (iter [fixed : Scanner] [ss : (Listof Scanner)]) : Scanner
    (println (length ss))
    (match ss
      [(cons x xs)
       (let ([t (find-transform fixed x)])
         (println t)
         (if t
             (iter (set-union fixed (transform-scanner t x)) xs)
             (iter fixed (append xs (list x)))))]
      [(list) fixed]))
  (set-count (iter (first scanners) (rest scanners))))

(println (solve1 input))
