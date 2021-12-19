#lang typed/racket

(require
  math/matrix
  math/array)

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


(define (solve [scanners : (Listof Scanner)]) : (Pairof (Listof transform) Scanner)
  (define (iter [ts : (Listof transform)] [fixed : Scanner] [ss : (Listof Scanner)]) : (Pairof (Listof transform) Scanner)
    (match ss
      [(cons x xs)
       (let ([t (find-transform fixed x)])
         (if t
             (iter (cons t ts) (set-union fixed (transform-scanner t x)) xs)
             (iter ts fixed (append xs (list x)))))]
      [(list) (cons ts fixed)]))
  (iter '() (first scanners) (rest scanners)))

(define result : (Pairof (Listof transform) Scanner)
  (solve input))

(define solve1
  (set-count (cdr result)))

(println (set-count (cdr result)))

(define (solve2 [transforms : (Listof transform)])
  (for*/fold ([m : Integer 0])
             ([t1 (in-list transforms)]
              [t2 (in-list transforms)]) : Integer
    (max m
         (array-all-sum
          (array-abs
           (matrix-
            (transform-translation t1)
            (transform-translation t2)))))))

(println (solve2 (car result)))

