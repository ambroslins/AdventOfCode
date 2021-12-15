#lang typed/racket

(require
  racket/file
  math/array
  pfds/heap/binomial
  )

(define input : (Array Integer)
  (let* ([flat (for*/array ([line (file->lines "inputs/15.txt")]
                            [c (in-string line)]) : Integer
                 (assert (string->number (string c)) exact-integer?))]
         [size (assert (sqrt (array-size flat)) index?)])
    (array-reshape flat (vector size size))))

(struct node ([cost : Integer] [index : In-Indexes]) #:transparent)

(define (sum [xs : (Sequenceof Integer)])
  (for/sum ([x xs]) : Integer x))

(define (node< [n1 : node] [n2 : node])
  (if (= (node-cost n1) (node-cost n2))
      (>= (sum (node-index n1)) (sum (node-index n2)))
      (< (node-cost n1) (node-cost n2))))

(define (index-ref [index : In-Indexes] [i : Integer]) : Integer
  (assert (vector-ref index i) exact-integer?))

(define (neighbour-indexes [shape : In-Indexes] [index : In-Indexes]) : (Listof In-Indexes)
  (for*/list ([n : Integer (in-range (vector-length index))]
              [d : Integer '(-1 1)]
              #:when (< -1 (+ (index-ref index n) d) (index-ref shape n))) : (Listof In-Indexes)
    (build-vector (vector-length index)
                  (lambda ([j : Integer]) (if (= n j)
                                              (+ d (index-ref index j))
                                              (index-ref index j))
                    )))
  )

(define (a* [grid : (Array Integer)] [start : In-Indexes] [end : In-Indexes])
  (define shape (array-shape grid))
  (define seen : (Mutable-Array Boolean)
    (array->mutable-array (make-array shape #f)))
  (define (iter [to-search : (Heap node)]) : (U Integer #f)
    (if (empty? to-search)
        #f
        (let* ([current (find-min/max to-search)]
               [current-ix (node-index current)]
               [cost (node-cost current)])
          (array-set! seen current-ix #t)
          (if (equal? end current-ix)
              (node-cost current)
              (iter (for/fold ([s (filter
                                   (lambda ([n : node]) (not (equal? (node-index n) current-ix)))
                                   to-search)])
                              ([ix (neighbour-indexes shape (node-index current))]
                               #:unless (array-ref seen ix))
                      (insert (node (+ (array-ref grid ix) (node-cost current)) ix) s)))))))
  (iter (heap node< (node 0 start))))

(define (solve [grid : (Array Integer)])
  (a* grid #(0 0) (vector-map (lambda ([x : Integer]) (- x 1)) (array-shape grid))))

(println (solve input))

(define (tile-array [arr : (Array Integer)] [axis : Integer])
  (array-list->array
   (build-list 5 (lambda ([n : Integer])
                   (let ([a (array+ arr (array n))])
                     (array-if (array> a (array 9))
                               (array- a (array 9))
                               a))))
   axis))


(define big-input
  (array-reshape
   (tile-array (tile-array input 1) 0)
   (vector-map (lambda ([x : Integer]) (* x 5)) (array-shape input))))

(println (solve big-input))
