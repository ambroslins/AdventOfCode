#lang racket

(require racket/file)

(define input
  (for/vector ([line (file->lines "inputs/9.txt")])
    (for/vector ([c (in-string line)])
      (string->number (list->string (list c))))))

(struct point (x y value) #:transparent)

(define (grid-point grid x y)
  (point x y (vector-ref (vector-ref grid y) x)))

(define (adjacent grid x y)
  (let ([nx (vector-length (vector-ref grid 0))]
        [ny (vector-length grid)])
    (for/list
        ([dx (list 0 -1 0 1)]
         [dy (list -1 0 1 0)]
         #:when (and (< -1 (+ x dx) nx) (< -1 (+ y dy) ny)))
      (grid-point grid (+ x dx) (+ y dy)))))

(define (low-points grid)
  (for*/list
      ([(row y) (in-indexed grid)]
       [(v x) (in-indexed row)]
       #:when (for/and ([n (adjacent grid x y)]) (< v (point-value n))))
    (point x y v)))

(define (solve1 grid)
  (for/sum ([low-point (low-points grid)])
    (add1 (point-value low-point))))

(println (solve1 input))

(define (basin grid p points)
  (if (> (point-value p) 8)
      points
      (for/fold ([ps (set-add points p)])
                ([n (adjacent grid (point-x p) (point-y p))]
                 #:unless (set-member? ps n))
        (basin grid n ps))))

(define (solve2 grid)
  (let ([basins (for/list ([low (low-points grid)])
                  (set-count (basin grid low (set))))])
    (for/product ([v (take (sort basins >) 3)])
      v)))

(println (solve2 input))