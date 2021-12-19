#lang typed/racket

(require
  math/matrix
  math/array)

(define-type Scanner (Setof (Matrix Integer)))

(require/typed
 "day19-parser.rkt"
 [read-input (-> String (Listof Scanner))])

(define test-input
  (read-input "inputs/19-test.txt"))

(define input
  (read-input "inputs/19.txt"))

(define (cross-product-matrix [v : (Matrix Real)]) : (Matrix Real)
  (match-let ([(list a b c) (matrix->list v)])
    (matrix [[0 (- c) b]
             [c 0 (- a)]
             [(- b) a 0]])))

(define (rotation-matrix [omega : (Matrix Real)] [theta : Real]) : (Matrix Real)
  (let ([omega-cross (cross-product-matrix omega)])
    (matrix+ (identity-matrix 3)
             (matrix-scale omega-cross (sin theta))
             (matrix-scale (matrix-expt omega-cross 2) (- 1 (cos theta))))))

(define basis
  (list (col-matrix [1 0 0])
        (col-matrix [0 1 0])
        (col-matrix [0 0 1])))

(define angles
  (build-list 4 (lambda ([i : Real]) (* pi (/ i 4.0)))))

(define rotations
  (for*/set ([omega1 basis]
             [theta1 angles]
             [omega2 (take basis 3)]
             [theta2 (take angles 4)]
             [omega3 (take basis 2)]
             [theta3 (take angles 3)]) : (Setof (Matrix Integer))
    (matrix-map
     exact-round
     (matrix* (rotation-matrix omega1 theta1)
              (rotation-matrix omega2 theta2)
              (rotation-matrix omega3 theta3)))))


(struct transform ([rotation : (Matrix Integer)] [translation : (Matrix Integer)]) #:transparent)

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

(define scanner0 (first test-input))
(define scanner1 (second test-input))

(println (set-count rotations))

(println (solve1 input))
