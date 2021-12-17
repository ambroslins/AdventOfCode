#lang racket

(require
  racket/file
  megaparsack megaparsack/text
  data/applicative data/monad)

(struct point (x y) #:transparent)

(struct target (start end) #:transparent)

(struct probe (position velocity) #:transparent)

(define int/p
  (do [sign <- (or/p (do (char/p #\-) (pure -)) (pure identity))]
    [i <- integer/p]
    (pure (sign i))))

(define input/p
  (do (string/p "target area: x=")
    [xmin <- int/p]
    (string/p "..")
    [xmax <- int/p]
    (string/p ", y=")
    [ymin <- int/p]
    (string/p "..")
    [ymax <- int/p]
    (pure (target (point xmin ymin) (point xmax ymax)))))

(define input
  (parse-result! (parse-string input/p (file->string "inputs/17.txt"))))

(define example
  (target (point 20 -10) (point 30 -5)))

(define (signum n)
  (cond [(= n 0) 0]
        [(> n 0) 1]
        [(< n 0) -1]))
(define step
  (match-lambda
    [(probe (point x y) (point vx vy))
     (probe
      (point (+ x vx) (+ y vy))
      (point (- vx (signum vx)) (- vy 1))) ]))

(define (launch p)
  (stream-cons p (launch (step p))))

(define (within-target t ps)
  (match-let ([(target (point xmin ymin) (point xmax ymax)) t])
    (for/fold ([res #f])
              ([p (in-stream ps)]
               #:break (or res
                           (> (point-x (probe-position p)) xmax)
                           (< (point-y (probe-position p)) ymin)))
      (and (<= xmin (point-x (probe-position p)) xmax)
           (<= ymin (point-y (probe-position p)) ymax)))))

(define (solve1 t)
  (let ([n (abs (+ (point-y (target-start t)) 1))])
    (/ (* n (+ n 1)) 2)))

(println (solve1 input))

(define (solve2 t)
  (match-let ([(target (point xmin ymin) (point xmax ymax)) t])
    (for*/sum
        ([vx (in-inclusive-range (exact-floor (sqrt xmin)) xmax)]
         [vy (in-inclusive-range ymin (abs ymin))]
         #:when (within-target t (launch (probe (point 0 0) (point vx vy)))))
      1)))

(println (solve2 input))