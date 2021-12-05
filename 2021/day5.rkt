#lang racket

(require racket/file)
(require threading)
(require data/applicative data/monad )
(require megaparsack megaparsack/text)

(struct point (x y))

(struct line (start end))

(define point/p
  (do [x <- integer/p]
    (char/p #\,)
    [y <- integer/p]
    (pure (point x y))))

(define line/p
  (do [start <- point/p]
    (string/p " -> ")
    [end <- point/p]
    (pure (line start end))))

(define input
  (parse-result!
   (parse-string
    (many/p line/p #:sep (many+/p space/p))
    (file->string "inputs/5.txt"))))

(define (line-hor-or-ver? l)
  (let ([start (line-start l)]
        [end (line-end l)])
    (or
     (= (point-x start) (point-x end))
     (= (point-y start) (point-y end)))) )

(define (point->pair p)
  (cons (point-x p) (point-y p)))

(define (sign n)
  (cond [(= n 0) 0]
        [(> n 0) 1]
        [(< n 0) -1]))

(define line->points
  (match-lambda
    [(line (point x1 y1) (point x2 y2))
     (let* ([dx (- x2 x1)]
            [dy (- y2 y1)]
            [d (max (abs dx) (abs dy))])
       (for/list
           ([i (in-inclusive-range 0 d)])
         (point
          (+ x1 (* i (sign dx)))
          (+ y1 (* i (sign dy))))))]))

(define solve
  (lambda~>>
   (map line->points)
   (flatten)
   (group-by point->pair)
   (filter (lambda (ps) (>= (length ps) 2)))
   (length)
   ))

(println (solve (filter line-hor-or-ver? input)))
(println (solve input))