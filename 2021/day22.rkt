#lang racket

(require
  racket/file
  megaparsack megaparsack/text
  data/applicative data/monad)

(define number/p
  (do [sign <- (or/p (do (char/p #\-) (pure -)) (pure +))]
    [i <- integer/p]
    (pure (sign i))))

(struct point (x y z) #:transparent)

(define (point->list p)
  (list (point-x p) (point-y p) (point-z p)))

(struct range (from to) #:transparent)

(struct cuboid (start end) #:transparent)

(define (ranges->cuboid rs)
  (cuboid (apply point (map range-from rs))
          (apply point (map range-to rs))))

(struct step (cuboid on?) #:transparent)

(define on/off/p
  (do (char/p #\o)
    (or/p (do (string/p "n") (pure #t))
          (do (string/p "ff") (pure #f)))))

(define (coord/p axis)
  (do (char/p axis)
    (char/p #\=)
    [low <- number/p]
    (string/p "..")
    [high <- number/p]
    (many/p #:max 1 (char/p #\,))
    (many/p space/p)
    (pure (cons low high))))

(define step/p
  (do [on <- on/off/p]
    space/p
    [x <- (coord/p #\x)]
    [y <- (coord/p #\y)]
    [z <- (coord/p #\z)]
    (pure (step (cuboid
                 (point (car x) (car y) (car z))
                 (point (add1 (cdr x)) (add1 (cdr y)) (add1 (cdr z))))
                on))))

(define input
  (parse-result! (parse-string (many+/p step/p) (file->string "inputs/22.txt"))))


(define (cuboid-volume c)
  (for/product ([a (point->list (cuboid-start c))]
                [b (point->list (cuboid-end c))])
    (- b a)))

; https://stackoverflow.com/a/15812696
(define (line-overlap a b c d )
  (if (and (>= b c) (>= d a))
      (range (max a c) (min b d))
      #f))

(define (cuboid-intersection c1 c2)
  (let ([overlaps
         (for/list ([a (point->list (cuboid-start c1))]
                    [b (point->list (cuboid-end c1))]
                    [c (point->list (cuboid-start c2))]
                    [d (point->list (cuboid-end c2))])
           (line-overlap a b c d))])
    (if (andmap identity overlaps)
        (ranges->cuboid overlaps)
        #f )))

(define (cuboid-divide outer inner)
  (let* ([start-points (list (cuboid-start outer) (cuboid-start inner) (cuboid-end inner))]
         [end-points (list (cuboid-start inner) (cuboid-end inner) (cuboid-end outer))]
         [ranges (lambda (axis)
                   (for/list ([start start-points] [end end-points])
                     (range (axis start) (axis end))))])
    (for*/list
        ([x-range (ranges point-x)]
         [y-range (ranges point-y)]
         [z-range (ranges point-z)]
         [c (in-value (ranges->cuboid (list x-range y-range z-range)))]
         #:unless (= (cuboid-volume c) 0))
      c)))

(define (cuboid-difference c1 c2)
  (let ([intersection (cuboid-intersection c1 c2)])
    (if intersection
        (remove intersection (cuboid-divide c1 intersection))
        (list c1))))

(define (apply-steps steps)
  (for/fold ([cuboids (list)])
            ([step steps])
    (let ([differences
           (append-map
            (lambda (c) (cuboid-difference c (step-cuboid step)))
            cuboids)])
      (if (step-on? step)
          (cons (step-cuboid step) differences)
          differences))))

(define (solve steps)
  (for/sum ([cuboid (apply-steps steps)])
    (cuboid-volume cuboid)))

(define (initialization-step? s)
  (and (<= -50 (apply min (point->list (cuboid-start (step-cuboid s)))))
       (>= 50 (apply max (point->list (cuboid-end (step-cuboid s)))))))

(println (solve (takef input initialization-step?)))

(println (solve input))

; (for-each println (apply-steps input))




