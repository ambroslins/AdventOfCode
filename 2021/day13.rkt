#lang racket

(require
  racket/file
  megaparsack megaparsack/text
  data/applicative data/monad)

(struct dot (x y) #:transparent)

(struct fold-x (value))
(struct fold-y (value))

(struct manual (dots folds) #:transparent)

(define dot/p
  (do [x <- integer/p]
    (char/p #\,)
    [y <- integer/p]
    (pure (dot x y))))

(define fold/p
  (do (string/p "fold along ")
    [f <- (or/p
           (do (char/p #\x) (pure fold-x))
           (do (char/p #\y) (pure fold-y)))]
    (char/p #\=)
    [value <- integer/p]
    (pure (f value))))

(define (lines/p p)
  (many/p (do [x <- p] (many/p #:max 1 space/p) (pure x))))

(define input/p
  (do [dots <- (lines/p dot/p)]
    (many+/p space/p)
    [folds <- (lines/p fold/p)]
    (pure (manual (list->set dots) folds))))

(define input
  (parse-result! (parse-string input/p (file->string "inputs/13.txt"))))

(define (do-fold fold dots)
  (let ([f (match fold
             [(fold-x n)
              (match-lambda [(dot x y)
                             (if (> x n)
                                 (dot (- (* n 2) x) y)
                                 (dot x y))])]
             [(fold-y n)
              (match-lambda [(dot x y)
                             (if (> y n)
                                 (dot x (- (* n 2) y))
                                 (dot x y))])])])
    (for/set ([d (in-set dots)])
      (f d))
    ))

(define (solve1 man)
  (set-count
   (do-fold
    (first (manual-folds man))
    (manual-dots man))))

(println (solve1 input))

(define dots
  (foldl
   do-fold
   (manual-dots input)
   (manual-folds input)))

(for ([y (in-range 6)])
  (displayln
   (build-string
    40
    (lambda (x)
      (if (set-member? dots (dot x y))
          #\#
          #\.)))))
