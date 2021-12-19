#lang racket

(provide read-input)

(require
  racket/file
  math/matrix
  megaparsack megaparsack/text
  (prefix-in f: data/functor)
  data/applicative data/monad)

(define number/p
  (do [sign <- (or/p (do (char/p #\-) (pure -)) (pure +))]
    [i <- integer/p]
    (pure (sign i))))

(define beacon/p
  (do [xs <- (many/p #:sep (char/p #\,) #:min 3 #:max 3 number/p)]
    (many/p space/p)
    (pure (->col-matrix xs))))

(define scanner/p
  (do (string/p "--- scanner ")
    [i <- integer/p]
    (string/p " ---")
    (many+/p space/p)
    (f:map list->set (many+/p (try/p beacon/p)))))

(define (read-input path)
  (parse-result! (parse-string (many+/p scanner/p) (file->string path))))
