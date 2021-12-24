#lang racket

(require
  racket/file
  megaparsack megaparsack/text
  data/applicative data/monad)

(struct inp (a) #:transparent)
(struct add (a b) #:transparent)
(struct mul (a b) #:transparent)
(struct div (a b) #:transparent)
(struct mod (a b) #:transparent)
(struct eql (a b) #:transparent)

(define number/p
  (do [sign <- (or/p (do (char/p #\-) (pure -)) (pure +))]
    [i <- integer/p]
    (pure (sign i))))

(define input/p
  (do (string/p "inp ")
    [a <- letter/p]
    (pure (inp a))))

(define (make-instruction/p s i)
  (do (try/p (string/p s))
    space/p
    [a <- letter/p]
    space/p
    [b <- (or/p letter/p number/p)]
    (pure (i a b))))

(define instruction/p
  (or/p
   input/p
   (make-instruction/p "add" add)
   (make-instruction/p "mul" mul)
   (make-instruction/p "div" div)
   (make-instruction/p "mod" mod)
   (make-instruction/p "eql" eql)))

(define input
  (for/list ([line (file->lines "inputs/24.txt")])
    (parse-result! (parse-string instruction/p line))))

(define (exec state proc a b)
  (let ([y (if (number? b) b (hash-ref state b))])
    (hash-update state a (lambda (x) (proc x y)))))

(define (solve digits state instructions)
  (if (empty? instructions)
      (if (= 0 (hash-ref state #\z)) digits #f)
      (match (first instructions)
        [(add a b) (solve digits (exec state + a b) (rest instructions))]
        [(mul a b) (solve digits (exec state * a b) (rest instructions))]
        [(div a b) (solve digits (exec state quotient a b) (rest instructions))]
        [(mod a b) (solve digits (exec state remainder a b) (rest instructions))]
        [(eql a b) (solve digits (exec state (lambda (x y) (if (= x y) 1 0)) a b) (rest instructions))]
        [(inp a) (for/or ([d (in-inclusive-range 9 1 -1)])
                   (solve (cons d digits) (hash-set state a d) (rest instructions))) ])))

(define initial-state
  (for/hash ([v (in-string "wxyz")]) (values v 0)))

(println (solve '() initial-state input))

