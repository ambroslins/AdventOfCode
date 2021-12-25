#lang racket

; solution using hoare logic
; basically we build a symbolic expression going through the instructions backwards
; then we substitue the inital values and brute force the digits
; we simplify the expressions to keep them as small as possible
; and return early if no valid solution exists

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

(define (recurse proc condition)
  (define (rec a)
    (recurse proc a))
  (match condition
    [(add a b) (proc (add (rec a) (rec b)))]
    [(mul a b) (proc (mul (rec a) (rec b)))]
    [(div a b) (proc (div (rec a) (rec b)))]
    [(mod a b) (proc (mod (rec a) (rec b)))]
    [(eql a b) (proc (eql (rec a) (rec b)))]
    [else (proc condition)]))


(define (subs condition x y)
  (recurse
   (lambda (c) (if (equal? c x) y c))
   condition))

(struct interval (min max) #:transparent)

(define (expr-interval expr)
  (define (proc x)
    (match x
      [(add a b)
       (and a b
            (interval (+ (interval-min a) (interval-min b))
                      (+ (interval-max a) (interval-max b))))]
      [(mul a b)
       (and a b
            (interval (* (interval-min a) (interval-min b))
                      (* (interval-max a) (interval-max b))))]
      [(div a b)
       (and a b
            (interval (quotient (interval-min a) (interval-max b))
                      (quotient (interval-max a) (interval-min b))))]
      [(mod a b)
       (or (and a b
                (interval (if (< (interval-max a) (interval-max b)) (max 0 (interval-min a)) 0)
                          (- (interval-max b) 1)))
           (and b (interval 0 (- (interval-max b) 1))))]
      [(eql a b) (interval 0 1)]
      [(digit n) (interval 1 9)]
      [(? number? a) (interval a a)]
      [else #f]))
  (recurse proc expr))

; this is important to keep the conditions as small as possible
(define (simplify condition)
  (define (proc x)
    (match x
      [(eql (? number? a) (? number? b)) (if (= a b) 1 0)]
      [(eql (? number? a) b) (simplify (eql b a))]
      [(eql (add a (? number? b)) c) (simplify (eql a (add c (- b))))]
      [(eql (mul a (? number? b)) (? number? c))
       (let ([x (/ c b)])
         (if (exact-integer? x)
             (simplify (eql a x))
             0)) ]
      [(eql a b)
       (=> continue)
       (let ([in-a (expr-interval a)]
             [in-b (expr-interval b)])
         (if (and in-a in-b)
             (cond
               [(< (interval-max in-a) (interval-min in-b)) 0]
               [(< (interval-max in-b) (interval-min in-a)) 0]
               [else (continue)])
             (continue)))]


      [(add (? number? a) (? number? b)) (+ a b)]
      [(add a 0) a]
      [(add 0 b) b]

      [(mul (? number? a) (? number? b)) (* a b)]
      [(mul a 0) 0]
      [(mul 0 b) 0]
      [(mul a 1) a]
      [(mul 1 b) b]

      [(div (? number? a) (? number? b)) (quotient a b)]
      [(div a 1) a]
      [(div a b)
       (=> continue)
       (let ([in-a (expr-interval a)] [in-b (expr-interval b)])
         (if (and in-a in-b
                  (< (interval-max in-a) (interval-min in-b))
                  (>= (interval-min in-a) 0))
             0
             (continue)))]


      [(mod (? number? a) (? number? b)) (remainder a b)]
      [(mod (mul a (? number? b)) (? number? c)) #:when (= b c) 0]

      [(mod (add (mul a (? number? b)) c) (? number? d)) #:when (= b d) c]

      [else x]))

  (recurse proc condition))

(struct digit (n) #:transparent)

(define (make-step n)
  (lambda (instruction post)
    (simplify
     (match instruction
       [(add a b) (subs post a (add a b))]
       [(mul a b) (subs post a (mul a b))]
       [(div a b) (subs post a (div a b))]
       [(mod a b) (subs post a (mod a b))]
       [(eql a b) (subs post a (eql a b))]
       [(inp a)
        (set! n (+ n 1))
        (subs post a (digit n))]))))

(define (run instructions)
  (define n -1)
  (foldl (make-step n) (eql #\z 0) instructions))

(define monad (simplify (subs (run (reverse input)) #\z 0)))

; brute-force all the numbers
; however this will stop early if no solution for a given prefix exists
(define (model-number in-digit digits m)
  (cond
    [(equal? 1 m) digits]
    [(equal? 0 m) #f]
    [(> (length digits) 14) #f]
    [else (for/or ([d in-digit])
            (model-number
             in-digit
             (cons d digits)
             (simplify (subs m (digit (- 13 (length digits))) d))))]))

(println monad)
(println (reverse (model-number (in-inclusive-range 9 1 -1) '() monad)))
(println (reverse (model-number (in-inclusive-range 1 9) '() monad)))

