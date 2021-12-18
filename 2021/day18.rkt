#lang racket

(require
  racket/file
  megaparsack megaparsack/text
  data/applicative data/monad
  threading)

(struct tree (left right) #:transparent)

(struct left (tree) #:transparent)
(struct right (tree) #:transparent)

(struct zipper (tree path) #:transparent)

(define number/p
  (or/p integer/p
        (do (string/p "[")
          [x <- number/p]
          (string/p ",")
          [y <- number/p]
          (string/p "]")
          (pure (tree x y)))))

(define input
  (for/list ([line (file->lines "inputs/18.txt")])
    (parse-result! (parse-string number/p line))))

(define go-left
  (match-lambda
    [(zipper (tree l r) path) (zipper l (cons (left r) path))]))

(define go-right
  (match-lambda
    [(zipper (tree l r) path) (zipper r (cons (right l) path))]))

(define go-up
  (match-lambda
    [(zipper t (cons (left r) path)) (zipper (tree t r) path)]
    [(zipper t (cons (right l) path)) (zipper (tree l t) path)]))

(define (go-top z)
  (if (empty? (zipper-path z))
      z
      (go-top (go-up z))))

(define example
  (parse-result! (parse-string number/p "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))

(define (add-first-left x path)
  (define rec
    (match-lambda
      [(tree l r) (tree l (rec r))]
      [(? number? y) (+ x y)]))
  (match path
    [(cons (right t) ps) (cons (right (rec t)) ps)]
    [(cons (left t) ps) (cons (left t) (add-first-left x ps))]
    [(list) '()]))

(define (add-first-right x path)
  (define rec
    (match-lambda
      [(tree l r) (tree (rec l) r)]
      [(? number? y) (+ x y)]))
  (match path
    [(cons (left t) ps) (cons (left (rec t)) ps)]
    [(cons (right t) ps) (cons (right t) (add-first-right x ps))]
    [(list) '()]))

(define (backtrack proc z)
  (match (zipper-path z)
    [(cons (left r) path) (proc (go-right (go-up z)))]
    [(cons (right l) path) (backtrack proc (go-up z))]
    [(list) z]))

(define (explode z)
  (match z
    [(zipper (tree (? number? x) (? number? y)) path)
     #:when (>= (length path) 4)
     (explode
      (go-top
       (zipper 0 (~>> path
                      (add-first-left x)
                      (add-first-right y)))))]
    [(zipper (tree l r) path) (explode (go-left z))]
    [else (backtrack explode z)]))

(define (split z)
  (match z
    [(zipper (? number? x) path)
     (if (>= x 10)
         (reduce
          (go-top
           (zipper
            (tree (exact-floor (/ x 2.0)) (exact-ceiling (/ x 2.0)))
            path)))
         (backtrack split z))]
    [else (split (go-left z))]))

(define (reduce z)
  (split (explode z)))

(define (reduce-tree t)
  (zipper-tree (reduce (zipper t '()))))

(define (magnitude t)
  (if (number? t)
      t
      (+ (* 3 (magnitude (tree-left t)))
         (* 2 (magnitude (tree-right t))))))

(define (solve1 ts)
  (magnitude
   (for/fold ([x #f]) ([t (in-list ts)])
     (if x
         (reduce-tree (tree x t))
         t))))

(define (solve2 ts)
  (for*/fold ([m 0])
             ([t1 (in-list ts)] [t2 (in-list ts)])
    (max m (magnitude (reduce-tree (tree t1 t2))))))

(println (solve1 input))

(println (solve2 input))
