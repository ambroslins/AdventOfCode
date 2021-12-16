#lang racket

(require
  racket/file
  (prefix-in f: data/functor) data/applicative data/monad
  megaparsack megaparsack/text
  threading)

(define hex-map
  (for/hash ([c (in-string "0123456789ABCDEF")]
             [i (in-naturals 0)])
    (values c i)))

(define (hex->nibble c)
  (let ([n (hash-ref hex-map c)])
    (build-list 4 (lambda (i) (odd? (arithmetic-shift n (- i 3)))))))

(define (bit-list->integer lst)
  (for/fold ([n 0]) ([b (in-list lst)])
    (+ (* 2 n) (if b 1 0))))

(define input
  (~>> (file->string "inputs/16.txt")
       (string-trim)
       (string->list)
       (append-map hex->nibble)
       (map (lambda (b) (if b #\1 #\0)))
       (list->string)))

(define zero/p
  (do (char/p #\0) (pure #f)))

(define one/p
  (do (char/p #\1) (pure #t)))

(define bit/p
  (or/p zero/p one/p))

(define (bits/p n)
  (do [xs <- (repeat/p n bit/p)]
    (pure (bit-list->integer xs))))

(struct header (version type) #:transparent)

(define header/p
  (do [version <- (bits/p 3)]
    [type <- (bits/p 3)]
    (pure (header version type))))

(struct literal (value) #:transparent)

(define literal/p
  (do [xs <- (many/p (do one/p (repeat/p 4 bit/p)))]
    [x <- (do zero/p (repeat/p 4 bit/p))]
    (pure (literal (bit-list->integer (flatten (list xs x)))))))

(struct package (header content) #:transparent)

(define (parse! p s)
  (parse-result! (parse-string p s)))

(define package/p
  (do [h <- header/p]
    (if (= (header-type h) 4)
        (do [l <- literal/p] (pure (package h l)))
        (do [i <- bit/p]
          (if i
              (do [l <- (bits/p 11)]
                [ps <- (repeat/p l package/p)]
                (pure (package h ps)))
              (do [l <- (bits/p 15)]
                [s <- (f:map list->string (repeat/p l any-char/p))]
                (pure (package h (parse! (many+/p package/p) s)))))))))

(define transmission
  (parse! package/p input))

(define (sum-version-numbers p)
  (+ (header-version (package-header p))
     (if (list? (package-content p))
         (for/sum ([q (package-content p)]) (sum-version-numbers q))
         0)))

(println (sum-version-numbers transmission))

(define (proc n)
  (match n
    [0 +]
    [1 *]
    [2 min]
    [3 max]
    [5 >]
    [6 <]
    [7 =]))

(define (package-eval p)
  (let ([type-id (header-type (package-header p))]
        [content (package-content p)])
    (if (= 4 (header-type (package-header p)))
        (literal-value (package-content p))
        (let ([x (apply (proc type-id) (map package-eval content))])
          (if (boolean? x) (if x 1 0) x)))))

(println (package-eval transmission))