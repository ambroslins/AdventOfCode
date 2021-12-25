#lang racket

(require
  racket/file)

(struct point (x y) #:transparent)

(struct herd (size east south) #:transparent)

(define input
  (for*/fold ([size (point 0 0)] [east (set)] [south (set)] #:result (herd size east south))
             ([(line y) (in-indexed (file->lines "inputs/25.txt"))]
              [(c x) (in-indexed (in-string line))])
    (let ([s (point (max (+ 1 x) (point-x size)) (max (+ 1 y) (point-y size)))])
      (cond
        [(char=? c #\>) (values s (set-add east (point x y)) south)]
        [(char=? c #\v) (values s east (set-add south (point x y)))]
        [else (values s east south)]))))

(define (point-east size p)
  (point (remainder (+ 1 (point-x p)) (point-x size)) (point-y p)))

(define (point-south size p)
  (point (point-x p) (remainder (+ 1 (point-y p)) (point-y size))))

(define step-east
  (match-lambda
    [(herd size east south)
     (herd
      size
      (for/set ([p (in-set east)])
        (let ([pe (point-east size p)])
          (if (or (set-member? east pe) (set-member? south pe)) p pe)))
      south )]))


(define step-south
  (match-lambda
    [(herd size east south)
     (herd
      size
      east
      (for/set ([p (in-set south)])
        (let ([ps (point-south size p)])
          (if (or (set-member? east ps) (set-member? south ps)) p ps))))]))

(define (step h)
  (step-south (step-east h)))

(define herd-stream
  (let iter ([h input])
    (stream-cons h (iter (step h)))))

(println
 (for/first
     ([(h1 i) (in-indexed herd-stream)]
      [h2 (stream-rest herd-stream)]
      #:when (and (equal? (herd-east h1) (herd-east h2)) (equal? (herd-south h1) (herd-south h2))))
   (+ 1 i)))
