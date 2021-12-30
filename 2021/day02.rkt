#lang racket

(require racket/file)

(define input (map
               (lambda (line)
                 (let ([x (string-split line)])
                   (cons (first x) (string->number (second x)))))
               (file->lines "inputs/2.txt")))

(define test
  (list
   (cons "forward" 5)
   (cons "down" 5)
   (cons "forward" 8)
   (cons "up" 3)
   (cons "down" 8)
   (cons "forward" 2)
   ))

(define (move1 command pos)
  (let ([x (car pos)]
        [y (cdr pos)]
        [d (cdr command)])
    (case (car command)
      [("forward") (cons (+ x d) y)]
      [("down") (cons x (+ y d))]
      [("up") (cons x (- y d))]
      )))

(define (solve1 xs)
  (let ([pos (foldl move1 (cons 0 0) xs)])
    (* (car pos) (cdr pos))))

(println (solve1 input))

(define (move2 command state)
  (let* ([pos (car state)]
         [aim (cdr state)]
         [x (car pos)]
         [y (cdr pos)]
         [d (cdr command)])
    (case (car command)
      [("forward") (cons (cons (+ x d) (+ y (* d aim))) aim)]
      [("down") (cons (cons x y) (+ aim d))]
      [("up") (cons (cons x y) (- aim d))]
      )))

(define (solve2 xs)
  (let* ([state (foldl move2 (cons (cons 0 0) 0) xs)]
         [pos (car state)])
    (* (car pos) (cdr pos))))

(println (solve2 input))
