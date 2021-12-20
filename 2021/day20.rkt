#lang racket

(require
  racket/file)

(define input
  (let* ([lines (file->lines "inputs/20.txt")]
         [algorithm (for/vector ([c (in-string (first lines))]) (char=? c #\#))]
         [image (for*/set
                    ([(row y) (in-indexed (drop lines 2))]
                     [(c x) (in-indexed row)]
                     #:when (char=? c #\#))
                  (cons x y))])
    (cons algorithm image)))

(define (neighbours image pos)
  (for*/list ([dy (list -1 0 1)] [dx (list -1 0 1)])
    (set-member? image (cons (+ dx (car pos)) (+ dy (cdr pos))))))

(define (bits->integer bs)
  (for/fold ([acc 0]) ([b (in-list bs)])
    (+ (* acc 2) (if b 1 0))))

(define (positions-in-image image)
  (let* ([xs (set-map image car)]
         [ys (set-map image cdr)])
    (for*/stream
        ([x (in-inclusive-range (- (apply min xs) 1) (+ (apply max xs) 1))]
         [y (in-inclusive-range (- (apply min ys) 1) (+ (apply max ys) 1))])
      (cons x y))))


; because in the input algorithm the first (0 index) element is # and the last .
; after one enhancement there will be infinitely many # pixel
; therefor we build the negative enhanced image and only save the . pixels
(define (enhance proc algorithm image)
  (for/set ([pos (positions-in-image image)]
            #:unless (proc (vector-ref algorithm (bits->integer (map proc (neighbours image pos))))))
    pos))

(define (enhance-twice algorithm image)
  (enhance not algorithm (enhance identity algorithm image)))

(define (solve1 in)
  (set-count (enhance-twice (car in) (cdr in))))

(println (solve1 input))

(define (solve2 in)
  (set-count
   (for/fold ([image (cdr in)])
             ([_ (in-range 25)])
     (enhance-twice (car in) image))))

(println (solve2 input))
