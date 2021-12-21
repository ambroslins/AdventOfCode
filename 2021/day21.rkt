#lang racket

(require
  racket/file)

(struct player (space score) #:transparent)

(struct game (player-a player-b) #:transparent)

(define input
  (let ([start (for/list ([line (file->lines "inputs/21.txt")])
                 (string->number (string (last (string->list line)))))])
    (game (player (first start) 0)
          (player (second start) 0))))

(define example
  (game (player 4 0) (player 8 0)))

(define (move g n)
  (let* ([a (game-player-a g)]
         [space (+ 1 (remainder (+ (player-space a) n -1) 10))])
    (game (game-player-b g)
          (player space (+ (player-score a) space)))))

(define deterministic-dice
  (sequence->stream (in-cycle (in-inclusive-range 1 100))))

(define (stream-drop s n)
  (if (positive? n)
      (stream-drop (stream-rest s) (- n 1))
      s))

(define (stream-scan f i s)
  (if (stream-empty? s)
      (empty-stream)
      (stream-cons
       i
       (stream-scan f (f i (stream-first s)) (stream-rest s)))))

(define (dice->rolls dice)
  (stream-cons (stream-fold + 0 (stream-take dice 3)) (dice->rolls (stream-drop dice 3))))

(define (play start-game dice)
  (sequence->list
   (stop-before
    (stream-scan move start-game (dice->rolls dice))
    (lambda (g) (>= (player-score (game-player-b g)) 1000)))))

(define (solve1 start-game)
  (let ([games (play start-game deterministic-dice)])
    (* (player-score (game-player-b (last games)))
       (length games) 3 )))

(println (solve1 input))

(define dirac-dice-rolls
  (for*/fold ([h (hash)])
             ([i (in-inclusive-range 1 3)]
              [j (in-inclusive-range 1 3)]
              [k (in-inclusive-range 1 3)])
    (hash-update h (+ i j k) add1 0)))

(define (solve2 start-game)
  (define cache (make-hash '()))
  (define (iter g player-1?)
    (let ([cached (hash-ref cache (cons g player-1?) #f)])
      (or cached
          (if (>= (player-score (game-player-b g)) 21)
              (if player-1? 0 1)
              (let ([wins
                     (for/sum ([(roll n) (in-hash dirac-dice-rolls)])
                       (* n (iter (move g roll) (not player-1?))))])
                (hash-set! cache (cons g player-1?) wins)
                wins)))))
  (iter start-game #t))

(println (solve2 input))