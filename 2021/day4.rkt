#lang racket

(require racket/file)
(require racket/set)
(require data/monad data/applicative)
(require megaparsack megaparsack/text)

(struct bingo (numbers boards))

(define numbers/p
  (many/p integer/p #:sep (char/p #\,)))

; windows
(define newline/p
  (do (many/p (char/p #\return)) (char/p #\newline)))

(define space-char/p
  (char/p #\space))

(define board/p
  (many+/p
   (many+/p integer/p #:sep (many+/p space-char/p))
   #:sep (try/p (do newline/p (many/p space-char/p) (lookahead/p integer/p)))))

(define input/p
  (do [numbers <- numbers/p]
    (many+/p space/p)
    [boards <- (many/p
                board/p
                #:sep (try/p (do newline/p newline/p (many/p space-char/p))))]
    (pure (bingo numbers boards))))

(define input
  (parse-result!
   (parse-string
    input/p
    (file->string "inputs/4.txt"))))

(define (transpose xss)
  (apply map list xss))

(define (check-rows? numbers board)
  (for/or ([row board]) (for/and ([n row]) (set-member? numbers n))))

(define (check-columns? numbers board)
  (check-rows? numbers (transpose board)))

(struct result (marked-numbers last-number score))

(define (play-bingo numbers board)
  (let* ([marked-numbers
          (for/fold
           ([marked (set)])
           ([num numbers]
            #:break (or (check-rows? marked board) (check-columns? marked board)))
            (set-add marked num))]
         [last-number
          (for/last
              ([num numbers] #:when (set-member? marked-numbers num))
            num)]
         [score
          (for*/sum
              ([row board] [x row] #:unless (set-member? marked-numbers x))
            x)])
    (result (set-count marked-numbers) last-number score)))

(define results
  (for/list
      ([board (bingo-boards input)])
    (play-bingo (bingo-numbers input) board)))

(define (result-value res)
  (* (result-last-number res) (result-score res)))

(define solve1
  (result-value
   (argmin result-marked-numbers results)))

(println solve1)

(define solve2
  (result-value
   (argmax result-marked-numbers results)))

(println solve2)
