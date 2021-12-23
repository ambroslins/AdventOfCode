#lang racket

(struct burrow (halways rooms cost) #:transparent)

; #############
; #...........#
; ###B#C#B#D###
;   #D#C#B#A#
;   #D#B#A#C#
;   #A#D#C#A#
;   #########
(define example
  (burrow (make-list 7 #f)
          (list (list 1 3 3 0)
                (list 2 2 1 3)
                (list 1 1 0 2)
                (list 3 0 2 0))
          0))

; #############
; #...........#
; ###D#A#C#C###
;   #D#C#B#A#
;   #D#B#A#C#
;   #D#A#B#B#
;   #########
(define input
  (burrow (make-list 7 #f)
          (list (list 3 3 3 3)
                (list 0 2 1 0)
                (list 2 1 0 1)
                (list 2 0 2 1))
          0))

(define (burrow-done? b)
  (and (andmap (lambda (x) (false? x)) (burrow-halways b))
       (for*/and ([(r i) (in-indexed (burrow-rooms b))]
                  [x r])
         (= x i))))


(define (halway-moves b)
  (let ([halways (burrow-halways b)]
        [rooms (burrow-rooms b)])
    (filter
     identity
     (for/list ([(x i) (in-indexed halways)])
       (cond [(false? x) #f]
             [(>= (length (list-ref rooms x)) 4) #f]
             [(ormap (lambda (y) (not (= x y))) (list-ref rooms x)) #f]
             [(< (add1 x) i)
              (if (sequence-andmap (lambda (j) (false? (list-ref halways j))) (in-inclusive-range (+ x 2) (- i 1)))
                  (burrow
                   (list-set halways i #f)
                   (list-update rooms x (lambda (xs) (cons x xs)))
                   (+ (burrow-cost b)
                      (* (expt 10 x)
                         (+ (if (= i 6) -1 0)
                            (- 5 (length (list-ref rooms x)))
                            (* 2 (- i x 2))   ))))
                  #f)]
             [else
              (if (sequence-andmap (lambda (j) (false? (list-ref halways j))) (in-inclusive-range (+ i 1) (+ x 1)))
                  (burrow
                   (list-set halways i #f)
                   (list-update rooms x (lambda (xs) (cons x xs)))
                   (+ (burrow-cost b)
                      (* (expt 10 x)
                         (+ (if (= i 0) -1 0)
                            (- 5 (length (list-ref rooms x)))
                            (* 2 (- x i -1))   ))))
                  #f)])))))

(define (room-moves b)
  (define halways (burrow-halways b))
  (define rooms (burrow-rooms b))
  (define (left-moves i x xs)
    (for/list ([j (in-inclusive-range (+ i 1) 0 -1)]
               #:break (list-ref halways j))
      (burrow (list-set halways j x)
              (list-set rooms i xs)
              (+ (burrow-cost b)
                 (* (expt 10 x)
                    (+ (if (= j 0) -1 0)
                       (- 6 (length (list-ref rooms i)))
                       (* 2 (- i j -1))))))))
  (define (right-moves i x xs)
    (for/list ([j (in-inclusive-range (+ i 2) 6)]
               #:break (list-ref halways j))
      (burrow (list-set halways j x)
              (list-set rooms i xs)
              (+ (burrow-cost b)
                 (* (expt 10 x)
                    (+ (if (= j 6) -1 0)
                       (- 6 (length (list-ref rooms i)))
                       (* 2 (- j i 2))))))))
  (apply append
         (for/list ([(r i) (in-indexed rooms)])
           (if (andmap (lambda (x) (= x i)) r)
               '()
               (append
                (left-moves i (first r) (rest r))
                (right-moves i (first r) (rest r)))))))

(define (moves b)
  (if (>= (burrow-cost b) 48429)
      '()
      (append (halway-moves b) (room-moves b))))

(define (solutions b)
  (let ([ms (moves b)])
    (append (filter burrow-done? ms)
            (append-map solutions ms))))

(define (solve b)
  (if (burrow-done? b)
      (burrow-cost b)
      (apply min 48429 (map solve (moves b)))))

(println (solve input))
