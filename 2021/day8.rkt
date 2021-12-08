#lang racket

(require racket/file)
(require threading)

(define input
  (map (lambda~>>
        (string-split _ "|")
        (map string-split))
       (file->lines "inputs/8.txt")
       ))

(define solve1
  (lambda~>>
   (append-map second)
   (count (lambda (d) (member (string-length d) (list 2 4 3 7))))
   ))

(println (solve1 input))


(define digit-map
  (let ([ digits
          (list "abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg") ])
    (for/hash
        ([(d i) (in-indexed digits)])
      (values (list->set (string->list d))  i))))

(define segments
  (string->list "abcdefg"))

(define all-connections
  (for/list ([p (in-permutations segments)])
    (make-hash (map cons segments p))))

(define (find-connection patterns)
  (for/first
      ([mapping (in-list all-connections)]
       #:when (for/and ([digit patterns])
                (hash-has-key?
                 digit-map
                 (for/set ([c (in-string digit)])
                   (hash-ref mapping c)))))
    mapping))

(define (digits->decimal digits)
  (for/fold
   ([acc 0])
   ([d digits])
    (+ (* acc 10) d)))

(define (output-value patterns output)
  (let ([mapping (find-connection patterns)])
    (digits->decimal
     (for/list ([digit output])
       (hash-ref
        digit-map
        (for/set ([c (in-string digit)])
          (hash-ref mapping c)))))))

(define (solve2 entries)
  (for/sum ([entry entries])
    (output-value (first entry) (second entry))))

(println (solve2 input))