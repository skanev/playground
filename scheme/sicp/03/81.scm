; SICP exercise 3.81
;
; Exercise 3.6 discussed generalizing the random-number generator to allow one
; to reset the random-number sequence so as to produce repeatable sequences of
; "random" numbers. Produce a stream formulation of this same generator that
; operates on an input stream of requests to generate a new random number or
; to reset the sequence to a specified value and that produces the desired
; stream of random numbers. Don't use assignment in your solution.

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-take stream n)
  (if (= n 0)
      '()
      (cons (stream-car stream) (stream-take (stream-cdr stream) (- n 1)))))

(define random-init (current-milliseconds))

(define (rand-update number)
  (let ((modulus 4294967296)
        (multiplier 1664525)
        (increment 1013904223))
    (modulo (+ (* multiplier number) increment) modulus)))

(define (random-numbers requests)
  (define (next seed requests)
    (cond ((stream-null? requests) the-empty-stream)
          ((eq? (stream-car requests) 'generate)
           (let ((generated (rand-update seed))
                 (rest (stream-cdr requests)))
             (stream-cons generated (next generated rest))))
          ((eq? (stream-car requests) 'reset)
           (let ((new-seed (stream-car (stream-cdr requests)))
                 (rest (stream-cdr (stream-cdr requests))))
             (next new-seed rest)))))
  (next random-init requests))
