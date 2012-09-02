; SICP exercise 3.55
;
; Define a procedure partial-sums that takes as argument a stream S and
; returns the stream whose elements are S₀, S₀ + S₁, S₀ + S₁ + S₂, ... . For
; example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (add-streams a b) (stream-map2 + a b))
(define (mul-streams a b) (stream-map2 * a b))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(define (partial-sums stream)
  (define result
    (stream-cons (stream-car stream)
                 (add-streams (stream-cdr stream) result)))
  result)

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))

(define (stream-take stream n)
  (if (= n 0)
      '()
      (cons (stream-car stream) (stream-take (stream-cdr stream) (- n 1)))))
