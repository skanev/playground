; SICP exercise 3.54
;
; Define a procedure mul-streams, analogous to add-streams, that produces the
; elementwise product of its two input streams. Use this together with the
; stream of integers to complete the following definition of the stream whose
; nth elemen (counting down from 0) is n + 1 factorial:
;
;   (define factorials (cons-stream 1
;                                   (mul-streams <??>
;                                                <??>)))

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (add-streams a b) (stream-map2 + a b))
(define (mul-streams a b) (stream-map2 * a b))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))
(define factorials (stream-cons 1 (mul-streams factorials (stream-cdr integers))))

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
